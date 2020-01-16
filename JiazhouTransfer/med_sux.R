###Med list Transfer; P2 -> Masterdemo
proc.rc.medlist <- function(med_dfx=NULL,get_RxName=T) {
  med_dfx$redcap_event_name<-NULL
  med_dfx_melt<-reshape2::melt(med_dfx,id.var="registration_redcapid")
  med_dfx_melt$var_str_sp <- strsplit(as.character(med_dfx_melt$variable),"_")
  med_dfx_melt$Num<-sapply(med_dfx_melt$var_str_sp,`[[`,3)
  med_dfx_melt$Type<-sapply(med_dfx_melt$var_str_sp,`[[`,2)
  med_dfx_sp<-split(med_dfx_melt,paste(med_dfx_melt$registration_redcapid,med_dfx_melt$Num,sep = "_"))
  
  if(get_RxName){
    rx_ref<-data.frame(RxID = unique(med_dfx_melt[!is.na(med_dfx_melt$value) & med_dfx_melt$Type=="name","value"]),stringsAsFactors = F)
    message("Getting ",nrow(rx_ref)," records from Rx database. Might take some time")
    rx_ref$RxName <- sapply(rx_ref$RxID,function(RxNum){
      #message(RxNum)
      RxRead<-rx_allProperties(RxNum)$propConceptGroup$propConcept
      RxRead<-RxRead[sapply(RxRead,`[[`,"propCategory")=="NAMES"]
      if(length(RxRead)<1) {
        RxRead <- "Unknown, see comment"
      } else {
        RxRead<-RxRead[sapply(RxRead,`[[`,"propName")%in% c("RxNorm Name","Prescribable Synonym")][[1]][["propValue"]]
      }
      return(RxRead)
    })
  }
  
  
  
  med_dfx_rej<-do.call(rbind,lapply(med_dfx_sp,function(dfmedx){
    if(any(!is.na(dfmedx$value))){
      #Renaming the numeric value to string 
      if(get_RxName && !is.na(dfmedx$value[dfmedx$Type == "name"])) {
        
        if(!"spname" %in% dfmedx$Type ){
          
          dfmedx$var_str_sp[[which(dfmedx$Type=="name")]] <- gsub("^name$","spname",dfmedx$var_str_sp[[which(dfmedx$Type=="name")]])
          dfmedx$Type[dfmedx$Type=="name"]<-"spname"
          dfmedx$variable<-sapply(dfmedx$var_str_sp,paste,collapse="_")
          dfmedx$value[dfmedx$Type == "spname"]<-paste(dfmedx$value[dfmedx$Type == "spname"],rx_ref$RxName[match(dfmedx$value[dfmedx$Type == "spname"],rx_ref$RxID)],sep = ": ")
        } else {
          dfmedx$value[dfmedx$Type == "spname"]<-paste(dfmedx$value[dfmedx$Type == "name"],rx_ref$RxName[match(dfmedx$value[dfmedx$Type == "name"],rx_ref$RxID)],sep = ": ")
          dfmedx$value[dfmedx$Type == "name"] <-NA
        }
        
      }
      return(dfmedx)
    } else {
      return(NULL)
    }
    
  }))
  med_rej_sp<-split(med_dfx_rej,med_dfx_rej$registration_redcapid)
  return(med_rej_sp)
}


idmap <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L)[c("registration_redcapid","registration_wpicid","registration_soloffid")]
names(idmap)<-c("masterdemo_id","wpic_id","soloff_id")

protect_med<-bsrc.getform(protocol = ptcs$protect,formname = "ongoing_medication_list",aggressivecog = 0,online = T,mod = F,no_calc = F)
save(protect_med,file = "protect_med_backup.rdata")

bsocial_med<-bsrc.getform(protocol = ptcs$bsocial,formname = "medication_list",aggressivecog = 0,online = T,mod = T,no_calc = F)
save(bsocial_med,file = "bsocial_med_backup.rdata")

masterdemo_med<-bsrc.getform(protocol = ptcs$masterdemo,formname = "medication_list",aggressivecog = 0,online = T,batch_size = 1000L)


#General function 
med_df<-bsocial_med
med_df_sp <- split(med_df,med_df$redcap_event_name)

med_dfx<-med_df_sp$baseline_arm_1


med_dfx_sp<-proc.rc.medlist(med_dfx)
med_md_sp<-proc.rc.medlist(masterdemo_med,get_RxName=F)


med_dfx_spa<-do.call(rbind,lapply(med_dfx_sp,function(dk){
  if(unique(dk$registration_redcapid) %in% names(med_md_sp)) {
    max_cur<-max(as.numeric(med_md_sp[[unique(dk$registration_redcapid)]]$Num))
    dk$Num<-max_cur + as.numeric(dk$Num)
    for(ix in 1:nrow(dk)){
      dk$var_str_sp[[ix]][[3]]<-dk$Num[ix]
    }
    dk$variable<-sapply(dk$var_str_sp,paste,collapse="_")
  }
  return(dk)
}))

med_dfx_sp<-bsrc.findid(df = med_dfx_spa,idmap = idmap,id.var = "registration_redcapid")
message(paste(unique(med_dfx_sp$registration_redcapid[!med_dfx_sp$ifexist]),collapse=", ")," has no masterdemo record double check! Remove for now.")

med_dfx_sp$registration_redcapid<-med_dfx_sp$masterdemo_id
med_dfx_sp<-med_dfx_sp[med_dfx_sp$ifexist,]

med_rej_sp <-split(med_dfx_sp,med_dfx_sp$registration_redcapid)
gx<-lapply(med_rej_sp,function(dfRej){
  df_dcast<-reshape2::dcast(data = dfRej[which(!is.na(dfRej$value)),],formula = registration_redcapid ~ variable, drop = T, value.var = "value")
  d<-redcap_upload(ds = df_dcast,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token,retry_whenfailed = F)
  d$success
})

bsocial_incompete<-c(221456,221790,221872)

################################
###############SUI HX###########
################################
bsrc.procmeta <- function(metadata = NULL){
  cb_indx<-which(metadata$field_type %in% c("checkbox"))
  all_varinames<-metadata$field_name[-cb_indx]
  gx<-bsrc.getchoicemapping(variablenames = metadata$field_name[cb_indx],metadata = metadata)
  checkbox_vars<-sapply(names(gx),function(ax){paste(ax,gx[[ax]]$choice.code,sep = "___")})
  return(list(metadata=metadata,checkbox_vars=checkbox_vars))
}



###The base will be b-social attempt history:
msdm_db<-bsrc.checkdatabase2(protocol = ptcs$masterdemo)
p_meta<-bsrc.procmeta(metadata = msdm_db$metadata)
sahx_bs <- bsrc.getform(protocol = ptcs$bsocial,formname = "suicide_history",online = T,batch_size = 1000L,mod = T,at_least = 2) #Online version is used to ensure MOST up-to-date data
sahx_bs<-bsrc.findid(df = sahx_bs,idmap = idmap,id.var = "registration_redcapid")
message(paste(unique(sahx_bs$registration_redcapid[!sahx_bs$ifexist]),collapse=", ")," has no masterdemo record double check! Remove for now.")
sahx_bs$registration_redcapid<-sahx_bs$masterdemo_id
sahx_bs<-sahx_bs[sahx_bs$ifexist,which(!names(sahx_bs) %in% c("redcap_event_name","masterdemo_id","wpic_id","soloff_id","ogid","ifexist"))]
sahx_backup<-bsrc.conredcap2(protocol = ptcs$masterdemo,batch_size = 1000L)
if(FALSE){
  sahx_bs_v<-bsrc.verify(df_new = sahx_bs,df_ref = msdm_db$data,id.var = "registration_redcapid")
  bs_demo_dcast<-reshape2::dcast(bs_demo_dx,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")
  redcap_upload(ds_to_write = sahx_bs,batch_size = 100L,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
} else {
  message("First time didn't have to deal with this; but if later....use the protect method!")
}
#Make a copy of bsocial
file.copy(from = ptcs$bsocial$rdpath,
          to = file.path(dirname(ptcs$bsocial$rdpath),"Operations","bsocial_backup_111919_beforeRemovingSUX.rdata"),overwrite = F)
#######
##Now intergrate protect:
sahx_pt <- bsrc.getform(protocol = ptcs$protect,formname = "ongoing_suicide_hx_lethality",online = T,batch_size = 1000L,mod = T,at_least = 1)
msdm_db <- bsrc.checkdatabase2(protocol = ptcs$masterdemo,online = T,batch_size=1000L)
sahx_pt<-bsrc.findid(df = sahx_pt,idmap = idmap,id.var = "registration_redcapid")
###Checks 
message(paste(unique(sahx_pt$registration_redcapid[!sahx_pt$ifexist]),collapse=", ")," has no masterdemo record double check! Remove for now.")
message(paste(unique(sahx_pt$registration_redcapid[which(sahx_pt$registration_redcapid != sahx_pt$masterdemo_id)]),
              collapse=", ")," has different id to masterdemo record double check! Remove for now.")

sahx_pt$registration_redcapid<-sahx_pt$masterdemo_id
sahx_pt<-sahx_pt[sahx_pt$ifexist,which(!names(sahx_pt) %in% c("redcap_event_name","masterdemo_id","wpic_id","soloff_id","ogid","ifexist","sahx_mlanum_r","sahx_mranum_r"))]

cur_sahx_msdm <- bsrc.getform(protocol = ptcs$masterdemo,formname = "suicide_history",online = T,batch_size = 1000L,mod = T,at_least = 1)
sahx_pt_sp <-split(sahx_pt,ifelse(sahx_pt$registration_redcapid %in% cur_sahx_msdm$registration_redcapid,"exists","new"))
#Again, first deal with people who are had no history:
#Make sure they are truely, nothing new here:
sahx_pt_v<-bsrc.verify(df_new = sahx_pt_sp$new,df_ref = msdm_db$data,id.var = "registration_redcapid")

if(any(!sahx_pt_v$DIFF$REF %in% c(0,"",NA))){message("HELL NO IT'S GONNA OVERWIRTE DATA WHEN IT'S NOT SUPPOSED TO!!!!!! STOP IT!!!!!!!")}else {
  redcap_upload(ds_to_write = sahx_pt_sp$new,batch_size = 100L,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
}


redcap_upload(ds_to_write = sahx_pt_v$DIFF[which(sahx_pt_v$DIFF$REF %in% c(0,"",NA) & !sahx_pt_v$DIFF$NEW %in% c(0,"",NA)),],batch_size = 100L,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)


sahx_pt_v<-bsrc.verify(df_new = sahx_pt_sp$exists,df_ref = msdm_db$data,id.var = "registration_redcapid")




######Protect transfer:
####This is the suicide histroy########
##Get the existing folks, exclude them for now;
sahx_baseline<-read.csv("/Users/jiazhouchen/Box/skinner/data/Redcap Transfer/All protect data/suicide history/S_SQUEST.csv",stringsAsFactors = F)
sahx_varimap<-na.omit(unpack(readxl::read_xlsx(file.path("/Users/jiazhouchen/Box/skinner/data/Redcap Transfer","redcap outputs","To be transferred","SP","SUICIDE HISTORY_cv.xlsx")))$map)
sahx_varimap$date_field_yn<-grepl("date",sahx_varimap$RC_name)

sahx_dfx<-sahx_baseline[names(sahx_baseline) %in% sahx_varimap$OG_name]
names(sahx_dfx)<-sahx_varimap$RC_name[match(names(sahx_dfx),sahx_varimap$OG_name)]
sahx_dfx<-bsrc.findid(sahx_dfx,idmap = idmap)
sahx_dfx$ID<-sahx_dfx$masterdemo_id
if(any(!sahx_dfx$ifexist)){warning("Dataframe has IDs that are not matched.")}
sahx_dfx[c(names(idmap),"ogid","ifexist")]<-NULL

#For now only transfer P2 folks:
sahx_dfz<-reshape2::melt(sahx_dfx,id.vars=c("ID","sahx_attemptnum"))
sahx_dfz$type<-sapply(strsplit(as.character(sahx_dfz$variable),split = "-x-"),`[[`,1)
sahx_dfz$num<-sapply(strsplit(as.character(sahx_dfz$variable),split = "-x-"),`[[`,2)
#Recast
sahx_dfz$variable<-NULL;
sahx_dfr<-reshape(data = sahx_dfz,timevar = "type",idvar = c("ID","num","sahx_attemptnum"),direction = "wide",sep = "-x-")
names(sahx_dfr)<-gsub("value-x-","",names(sahx_dfr))


sahx_dfr$sahx_sadate_at<-as.character(as.Date(sahx_dfr$sahx_sadate_at,format = "%m/%d/%Y"))

sp_sahx<-split(sahx_dfr,sahx_dfr$ID)





find_sahx_duplicate_single <- function(grz,skipnumcheck=F) {
  grz[grz==""]<-NA
  success<-TRUE 
  if(!skipnumcheck && is.na(unique(grz$sahx_attemptnum))){
    return(list(o_df=grz,status="NO DATA"))
  }
  #Clean up 
  if(skipnumcheck) {grz$sahx_attemptnum <- NA}
  fix_v <- c("ID","sahx_attemptnum")
  grz_fix<-unique(grz[fix_v])
  message(grz_fix$ID)
  gra <- grz[which(!names(grz) %in% fix_v)]
  gra <- gra[which(!duplicated(gra[names(gra) != "num"])),]
  
  if(grz_fix$sahx_attemptnum <1 && !skipnumcheck){
    return(list(o_df=grz[1,],status="ZERO ATTEMPT"))
  }
  gra<-gra[which(apply(gra[names(gra) != "num"],1,function(x){any(!is.na(x))})),]
  
  #If it's already pre-fixed, pass through
  if(nrow(gra)==as.numeric(grz_fix$sahx_attemptnum) && !skipnumcheck) {
    gr_final <- cbind(grz_fix,gra)
    return(list(o_df=gr_final,status="ATTEMPT NUM MATCHED"))
  }
  na_date_num <- gra$num[which(is.na(gra$sahx_sadate_at))]
  gra$sahx_sadate_at[which(is.na(gra$sahx_sadate_at))] <- "FAKEDATE"
  gra_dsp <- split(gra,gra$sahx_sadate_at)
  if(length(gra_dsp) == as.numeric(grz_fix$sahx_attemptnum) || skipnumcheck){
    grc_raw<-lapply(gra_dsp,function(x){
      if(x$sahx_sadate_at=="FAKEDATE"){x$sahx_sadate_at<-NA}
      x$sahx_lr_at[is.na(x$sahx_lr_at)]<-x$sahx_lr_at[match(paste(x[is.na(x$sahx_lr_at),c("sahx_describe_at","sahx_sadate_at")],collapse = "|X|"),
                                                            paste(x[!is.na(x$sahx_lr_at),c("sahx_describe_at","sahx_sadate_at")],collapse = "|X|"))]
      x$sahx_describe_at[is.na(x$sahx_describe_at)]<-x$sahx_lr_at[match(paste(x[is.na(x$sahx_describe_at),c("sahx_lr_at","sahx_sadate_at")],collapse = "|X|"),
                                                            paste(x[!is.na(x$sahx_describe_at),c("sahx_lr_at","sahx_sadate_at")],collapse = "|X|"))]
      
      x <- unique(x)
      if(nrow(x)==1){
        return(list(df=x,matched=T))
      } else if (nrow(unique(x[c("sahx_sadate_at","sahx_describe_at")]))==1 && length(na.omit(unique(x$sahx_lr_at)))) {
        return(list(df=x[which(!is.na(x$sahx_lr_at))[1],],matched=T))
      } else {
        return(list(df=x,matched=F))
      }
    })
   
    matched <-sapply(grc_raw, `[[`,"matched")
    grc <- do.call(rbind,lapply(grc_raw[which(matched)],`[[`,"df"))
    rownames(grc)<-NULL
    if(length(which(matched))==as.numeric(grz_fix$sahx_attemptnum) && !skipnumcheck){
      return(list(o_df=cbind(grz_fix,grc),status="DUPLICATE DETECTED AND SOLVED"))
    } else if (length(which(!matched)) ==1 && length(na.omit(unique(grc_raw[[which(!matched)]]$df$sahx_lr_at))) == 1) {
      gre_a <- grc_raw[[which(!matched)]]$df
      gre <- gre_a[1,]
      gre$sahx_describe_at <- paste(unique(gre_a$sahx_describe_at),collapse = " / ")
      if(is.na(gre$sahx_lr_at) && any(!is.na(gre_a$sahx_lr_at))){gre$sahx_lr_at <- as.numeric(na.omit(unique(gre_a$sahx_lr_at)))}
      grf <- rbind(grc,gre)
      return(list(o_df=cbind(grz_fix,grf),status="IMPERFECT MATCH, CHECK REQUIRED"))
    } else if (skipnumcheck) {
      return(list(o_df=cbind(grz_fix,gra),status="SKIP ATT NUM CHECK"))
    } else {return(list(o_df=cbind(grz_fix,gra),status="NO DATA:UNABLE TO MATCH"))}
  } else {return(list(o_df=cbind(grz_fix,gra),status="NO DATA:UNIQUE DATES NUM IS DIFF THAN ATT NUM"))}
  return(list(o_df=grz,status="NO DATA:HELP!SHOULDN'T HAPPEN"))
}

sp_sahx_proc<-cleanuplist(lapply(sp_sahx,find_sahx_duplicate_single))
report_df<-data.frame(ID=names(sp_sahx_proc),Status=as.character(sapply(sp_sahx_proc,`[[`,"status",USE.NAMES = F)),stringsAsFactors = F)
left_overIDs<-report_df$ID[which(grepl("NO DATA",report_df$Status))]
carryonIDs<-report_df$ID[which(!grepl("NO DATA",report_df$Status))]
left_over_ddf <- do.call(rbind,lapply(sp_sahx_proc[left_overIDs],`[[`,"o_df"))
left_over_fdf <- merge(left_over_ddf,report_df,by = "ID",all.x = T)
write.csv(left_over_fdf,file = "/Users/jiazhouchen/Box/skinner/data/Redcap Transfer/All protect data/suicide history/Problems/baseline_sahx_status.csv",row.names = F)


sahx_dfx_clean<-do.call(rbind,lapply(sp_sahx_proc[carryonIDs],function(x){
  if(is.null(x$o_df)) {return(NULL)} 
  xr<-x$o_df
  xr$num<-NA
  if(unique(xr$sahx_attemptnum)!=0){
    xr$num<-1:nrow(xr)
  }
  return(xr)
}))

sahx_dfx_clean$sahx_lr_at[which(as.numeric(sahx_dfx_clean$sahx_lr_at)>8)]<-NA

sahx_gx_sp <- split(sahx_dfx_clean,sahx_dfx_clean$sahx_attemptnum>0)

cur_sahx_msdm <- bsrc.getform(protocol = ptcs$masterdemo,formname = "suicide_history",online = T,batch_size = 1000L,mod = T,at_least = 1)
save(cur_sahx_msdm,file = file.path("/Users/jiazhouchen/Box/skinner/data/Redcap Transfer/All protect data/suicide history/pre_upload_sahx.rdata"))

noSA_dfx<-sahx_gx_sp$`FALSE`
noSA_dfx$registration_redcapid<-noSA_dfx$ID; noSA_dfx$ID<-NULL
noSA_dfx<-noSA_dfx[c("registration_redcapid","sahx_attemptnum")]
noSA_dfx<-noSA_dfx[!noSA_dfx$registration_redcapid %in% cur_sahx_msdm$registration_redcapid,]
noSA_verify<-bsrc.verify(df_new = noSA_dfx,df_ref = cur_sahx_msdm,id.var = "registration_redcapid")
REDCapR::redcap_write(noSA_verify$NEW_INFO,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)


SA_clean_df <- clean_str(sahx_gx_sp$`TRUE`)$clean_df
wide_SAdfx<-reshape( SA_clean_df,idvar = c("ID","sahx_attemptnum"),timevar = "num",direction = "wide",sep = "")
wide_SAdfx$registration_redcapid<-wide_SAdfx$ID; wide_SAdfx$ID<-NULL
SA_verify<-bsrc.verify(df_new = wide_SAdfx,df_ref = cur_sahx_msdm,id.var = "registration_redcapid")
SA_togo <- SA_verify$NEW_INFO[!SA_verify$NEW_INFO$registration_redcapid %in% cur_sahx_msdm$registration_redcapid,]

# ##purging attempt histroyies #retry
#SA_togo[-1]<-""

redcap_seq_uplaod(ds = SA_togo,id.var = id.var,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)

SA_togo_existing <- SA_clean_df[SA_clean_df$ID %in% cur_sahx_msdm$registration_redcapid,]
SA_togo_sp <- split(SA_togo_existing,SA_togo_existing$ID)
cur_sahx_msdm_lx_sp<-bsrc.proc_multientry(long_df = cur_sahx_msdm,index_df = bsrc.sahx_index(sahx_df =  cur_sahx_msdm),IDvar = "registration_redcapid",at_least = 1)

SA_EX_try<-cleanuplist(lapply(SA_togo_sp,function(dfx){
  cur_df<-cur_sahx_msdm_lx_sp$list[[as.character(unique(dfx$ID))]]
  if(is.null(cur_df)){
    return(list(out_df=dfx,status=TRUE,ogf=dfx))
  }
  names(cur_df)<-paste(names(cur_df),"at",sep = "_")
  cur_df$sahx_attemptnum<-NA
  cur_df$ID <- cur_df$registration_redcapid_at;cur_df$num <- cur_df$index_num_at
  px_cur_df <- cur_df[names(dfx)]; px_cur_df$num <- paste0("rc_",px_cur_df$num)
  otc<-find_sahx_duplicate_single(rbind(px_cur_df,dfx),skipnumcheck = T)
  statusX <- !any(duplicated(otc$o_df))
  statusY <- otc$status; check_ensure<-FALSE
  if(statusX) {
    ota <- otc$o_df
    ota$sahx_attemptnum <- nrow(ota)
    if(length(which(is.na(match(px_cur_df$num,ota$num))))>0) {message("yo");statusY<-"DOUBLE CHECK: RC entry Replace";check_ensure <- TRUE}
    ota <- ota[which(!grepl("rc_",ota$num)),]
    if(nrow(ota)<1){message("NO DATA");return(NULL)}
    ota$num <- nrow(cur_df) + (1:nrow(ota))
    return(list(out_df=ota,status=statusY,ogf=rbind(dfx,px_cur_df)))
  }  else {
    message("DUPLICATE DETECHED. WILL REMOVE AND CALL FOR ATTENTION.")
    return(NULL)
  }
}))
SA_EX_try_df <- do.call(rbind,lapply(SA_EX_try,`[[`,"out_df"))
wide_SAEXdfx<-reshape( SA_EX_try_df,idvar = c("ID","sahx_attemptnum"),timevar = "num",direction = "wide",sep = "")
wide_SAEXdfx$registration_redcapid<-wide_SAEXdfx$ID; wide_SAEXdfx$ID<-NULL
bsrc.verify(df_new = wide_SAEXdfx,df_ref = cur_sahx_msdm,id.var = "registration_redcapid")$VALUE_CONFLICT
message("Make sure no other value than the attempter number is modified.")
redcap_seq_uplaod(ds = wide_SAEXdfx,id.var = id.var,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)








