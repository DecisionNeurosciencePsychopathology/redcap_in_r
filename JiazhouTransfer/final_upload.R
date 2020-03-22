####B-Social Transfer post cleaning:

rootdir = "/Users/jiazhouchen/Box/skinner/data/Redcap\ Transfer/BSOCIAL\ transfer/datacleaning_FINAL"
dir.create(file.path(rootdir,"Problems"))
bsocial<-bsrc.checkdatabase2(protocol = ptcs$bsocial)
metadata<-bsocial$metadata
f_paths<-list.files(rootdir,full.names = T,include.dirs = F)

bs_idmap <- bsrc.getform(protocol = ptcs$bsocial,formname = "record_registration",online = T,batch_size = 1000L)[c("registration_redcapid","registration_soloffid")]
names(bs_idmap)<-c("bsocial_rc_id","soloff_id")



outcome<-lapply(f_paths,function(xa) {
  print(basename(xa))
  if(!grepl(".csv$",xa)){messsage("skip");return(NULL)}
  x_data<-read.csv(xa,stringsAsFactors = F)
  
  x_data$X<-NULL
  x_data_findid <- bsrc.findid(df = x_data,idmap = bs_idmap,id.var = "registration_redcapid")
  #Deal with existing people first
  x_data_sp <- split(x_data_findid,x_data_findid$ifexist)
  #write non-existing ones to folder to figure it out later 
  if(!is.null(x_data_sp$`FALSE`)){
    message("There is a problem with ID matching.")
    write.csv(x_data_sp$`FALSE`,file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_noidmatch.csv")),row.names = F)
  }
  
  
  df_togo<-x_data_sp$`TRUE`
  df_togo$registration_redcapid<-df_togo$bsocial_rc_id
  df_togo<-df_togo[which(!names(df_togo) %in% c("ogid","ifexist",names(bs_idmap)))]
  ID_pos<-match("registration_redcapid",names(df_togo))
  
  f_names<-unique(metadata$form_name[match(names(df_togo),metadata$field_name)])
  og_forms<-bsrc.getform(protocol = ptcs$bsocial,formname = f_names[f_names!="record_registration"],online = T,batch_size = 1000L,mod = T,at_least = 1)
  
  if(length(unique(og_forms$redcap_event_name))>1){message("multiple events uploading is not supported yet. Will terminate now.");return(NULL)}
  #Write out other problems:
  if(length(which(!names(df_togo) %in% names(og_forms)))>0){
    message("Failed to intake variable: ", paste(names(df_togo)[which(!names(df_togo) %in% names(og_forms))],collapse = "; "))
    write.csv(df_togo[c(ID_pos,which(!names(df_togo) %in% names(og_forms)))],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_nointake.csv")),row.names=F)
  }
  
  df_togo_b <- df_togo[which(names(df_togo) %in% names(og_forms))]
  if(any(table(df_togo_b$registration_redcapid)>1)){
    dup_id<-names(which(table(df_togo_b$registration_redcapid) > 1))
    mp_df<-df_togo_b[which(df_togo_b$registration_redcapid %in% names(which(table(df_togo_b$registration_redcapid) > 1))),]
    message(nrow(mp_df)," records and ",length(unique(mp_df$registration_redcapid))," IDs have duplicated form. Removed");
    write.csv(mp_df,file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_multiple_entry.csv")),row.names=F)
    
    df_togo_b <- df_togo_b[which(!df_togo_b$registration_redcapid %in% dup_id),]
  }
  return(NULL)
  df_tg_v<-bsrc.verify(df_new = df_togo_b,df_ref = og_forms,id.var = "registration_redcapid")
  
  #Write out both non-NAs 
  if(any(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW))){
    message("Value conflict identified")
    write.csv(x = df_tg_v$DIFF[which(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW)),],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_valueconflict.csv")),row.names = F)
  }
  
  df_toupload<-df_tg_v$DIFF[is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW),]
  if(nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
  df_toupload<-reshape2::dcast(df_toupload,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")
  df_toupload$redcap_event_name<-unique(og_forms$redcap_event_name)
  
  gx<-redcap_seq_uplaod(ds = df_toupload,id.var = "registration_redcapid",redcap_uri = ptcs$bsocial$redcap_uri,token = ptcs$bsocial$token)
  
  return(list(outcome=gx,og_data=og_forms))
})




#Protect Version:

#Import Baseline:
rootdir <- "~/Box/skinner/data/Redcap Transfer/PT transfer/Baseline2/"
rootdir <- "/Users/jiazhouchen/Box/skinner/data/Redcap\ Transfer/PT\ transfer/Baseline/additional_to_be_uploaded"
dir.create(file.path(rootdir,"Problems"),showWarnings = F,recursive = T)
protect<-bsrc.checkdatabase2(protocol = ptcs$protect,online = T)
masterdemo_reg <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L)
learn_idmap <- masterdemo_reg[c("registration_redcapid","registration_wpicid")]
names(learn_idmap)<-c("masterdemo_id","wpic_id")

ptc_map<-bsrc.checkbox(variablename = "registration_ptcstat",dfx = masterdemo_reg)
cons_p2 <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,"protect2"),any)
cons_legacy <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,c("protect","suicid2","suicide")),any)
p2_only_ID <- ptc_map$registration_redcapid[cons_p2 & !cons_legacy]


f_paths<-list.files(rootdir,full.names = T,include.dirs = F)


#Do non-p2 people first:
gx<-lapply(f_paths,upload_transfer,
           id.var = "registration_redcapid",
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           target_ptc = ptcs$protect,
           target_evt = "baseline_arm_1",
           ID_list = p2_only_ID,skip_check = T,exempt_code = c(999,99,""),
           toignore = TRUE)

#Do the p2 folks here:
gx<-lapply(f_paths,upload_transfer,
           id.var = "registration_redcapid",
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           target_ptc = ptcs$protect,
           target_evt = "baseline_arm_2",
           ID_list = p2_only_ID,skip_check = F,exempt_code = c(999,99,""),
           toignore = FALSE)


id.var = "registration_redcapid"
idmap = learn_idmap
idmap_id = "masterdemo_id"
metadata = protect$metadata
target_ptc = ptcs$protect
target_evt = "baseline_arm_2"
ID_list = p2_only_ID
skip_check = F
toignore = FALSE
exempt_code = c(999,99,"")
x_data = NULL
look_up = sp_rctogo


upload_transfer<-function(xpath,x_data=NULL,error_outdir=NULL,id.var=NULL,idmap=NULL,idmap_id=NULL,metadata=NULL,target_ptc=NULL,target_evt=NULL,ID_list=NULL,toignore=FALSE,skip_check=FALSE,exempt_code=NULL) {
  #Read in data;
  if(is.null(x_data)){
    print(basename(xpath))
    if(!grepl(".csv$",xpath)){message("skip");return(NULL)}
    x_data<-read.csv(xpath,stringsAsFactors = F)
  }
  #Clean up and use ID map for ID matching
  x_data$X<-NULL
  x_data_findid <- bsrc.findid(df = x_data,idmap = idmap,id.var = id.var)
  x_data_sp <- split(x_data_findid,x_data_findid$ifexist)
  #write non-existing ones to folder to figure it out later 
  if(!is.null(x_data_sp$`FALSE`)){
    message("There is a problem with ID matching.")
    write.csv(x_data_sp$`FALSE`,file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_noidmatch.csv")),row.names = F)
  } else {
    message("All ID matched!")
  }
  
  #Rename ID variables:
  df_togo<-x_data_sp$`TRUE`
  df_togo$registration_redcapid<-df_togo[[idmap_id]]
  df_togo<-df_togo[which(!names(df_togo) %in% c("ogid","ifexist",names(idmap)))]
  ID_pos<-match(id.var,names(df_togo))
  
  df_tg_sp<-split(df_togo,df_togo[[id.var]])
  do_multi<-any(sapply(df_tg_sp,nrow)>1)
  
  if(do_multi) {
    lapply(df_tg_sp,function(dfk){
      dfk$CDATE <- as.Date(dfk$CDATE)
      lk <- look_up[[as.character(unique(dfk$registration_redcapid))]]
      #Maybe we just stop getting the additional ones for now ?
      #Only do the direct match first. 
      dfk$EVT <- lk$EVT[match(dfk$CDATE,lk$CDATE)]
      lk$USED <- lk$EVT %in% dfk$EVT
      dfj<-dfk[is.na(dfk$EVT),]
      lk_search <- lk[!lk$USED,]
      if(nrow(dfj)<1){
        return(dfk)
      }
      for (x in 1:nrow(dfj)) {
        dt_to_search<-dfj$CDATE[x]
        
        
      }
      
        
    })
  }
  
  
  
  #Finding appropriate data to pull;
  vari_ref<-data.frame(variable_name = names(df_togo),stringsAsFactors = F)
  vari_ref$no_tag <- gsub("___.*$","",vari_ref$variable_name)
  vari_ref$form_name <- metadata$form_name[match(vari_ref$no_tag,metadata$field_name)]
  vari_ref_sp<-split(vari_ref,is.na(vari_ref$form_name))
  if(!is.null(vari_ref_sp$`TRUE`)){
    message("These variables are not included in the uploading because they have no match for any form on redcap. \n",
            paste(vari_ref_sp$`TRUE`$variable_name,collapse = ", "))
    write.csv(df_togo[c(id.var,vari_ref_sp$`TRUE`$variable_name)],
              file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_no_form_matched.csv")),row.names = F)
    
  } else {
    message("Every variables are accounted for!")
  }
  df_togo <- df_togo[which(names(df_togo) %in% vari_ref_sp$`FALSE`$variable_name)]
  
  if(toignore){
    df_togo<-df_togo[!df_togo$registration_redcapid %in% ID_list,]
  } else {
    df_togo<-df_togo[df_togo$registration_redcapid %in% ID_list,]
  }
  
  fnames <- as.character(na.omit(unique(vari_ref$form_name[vari_ref$variable_name != id.var])))
  
  og_forms <- bsrc.getform(protocol = target_ptc,formname = fnames,online = T,batch_size = 1000L,mod = T,at_least = 1,filter_events = target_evt)
  
  if(!is.null(og_forms) && nrow(og_forms)!=0) {
    message("Checking against existing data points...")
    if(length(unique(og_forms$redcap_event_name))>1){message("multiple events uploading is not supported yet. Will terminate now.");return(NULL)}
    
    # #Write out other problems:
    # if(length(which(!names(df_togo) %in% names(og_forms)))>0){
    #   message("Failed to intake variable: ", paste(names(df_togo)[which(!names(df_togo) %in% names(og_forms))],collapse = "; "))
    #   write.csv(df_togo[c(ID_pos,which(!names(df_togo) %in% names(og_forms)))],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_nointake.csv")),row.names=F)
    # }
    
    df_togo_b <- df_togo[,which(names(df_togo) %in% names(og_forms))]
    
    if(any(table(df_togo_b[[id.var]])>1)){
      dup_id<-names(which(table(df_togo_b[[id.var]]) > 1))
      mp_df<-df_togo_b[which(df_togo_b[[id.var]] %in% names(which(table(df_togo_b[[id.var]]) > 1))),]
      message(nrow(mp_df)," records and ",length(unique(mp_df[[id.var]]))," IDs have duplicated form. Removed");
      write.csv(mp_df,file = file.path(error_outdir,paste0(gsub(".csv","",basename(xa)),"_problem_multiple_entry.csv")),row.names=F)
      
      df_togo_b <- df_togo_b[which(!df_togo_b$registration_redcapid %in% dup_id),]
    } 
    if(nrow(df_togo)<1){message("nothing to check...ending");return(NULL)}
    df_tg_v<-bsrc.verify(df_new = df_togo_b,df_ref = og_forms,id.var = id.var,exempt_code=exempt_code)
    
    #Write out both non-NAs 
    if(any(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW))){
      message("Value conflict identified")
      write.csv(x = df_tg_v$DIFF[which(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW)),],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_valueconflict.csv")),row.names = F)
    }
    
    if(!skip_check){
      df_toupload<-df_tg_v$DIFF[is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW),]
      if(is.null(df_toupload) || nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
      df_toupload<-reshape2::dcast(df_toupload,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")
      df_toupload$redcap_event_name<-unique(og_forms$redcap_event_name)
    } else {
      df_toupload <- df_togo_b
      if(nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
    }
    print(unique(df_toupload$redcap_event_name))
    #return(df_toupload)
    gx<-redcap_seq_uplaod(ds = df_toupload,id.var = id.var,redcap_uri = target_ptc$redcap_uri,token = target_ptc$token)
  } else {
    if(nrow(df_togo)<1){message("nothing to upload");return(NULL)}
    message("No data found in destination, directly inject...")
    df_togo$redcap_event_name <- target_evt
    print(unique(df_togo$redcap_event_name))
    #return(df_togo)
    gx<-redcap_seq_uplaod(ds = df_togo,id.var = id.var,redcap_uri = target_ptc$redcap_uri,token = target_ptc$token)
  }
  
  return(list(outcome=gx,og_data=og_forms))
}


#Non-Baseline forms:

###First do the single entry ones;
rootdir <- "~/Box/skinner/data/Redcap Transfer/PT transfer/Baseline2/"
dir.create(file.path(rootdir,"Problems"),showWarnings = F,recursive = T)
protect<-bsrc.checkdatabase2(protocol = ptcs$protect)
masterdemo_reg <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L)
learn_idmap <- masterdemo_reg[c("registration_redcapid","registration_wpicid")]
names(learn_idmap)<-c("masterdemo_id","wpic_id")

ptc_map<-bsrc.checkbox(variablename = "registration_ptcstat",dfx = masterdemo_reg)
cons_p2 <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,"protect2"),any)
cons_legacy <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,c("protect","suicid2","suicide")),any)
p2_only_ID <- ptc_map$registration_redcapid[cons_p2 & !cons_legacy]


f_paths<-list.files(rootdir,full.names = T,include.dirs = F)

gx<-lapply(f_paths,upload_transfer,error_outdir=file.path(rootdir,"F_Problems"),
           id.var = "registration_redcapid",
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           target_ptc = ptcs$protect,
           target_evt = "baseline_arm_1",
           ID_list = p2_only_ID,skip_check = FALSE,exempt_code = c(999,99),
           toignore = TRUE)
# 
# for (xpath in f_paths) {
#   print(xpath)
#   dfx <- read.csv(xpath,stringsAsFactors = F)
#   print(table(table(dfx$registration_redcapid))[1] / nrow(dfx))
# }



protect_cur<-bsrc.checkdatabase2(protocol = ptcs$protect,online = T)


gxa<-protect_cur$data[which(protect_cur$data$registration_redcapid %in% p2_only_ID & grepl("_arm_1",protect_cur$data$redcap_event_name)),]
gxa[-c(1,2)] <- ""
redcap_seq_uplaod(ds = gxa,id.var = id.var,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)




##Updating dates to the legacy database:
lookuptable<-readxl::read_xlsx(file.path("~/Box/skinner/data/Redcap Transfer","redcap outputs","Subject Visits Table","S_CONTACTS.xlsx"))
lookup_melt<-reshape2::melt(lookuptable,id.var=c("ID","CDATE","MISSCODE", "RATER", "CONTACT TYPE", "OUTCOME", "MISSINGNESS", "LOCATION1", "LOCATION2", "COMMENT" ))
lookup_melt<-lookup_melt[which(!is.na(lookup_melt$value)),]
lookup_melt$CDATE <- as.Date(lookup_melt$CDATE)
sp_lookup <- split(lookup_melt,lookup_melt$ID)

ptc_toget <- c("SUICIDE","SUICIDE2","PROTECT")
protect_cur <- bsrc.checkdatabase2(protocol = ptcs$protect)
gMAPx<-bsrc.getEVTDATEFIELD(db = protect_cur)

sp_rctogo<-lapply(sp_lookup,function(dfx,same_evt_days = 10){
  dfx<-bsrc.findid(dfx,learn_idmap)
  dfy<-dfx[dfx$variable %in% ptc_toget,]
  if(nrow(dfy)<1) {return(NULL)}
  #return(NULL)
  dfy$value <- toupper(dfy$value)
  dfy$value[dfy$value=="INT"] <- "ADDA"
  dfy$ID<-dfy$masterdemo_id
  dfy<-dfy[order(as.Date(dfy$CDATE)),]
  dfy <- change_evt(dty = dfy,protocol_name = "NUM",arm_num = 1,evtvariname = "value")
  dfy$value[dfy$value == "ADDA"] <- paste0("ADDA",1:length(which(dfy$value=="ADDA")))
  dfy$TIC <- NA
  #Consolidate same visit type; get date duration
  if(nrow(dfy)>1) {
    dfy$unique_evt <- paste(dfy$variable,dfy$value,sep = "_")
    dfy <- do.call(rbind,lapply(split(dfy,dfy$unique_evt),function(dfgx){
      if(nrow(dfgx)==1){return(dfgx)}
      dfgy <-dfgx[which.min(as.Date(dfgx$CDATE)),]
      dfgy$TIC <- list(dfgx$CDATE - dfgy$CDATE)
      return(dfgy)
    }))
    rownames(dfy) <- NULL
    dfy <- dfy[order(as.Date(dfy$CDATE)),]
  }
  #Consolidate possible identical visits
  if(nrow(dfy)>1){
    dfy$CDATE_diff <- (dfy$CDATE - dfy$CDATE[c(NA,1:(nrow(dfy)-1))])
    dfy$CDATE_logi<- dfy$CDATE_diff < same_evt_days
    dfy$CDATE_logi[which(grepl("ADDA",dfy$EVT))] <- dfy$CDATE_diff[which(grepl("ADDA",dfy$EVT))] <= 3
    dfy$CDATE_logi[is.na(dfy$CDATE_logi) ] <- FALSE
    dfy$num <- 1:nrow(dfy)
    dfy$num <- dfy$num - cumsum(as.numeric(dfy$CDATE_logi))
    dfy<-do.call(rbind,lapply(split(dfy,dfy$num),function(dfxa){
      if(nrow(dfxa)==1) {
        return(dfxa)
      }
      if(any(grepl("ADDA",dfxa$EVT))){
        ind_logi <- min(which(grepl("ADDA",dfxa$EVT)))
      } else {
        ind_logi <- which.max(as.numeric(dfxa$EVT))
      }
      dfxb <- dfxa[ind_logi,]
      dfxb$TIC <-  list(dfxa$CDATE - dfxb$CDATE)
      return(dfxb)
    }))
  }
  dfy$value[dfy$EVT == "ADDA"] <- paste0("ADDA",1:length(which(dfy$EVT=="ADDA")))


  if(toupper(dfy$value[which.min(dfy$CDATE)]) == "B" || grepl("ADDA",toupper(dfy$value[which.min(dfy$CDATE)]))) {
    baseline_evt <- as.character(dfy$variable[which.min(dfy$CDATE)])
    #message("Identified first contact as ",as.character(dfy$variable[which.min(dfy$CDATE)])," baseline")
    if(baseline_evt!=ptc_toget[min(match(unique(dfx$variable),ptc_toget),na.rm = T)]) {
      print(unique(dfx$ID))
      print(as.character(unique(dfx$variable)))
      
      message("#######!!!!!!! MISALIGNED STUDY ORDER!!!!! ",baseline_evt," ",ptc_toget[min(match(unique(dfx$variable),ptc_toget),na.rm = T)],
              ". Don't know what to do yet, pass.")
      return(NULL)
    }
  } else {
    print(unique(dfx$ID))
    print(as.character(unique(dfx$variable)))
    
    message("#######!!!!!!! First contact identified as ",as.character(dfy$variable[which.min(dfy$CDATE)])," ",as.character(dfy$value[which.min(dfy$CDATE)]),
            ". Don't know what to do yet, pass.")
    return(NULL)
  }
  dfy$EVT_num <- suppressWarnings(as.numeric(dfy$EVT))
  add_yrs<-aggregate(EVT_num~variable,data = dfy,FUN = max)
  add_yrs$EVT_num<-cumsum(add_yrs$EVT_num) - as.numeric(add_yrs$EVT_num)
  add_yrs$EVT_num[add_yrs$EVT_num < 0.5] <- 0
  
  dfy$EVT_num <- dfy$EVT_num + add_yrs$EVT_num[match(dfy$variable,add_yrs$variable)]
  dfy$EVT_num[dfy$EVT=="ADDA"] <- "ADDA"
  dfy<-change_evt(dty = dfy,protocol_name = "NUM2PROTECT",arm_num = 1,evtvariname = "EVT_num")
  
  return(dfy[c("ID","CDATE","variable","value","EVT","TIC")])
  
  dfy$date_variable<-gMAPx$date_variname[match(dfy$EVT,gMAPx$unique_event_name)]
  dfz<-na.omit(dfy[c("ID","EVT","date_variable","CDATE")])
  dfz$TIC <- 0 
  
  
  if(length(unique(dfy$variable))==1) {
    dfy<-change_evt(dty = dfy,protocol_name = "PROTECT",arm_num = 1,evtvariname = "value")
  } else {
    dfy<-change_evt(dty = dfy,protocol_name = "NUM",arm_num = 1,evtvariname = "value")
  }
  
  
  if(any(duplicated(dfz$EVT))) {
    
    dfz_i<-do.call(rbind,lapply(split(dfz,dfz$EVT),function(xd){
      if(grepl("baseline",unique(xd$EVT))) {max_days = 45 } else {max_days = 8}
      if(as.numeric(max(xd$CDATE) - min(xd$CDATE)) < max_days){
        return(xd[which.min(xd$CDATE),])
      } else{
        return(NULL)
      }
    }))
    
    if(!any(!dfz$EVT %in% dfz_i$EVT)) {
      dfz <- dfz_i
    } else {
      message("######## duplicated EVT #########");
      return(NULL)
    }
    
  }
  
  if(nrow(dfz)<1){return(NULL)}
  
  names(dfz)<-c("registration_redcapid","redcap_event_name","date_variable","CDATE")
  dfz$CDATE<-as.character(dfz$CDATE)
  dfz_e<-reshape2::dcast(dfz, registration_redcapid + redcap_event_name ~ date_variable, value.var="CDATE")
  #redcap_seq_uplaod(ds = dfz_e,id.var = "registration_redcapid",redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)
  return(as.data.frame(dfz_e))
})

sp_rctogo<-sp_rctogo[!sapply(sp_rctogo,is.null)]
sp_rctogo[sapply(sp_rctogo,function(x){any(duplicated(x$EVT))})]





evt_map_df <- do.call(rbind,sp_rctogo)
rownames(evt_map_df) <- NULL

gx<-lapply(f_paths,upload_transfer,
           id.var = "registration_redcapid",
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           target_ptc = ptcs$protect,
           target_evt = "baseline_arm_2",
           ID_list = p2_only_ID,skip_check = F,exempt_code = c(999,99,""),
           toignore = FALSE)






































