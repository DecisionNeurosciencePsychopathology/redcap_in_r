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
redcap_upload(ds_to_write = sahx_bs,batch_size = 100L,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
#Make a copy of bsocial
file.copy(from = ptcs$bsocial$rdpath,
          to = file.path(dirname(ptcs$bsocial$rdpath),"Operations","bsocial_backup_111919_beforeRemovingSUX.rdata"),overwrite = F)
##Now intergrate protect:
sahx_pt <- bsrc.getform(protocol = ptcs$protect,formname = "ongoing_suicide_hx_lethality",online = T,batch_size = 1000L,mod = T,at_least = 1)
sahx_pt<-bsrc.findid(df = sahx_pt,idmap = idmap,id.var = "registration_redcapid")
###Checks 
message(paste(unique(sahx_pt$registration_redcapid[!sahx_pt$ifexist]),collapse=", ")," has no masterdemo record double check! Remove for now.")
message(paste(unique(sahx_pt$registration_redcapid[which(sahx_pt$registration_redcapid != sahx_pt$masterdemo_id)]),
              collapse=", ")," has different id to masterdemo record double check! Remove for now.")

sahx_pt$registration_redcapid<-sahx_pt$masterdemo_id
sahx_pt<-sahx_pt[sahx_pt$ifexist,which(!names(sahx_pt) %in% c("redcap_event_name","masterdemo_id","wpic_id","soloff_id","ogid","ifexist"))]

cur_sahx_msdm <- bsrc.getform(protocol = ptcs$masterdemo,formname = "suicide_history",online = T,batch_size = 1000L,mod = T,at_least = 1)




sahx_bs_v<-bsrc.verify(df_new = sahx_bs,df_ref = msdm_db$data,id.var = "registration_redcapid")






