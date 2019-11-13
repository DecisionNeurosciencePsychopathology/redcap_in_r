####Jiazhou's mergeing script
masterdemo_db <-bsrc.conredcap2(protocol = ptcs$masterdemo,batch_size = 1000L)
#curdb<-new.env()
#load("/Users/jiazhouchen/Box/skinner/data/RedCap Data/MasterDemo/Backup/conredcap.backup.rdata",envir = curdb)
masterdemo <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = F,batch_size = 1000L,curdb = curdb,mod = T)

bs_demo<-bsrc.getform(protocol = ptcs$bsocial,formname = "record_registration",online = T,batch_size = 1000L)
bs_demo$bs_group<-bs_demo$registration_group 
bs_demo$registration_group <- plyr::mapvalues(bs_demo$bs_group,from = c(1,2,3,4,88,89),to = c("HC","ATT","ATT","NON",88,89),warn_missing = T)
bs_demo$registration_lethality<-NA
bs_demo$registration_lethality[which(bs_demo$bs_group == "2")] <- "ll"
bs_demo$registration_lethality[which(bs_demo$bs_group == "3")] <- "hl"
bs_demo$bs_group <-NULL


bs_demo_matched<-bs_demo[names(bs_demo)[names(bs_demo) %in% names(masterdemo)]]

change_varinames<-list(terminate_yesno = "reg_term_yesno_bsocial",termination_date = "reg_termdate_bsocial",termination_reason = "reg_term_reason_bsocial",
     termination_reason_other="reg_term_reason_n_bsocial",termination_reason_why="reg_term_reason_why",registration_id = "registration_wpicid",
     registration_consentdate="reg_condate_bsocial",registration_status="reg_status_bsocial")

for (i in 1:length(change_varinames)){
  bs_demo_matched[[change_varinames[[i]]]] <-  bs_demo[[names(change_varinames)[i]]]
} 



bs_demo_sp<-split(bs_demo_matched,paste0("is_",as.character(bs_demo_matched$registration_redcapid) %in% as.character(masterdemo$registration_redcapid)))


bs_demo_v<-bsrc.verify(df_new = bs_demo_sp$is_TRUE,df_ref = masterdemo,id.var = "registration_redcapid")


grp_check<-bs_demo_v$DIFF[bs_demo_v$DIFF$variable == "registration_group",]
grp_check[!is.na(grp_check$REF) & !is.na(grp_check$NEW),]


bs_demo_dx<-bs_demo_v$DIFF[is.na(bs_demo_v$DIFF$REF),]

bs_demo_dcast<-reshape2::dcast(bs_demo_dx,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")



bs_demo_dcast$registration_zipcode[which(nchar(bs_demo_dcast$registration_zipcode)!=5)]<-paste0("0",bs_demo_dcast$registration_zipcode[which(nchar(bs_demo_dcast$registration_zipcode)!=5)])


gt<-redcap_upload(ds = bs_demo_dcast,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)





bs_demo_lower<-as.data.frame(apply(bs_demo_matched,2,tolower))
masterdemo_lower<-as.data.frame(apply(masterdemo,2,tolower))
bs_lower_sp<-split(bs_demo_lower,paste0("is_",as.character(bs_demo_lower$registration_redcapid) %in% as.character(masterdemo$registration_redcapid)))

bs_lower_v<-bsrc.verify(df_new = bs_lower_sp$is_TRUE,df_ref = masterdemo_lower,id.var = "registration_redcapid")
bs_tocheck<-bs_lower_v$DIFF[!is.na(bs_lower_v$DIFF$REF) & !is.na(bs_lower_v$DIFF$NEW),]

bs_tocheck$NEW<-gsub(" ","", bs_tocheck$NEW); bs_tocheck$REF<-gsub(" ","", bs_tocheck$REF)
bs_tocheck<-bs_tocheck[bs_tocheck$NEW!=bs_tocheck$REF,]
bs_tocheck$variable<-as.character(bs_tocheck$variable)

bs_tocheck_sp <- split(bs_tocheck,bs_tocheck$variable)

for(i in 3:4) {
  bs_tocheck_sp$registration_phonenum[,i]<-gsub("(","",gsub(")","",gsub("-","",gsub(".","",bs_tocheck_sp$registration_phonenum[,i],fixed = T),fixed = T),fixed = T),fixed = T)
}
bs_tocheck_sp$registration_phonenum <- bs_tocheck_sp$registration_phonenum[bs_tocheck_sp$registration_phonenum$NEW!=bs_tocheck_sp$registration_phonenum$REF,]


bsrc.transformGroup <- function(from = NULL, from.type=c("bsocial","protect","masterdemo"),to.type=NULL){
  
  
  
  
  
  
}

#Check
bsrc.missingessential<-function(masterdemo,vars=c()){
    
}






ema_prog<-do.call(rbind,ema$fulldata.ema$progress_data)

mean(ema_prog[ema_prog$daysinstudy == 21 & ema_prog$Type=="Total","porp"])



