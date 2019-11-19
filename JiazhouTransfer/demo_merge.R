####Jiazhou's mergeing script
################B-Social

masterdemo_db <-bsrc.attachngrab(protocol = ptcs$masterdemo)
#curdb<-new.env()
#load("/Users/jiazhouchen/Box/skinner/data/RedCap Data/MasterDemo/Backup/conredcap.backup.rdata",envir = curdb)
masterdemo <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L,curdb = curdb,mod = T,at_least = 0)

idmap<-masterdemo[c("registration_redcapid","registration_wpicid","registration_soloffid")]
names(idmap)<-c("masterdemo_id","wpic_id","soloff_id")

bs_demo<-bsrc.getform(protocol = ptcs$bsocial,formname = "record_registration",online = F,curdb = curdb,batch_size = 1000L)
bs_demo$registration_ptcstat___bsocial <-1
bs_demo$bs_group<-bs_demo$registration_group 
bs_demo$registration_group <- plyr::mapvalues(bs_demo$bs_group,from = c(1,2,3,4,88,89),to = c("HC","ATT","ATT","NON",88,89),warn_missing = T)
bs_demo$registration_lethality<-NA
bs_demo$registration_lethality[which(bs_demo$bs_group == "2")] <- "ll"
bs_demo$registration_lethality[which(bs_demo$bs_group == "3")] <- "hl"
bs_demo$bs_group <-NULL

bs_demo<-bsrc.findid(df = bs_demo,idmap = idmap,id.var = "registration_redcapid")
bs_demo$registration_redcapid<-bs_demo$masterdemo_id

bs_demo_matched<-bs_demo[names(bs_demo)[names(bs_demo) %in% names(masterdemo)]]
change_varinames<-list(terminate_yesno = "reg_term_yesno_bsocial",termination_date = "reg_termdate_bsocial",termination_reason = "reg_term_reason_bsocial",
     termination_reason_other="reg_term_reason_n_bsocial",termination_reason_why="reg_term_reason_why",registration_id = "registration_wpicid",
     registration_consentdate="reg_condate_bsocial",registration_status="reg_status_bsocial",registration_pin="registration_lastfour")

for (i in 1:length(change_varinames)){
  bs_demo_matched[[change_varinames[[i]]]] <-  bs_demo[[names(change_varinames)[i]]]
} 

message("The following are not transferred: \n",
        paste(names(bs_demo)[!names(bs_demo) %in% names(masterdemo) & !names(bs_demo) %in% names(change_varinames)],collapse = " ")) 
bs_demo_matched$ogID <-bs_demo$ogid
bs_demo_sp<-split(bs_demo_matched,paste0("is_",bs_demo$ifexist))
bs_demo_sp$is_TRUE$ogID<-NULL

bs_demo_v<-bsrc.verify(df_new = bs_demo_sp$is_TRUE,df_ref = masterdemo,id.var = "registration_redcapid")


grp_check<-bs_demo_v$DIFF[bs_demo_v$DIFF$variable == "registration_group",]
grp_check[!is.na(grp_check$REF) & !is.na(grp_check$NEW),]


bs_demo_dx<-bs_demo_v$DIFF[is.na(bs_demo_v$DIFF$REF),]

bs_demo_dcast<-reshape2::dcast(bs_demo_dx,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")

bs_demo_dcast$registration_zipcode[which(nchar(bs_demo_dcast$registration_zipcode)!=5)]<-paste0("0",bs_demo_dcast$registration_zipcode[which(nchar(bs_demo_dcast$registration_zipcode)!=5)])
bs_demo_dcast$registration_lastfour[which(nchar(bs_demo_dcast$registration_lastfour)!=4)] <- paste0("0",bs_demo_dcast$registration_lastfour[which(nchar(bs_demo_dcast$registration_lastfour)!=4)])


gt<-redcap_seq_uplaod(ds = bs_demo_dcast,id.var="registration_redcapid",redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)

################K-Social

masterdemo <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L,curdb = curdb,mod = T,at_least = 0)
ks_demo<-bsrc.getform(protocol = ptcs$ksocial,online = T,formname = "registration_and_termination",batch_size = 1000L,at_least = 0)

ks_demo$registration_ptcstat___ksocial <-1
ks_demo$pt_group<- ks_demo$registration_group
ks_demo$registration_group <- plyr::mapvalues(ks_demo$pt_group,from = c(1,2,4,6,7,88,89),to = c("HC","DEP","IDE","ATT","ATT",88,89),warn_missing = T)
ks_demo$registration_lethality<-NA
ks_demo$registration_lethality[which(bs_demo$bs_group == "6")] <- "ll"
ks_demo$registration_lethality[which(bs_demo$bs_group == "7")] <- "hl"
ks_demo$pt_group <-NULL

ks_demo<-bsrc.findid(df = ks_demo,idmap = idmap,id.var = "registration_redcapid")
ks_demo<-ks_demo[ks_demo$ifexist,]
ks_demo$registration_redcapid<-ks_demo$masterdemo_id
ks_demo$termination_reason <- unlist(bsrc.checkbox(variablename = "termination_reason",dfx = ks_demo,returndf = F,cleandf = T)$Checkbox_text,use.names = F)

ks_demo_matched<-ks_demo[names(ks_demo)[names(ks_demo) %in% names(masterdemo)]]
change_varinames<-list(termination_yesno = "reg_term_yesno_ksocial",termination_date = "reg_termdate_ksocial",termination_reason = "reg_term_reason_ksocial",termination_reason_n = "reg_term_reason_n_ksocial",
                       termination_reason_other="reg_term_reason_n_ksocial",termination_reason_why="reg_term_reason_why",registration_id = "registration_wpicid",
                       registration_consentdate="reg_condate_ksocial",registration_status="reg_status_ksocial",registration_pin="registration_lastfour")

for (i in 1:length(change_varinames)){
  ks_demo_matched[[change_varinames[[i]]]] <-  ks_demo[[names(change_varinames)[i]]]
} 

message("The following are not transferred: \n",
        paste(names(ks_demo)[!names(ks_demo) %in% names(masterdemo) & !names(ks_demo) %in% names(change_varinames)],collapse = " ")) 



ks_demo_v<-bsrc.verify(df_new = ks_demo_matched,df_ref = masterdemo,id.var = "registration_redcapid")


grp_check<-ks_demo_v$DIFF[ks_demo_v$DIFF$variable == "registration_group",]
grp_check[!is.na(grp_check$REF) & !is.na(grp_check$NEW),]


ks_demo_dx<-ks_demo_v$DIFF[is.na(ks_demo_v$DIFF$REF),]

ks_demo_dcast<-reshape2::dcast(ks_demo_dx,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")

ks_demo_dcast$registration_zipcode[which(nchar(ks_demo_dcast$registration_zipcode)!=5)]<-paste0("0",ks_demo_dcast$registration_zipcode[which(nchar(ks_demo_dcast$registration_zipcode)!=5)])
ks_demo_dcast$registration_lastfour[which(nchar(ks_demo_dcast$registration_lastfour)!=4)] <- paste0("0",ks_demo_dcast$registration_lastfour[which(nchar(ks_demo_dcast$registration_lastfour)!=4)])


gt<-redcap_seq_uplaod(ds = ks_demo_dcast,id.var="registration_redcapid",redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)


ks_tocheck<-ks_demo_v$DIFF[!is.na(ks_demo_v$DIFF$REF) & !is.na(ks_demo_v$DIFF$NEW),]




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


####################################
#Get missing info from B-Social? 
bs_lg_demo<-read.csv("/Users/jiazhouchen/Box/skinner/projects_analyses/Project\ BPD\ Longitudinal/BPD\ Database/JC/DEMO.csv",stringsAsFactors = F)
bs_lg_demo$X<-NULL
bs_lg_demo<-bsrc.findid(df = bs_lg_demo,idmap = idmap,id.var = "ID")
bs_lgk_demo<-bs_lg_demo[which(bs_lg_demo$masterdemo_id %in% lack_info_sp$bsocial$registration_redcapid),]
bs_lgk_demo<-na.omit(bs_lgk_demo[c("masterdemo_id","BDAY","SEX")])
bs_lgk_demo$SEX <- ifelse(bs_lgk_demo$SEX==1,2,1)
names(bs_lgk_demo) <- c("registration_redcapid","registration_dob","registration_birthsex")

bslgk_demo_v<-bsrc.verify(df_new = bs_lgk_demo,df_ref = masterdemo,id.var = "registration_redcapid")
bslgk_demo_dx<-bslgk_demo_v$DIFF[is.na(bslgk_demo_v$DIFF$REF),]
bslgk_demo_dcast<-reshape2::dcast(bslgk_demo_dx,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")
redcap_seq_uplaod(ds = bslgk_demo_dcast,id.var="registration_redcapid",redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)

####Change birthsex:
masterdemo <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L,curdb = curdb,mod = T,at_least = 0)
masterdemo$sab<-NA
masterdemo$sab[is.na(masterdemo$registration_birthsex)]<-substr(masterdemo$registration_gender[is.na(masterdemo$registration_birthsex)],1,1)
masterdemo$sab[masterdemo$sab=="F"]<-1
masterdemo$sab[masterdemo$sab=="M"]<-2
masterdemo$sab[masterdemo$sab=="O"]<-NA
masterdemo$registration_birthsex[is.na(masterdemo$registration_birthsex)] <- masterdemo$sab[is.na(masterdemo$registration_birthsex)]

redcap_upload(masterdemo[which(!is.na(masterdemo$registration_birthsex)),c("registration_redcapid","registration_birthsex")],redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)


######Masterdemo check:
#########Essential checks:
masterdemo <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L,curdb = curdb,mod = T,at_least = -1)
masterdemo$registration_ptcstat <- unlist(bsrc.checkbox(variablename = "registration_ptcstat",dfx = masterdemo,returndf = F,cleandf = T)$Checkbox_list,use.names = F)
IDvar="registration_redcapid"
essential_vars<-c("registration_group","registration_dob","registration_initials","registration_birthsex")
bsrc.masterdemo.checkduplicate(protocol = ptcs$masterdemo,infovars = c(IDvar,"registration_birthsex","registration_initials","registration_soloffid"),uniquevars = c("registration_dob"))
essen_df<-masterdemo[c(IDvar,essential_vars,"registration_ptcstat")]
essen_df <- rc_na_remove(essen_df,mod = T,IDvar = "registration_redcapid",at_least = -1)


for (vx in c(essential_vars,"registration_ptcstat")) {
  message("variable: ",vx,", has ",length(which(is.na(essen_df[[vx]])))," number of missing values.")
}

lack_info<-essen_df[which(rowSums(is.na(essen_df))!=0),]
lack_info_sp <- split(lack_info,lack_info$registration_ptcstat)

#Check variables 
for (vx in names(masterdemo)) {
  if(length(which(is.na(masterdemo[[vx]])))>1){
  message("variable: ",vx,", has ",length(which(is.na(masterdemo[[vx]])))," number of missing values.")
  }
}





















