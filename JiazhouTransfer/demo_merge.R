####Jiazhou's mergeing script

masterdemo <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L)

bs_demo<-bsrc.getform(protocol = ptcs$bsocial,formname = "record_registration",online = T,batch_size = 1000L)



bs_demo_matched<-bs_demo[names(bs_demo)[names(bs_demo) %in% names(masterdemo)]]

change_varinames<-list(terminate_yesno = "reg_term_yesno_bsocial",termination_date = "reg_termdate_bsocial",termination_reason = "reg_term_reason_bsocial",
     termination_reason_other="reg_term_reason_n_bsocial",termination_reason_why="reg_term_reason_why",registration_id = "registration_wpicid",
     registration_consentdate="reg_condate_bsocial")

for (i in 1:length(change_varinames)){
  bs_demo_matched[[change_varinames[[i]]]] <-  bs_demo[[names(change_varinames)[i]]]
} 

bs_demo_sp<-split(bs_demo_matched,paste0("is_",as.character(bs_demo_matched$registration_redcapid) %in% as.character(masterdemo$registration_redcapid)))


df_new <- bs_demo_sp$is_TRUE
df_rc_current <- masterdemo
id = "registration_redcapid"
id.new = NULL
id.rc = NULL

if(is.null(id.new) && is.null(id.rc))







