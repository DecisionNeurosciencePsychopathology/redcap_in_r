
setwd("~/Documents/redcap_in_r/Data meeting and cleaning/")
source('~/Documents/github/UPMC/startup.R')
pt<-bsrc.checkdatabase2(protocol = ptcs$protect)
bs<-bsrc.checkdatabase2(protocol = ptcs$bsocial)

library(tidyr)
BSnotTerm<-unique(subset(md$data,registration_ptcstat___bsocial==1&(reg_term_yesno_bsocial==0|is.na(reg_term_yesno_bsocial)))$registration_redcapid) # all bsocial people 
for (fmname in c("scid","suicide_questions","sidpiv","scid_consensus")){
  #Checks for Protect BL transfer: 1 person should not have more than 1 SCID (repeat steps for Suicide Questions, SIDP, and SCID consensus)
  form<-bsrc.getform(protocol = ptcs$protect,formname = fmname,curdb = pt) # everyone who did the form 
  dupid<-form$registration_redcapid[which(duplicated(form$registration_redcapid))]
  if(length(dupid)>0){formdupid<-subset(form,registration_redcapid %in% dupid)
  write.csv(formdupid,paste0("~/Documents/github/UPMC/TRANSFER/checks/multiple_",fmname,".csv"))}
  #Also, each person should have 1 SCID, SIDP, Suicide Questions and SCID consensus in either P2 or P1 (unless they were terminated d/t ineligibility). Generate list of who does not (out of those who are eligible) and we will investigate why.
  form<-bsrc.findid(form,id.var="registration_redcapid",idmap = idmap)
  checkdf<-data.frame(form[c("registration_redcapid","redcap_event_name")],value=1,row=1:nrow(form),stringsAsFactors = F)
  checkdf<-tidyr::pivot_wider(checkdf,id_cols = "registration_redcapid", names_from = "redcap_event_name",values_from = "value")
  
  print(BSnotTerm[which(!BSnotTerm %in% form$masterdemoid)]) #people who don't have scid )
  print(any(rowSums(checkdf[-1],na.rm = T)==0)) #FALSE
  print(dupid)
  print(checkdf$registration_redcapid[which(rowSums(checkdf[-1],na.rm = T)==2)]) # all TRUE
}
#Only people who are NOT consented in any protocol before P2 has data in P2





 

formsuique<-bsrc.getform(protocol = ptcs$protect,formname = "suicide_questions",curdb = pt)
formsidpiv<-bsrc.getform(protocol = ptcs$protect,formname = "sidpiv",curdb = pt)
formscidcon<-bsrc.getform(protocol = ptcs$protect,formname = "scid_consensus",curdb = pt)
any(is.na(formsidpiv$registration_redcapid))
any(duplicated(formscidcon$registration_redcapid))
