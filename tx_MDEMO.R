#Masterdemo 
startup()

library(bsrc)

bsocial<-bsrc.checkdatabase2(ptcs$bsocial)
masterdemo<-bsrc.conredcap2(protocol = ptcs$masterdemo,batch_size = 1000L,output = T)
protect<-bsrc.checkdatabase2(ptcs$protect)

idmap<-masterdemo$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
names(idmap)<-c("masterdemoid","wpicid","soloffid")

reg_demo<-bsrc.getform(formname = "record_registration",curdb = bsocial)

reg_demo_matchid<-bsrc.findid(df = reg_demo,idmap = idmap,id.var = "registration_redcapid")

#Get the existing folks:
existingBSDEMO<-reg_demo_matchid[which(reg_demo_matchid$ifexist),]
if(any(existingBSDEMO$registration_redcapid != existingBSDEMO$masterdemoid)){stop("YO! Mismatch IDs!!!")}

notEXDEMO<-reg_demo_matchid[which(!reg_demo_matchid$ifexist),]
notEXDEMO<-bsrc.findid(df = notEXDEMO,idmap = idmap,id.var = "registration_soloffid")
notEXDEMO<-notEXDEMO[which((!is.na(as.numeric(notEXDEMO$registration_redcapid)))),]
notEXDEMO<-bsrc.change_grp_ptcs(input = notEXDEMO,origin = "bsocial","masterdemo")

notEXDEMO_rcmatch<-bsrc.findid(df = notEXDEMO,idmap = idmap,id.var = "registration_redcapid")

if(any(bsrc.findid(df = notEXDEMO,idmap = idmap,id.var = "registration_id")$ifexist)) {stop("CHECK WPIC ID")}
directtransvarinames<-masterdemo$metadata$field_name[which(masterdemo$metadata$field_name %in% gsub("___.*","",names(notEXDEMO_rcmatch)) )]

print("list of untransferables")
gsub("___.*","",names(notEXDEMO_rcmatch))[which(!gsub("___.*","",names(notEXDEMO_rcmatch)) %in% directtransvarinames)]


targ_a<-notEXDEMO_rcmatch[grep(paste(directtransvarinames,collapse = "|"),names(notEXDEMO_rcmatch))]
targ_b<-data.frame(registration_redcapid=notEXDEMO_rcmatch$registration_redcapid,
           registration_ptcstat___bsocial=1,reg_condate_bsocial= notEXDEMO_rcmatch$registration_consentdate,reg_status_bsocial=notEXDEMO_rcmatch$registration_status,
           reg_term_yesno_bsocial=notEXDEMO_rcmatch$terminate_yesno, reg_termdate_bsocial=notEXDEMO_rcmatch$termination_date,
           reg_term_reason_bsocial=notEXDEMO_rcmatch$termination_reason,
           reg_term_reason_why=notEXDEMO_rcmatch$termination_reason_why,
           reg_term_reason_n_bsocial=notEXDEMO_rcmatch$termination_reason_other)



#REDCapR::redcap_write(targ_a,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
REDCapR::redcap_write(targ_b,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)


bsrc.masterdemo.checkduplicate(infovars = c("registration_redcapid","registration_gender"),uniquevars = c("registration_initials","registration_dob"))











