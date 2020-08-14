#Master Demo Transfer and Validation:
masterdemo<-bsrc.conredcap2(protocol = ptcs$masterdemo,batch_size = 1000L,output = T)

########Get BSocial People:
bsocial<-bsrc.checkdatabase2(protocol = ptcs$bsocial,forceupdate = T)

subreg<-bsrc.getevent(eventname = "enrollment_arm_1",subreg = T,curdb = bsocial,)
subreg<-subreg[!is.na(as.numeric(subreg$registration_redcapid)),]
subreg[subreg==""]<-NA
subreg$registration_id[is.na(subreg$registration_id)]<-subreg$registration_redcapid[is.na(subreg$registration_id)]

masterdemo$metadata[masterdemo$metadata==""]<-NA

#We have to deal with the duplication problem;


bsrc.masterdemo.checkduplicate(infovars = c("registration_redcapid","registration_gender"),uniquevars = c("registration_initials","registration_lastfour","registration_dob"))

idmap<-masterdemo$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
idmap$alt_soloffid<-
#Let's deal with people who are NOT in the master demo:
if(any(duplicated(subreg$registration_redcapid))){stop("NOT POSSIBLE!")}
inMADM<-subreg[which(subreg$registration_redcapid %in% masterdemo$data$registration_redcapid),]
notinMADM<-subreg[which(!subreg$registration_redcapid %in% masterdemo$data$registration_redcapid),]









