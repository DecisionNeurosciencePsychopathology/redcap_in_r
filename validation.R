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



#Find Non-P2 people in P2 RedCap:
protect <- bsrc.checkdatabase2(protocol = ptcs$protect)
ID_evt_map<-protect$data[c(1,2)]
P2_rc_IDs<-unique(ID_evt_map$registration_redcapid[grepl("_arm_2",ID_evt_map$redcap_event_name)])

masterdemo_reg <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L)
ptc_map<-bsrc.checkbox(variablename = "registration_ptcstat",dfx = masterdemo_reg)
cons_p2 <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,"protect2"),any)
cons_legacy <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,c("protect","suicid2","suicide")),any)

P2_consented_ID <- ptc_map$registration_redcapid[cons_p2]

message("The following IDs are NOT consented for P2 but in P2 RedCap: ",paste(P2_rc_IDs[!P2_rc_IDs %in% P2_consented_ID],collapse = ", "))
message("The following IDs are CONSENTED for P2 but not in P2 RedCap: ",paste(P2_consented_ID[!P2_consented_ID %in% P2_rc_IDs],collapse = ", "))

###Check suicide attempt identical:
cur_sahx_msdm <- bsrc.getform(protocol = ptcs$masterdemo,formname = "suicide_history",online = T,batch_size = 1000L,mod = T,at_least = 1)

sahx_form<-cur_sahx_msdm
IDvar="registration_redcapid"
sahx_sp <- split(sahx_form,sahx_form[[IDvar]])






cur_sahx_msdm_lx_sp<-bsrc.proc_multientry(long_df = cur_sahx_msdm,index_df = bsrc.sahx_index(sahx_df =  sahx_form),IDvar = "registration_redcapid",at_least = 1)

unique_var <- c("sahx_sadate","sahx_lr")



sa_dup<-do.call(rbind,lapply(cur_sahx_msdm_lx_sp$list,function(dfa){
  if(any(duplicated(dfa[unique_var]))){
    return(dfa[which(duplicated(dfa[unique_var]) | duplicated(dfa[unique_var],fromLast = T)),])
  } else {return(NULL)}
}))
