#Ksocial
ksocial <- bsrc.checkdatabase2(ptcs$ksocial,forceupdate = T)

regi <- bsrc.getform(curdb = ksocial,formname = "registration_and_termination")
K_cons_IDs <- as.numeric(regi$registration_redcapid); IDs <- IDs[!is.na(IDs)]

k_scanned <- bsrc.getform(curdb=ksocial,formname = "fmri_prescan_form",at_least = 1)
k_scan_ID <- k_scanned$registration_redcapid

b_scanned <- bsrc.getform(protocol = ptcs$bsocial,formname = "fmri_session_checklist")
kb_scan_ID <- b_scanned$registration_redcapid[b_scanned$registration_redcapid %in% K_cons_IDs]

kb_scanned_joinIDs <- unique(c(k_scan_ID,kb_scan_ID))

mdemo <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T)
mdemo <- bsrc.checkbox(variablename = "registration_race",dfx = mdemo)
vri_indx <- data.frame(num=c(1:5,999),txt = c("AmInd","Asian","Black","HawaPaci","White","Unwilling"),stringsAsFactors = F)
mdemo$registration_race <- sapply(mdemo$registration_race,function(x){ifelse(length(x)>1,"Mixed",vri_indx$txt[match(x,vri_indx$num)])})
m_RGE <- mdemo[c("registration_redcapid","registration_gender","registration_hispanic","registration_race")]


k_scanned_sub <- m_RGE[m_RGE$registration_redcapid %in% kb_scanned_joinIDs,]
addmargins(xtabs(formula = ~registration_gender+registration_race+registration_hispanic,data = k_scanned_sub))

k_total_sub <- m_RGE[m_RGE$registration_redcapid %in% K_cons_IDs,]
addmargins(xtabs(formula = ~registration_gender+registration_race+registration_hispanic,data = k_total_sub))


k_behav_c <- list.dirs(path = "~/Box/skinner/data/MRI/clock_ksocial/",full.names = F,recursive = F)
k_behav_t <- list.dirs(path = "~/Box/skinner/data/MRI/trust_ksocial/",full.names = F,recursive = F)
k_behav <- unique(c(k_behav_c,k_behav_t))

b_behav_c <- list.dirs(path = "~/Box/skinner/data/MRI/clock_bsocial/",full.names = F,recursive = F)
b_behav_t <- list.dirs(path = "~/Box/skinner/data/MRI/trust_bsocial/",full.names = F,recursive = F)
b_behav <- unique(c(b_behav_c,b_behav_t))

behav_both <- unique(c(k_behav,b_behav))
behav_both_dx <- bsrc.findid(idmap = mdemo[c("registration_redcapid","registration_wpicid")],df = data.frame(ID=behav_both,USELESS=TRUE,stringsAsFactors = F),id.var = "ID")$registration_redcapid
behav_kcon <- behav_both_dx[behav_both_dx %in% K_cons_IDs]

behav_kcon[!behav_kcon %in% kb_scanned_joinIDs]
kb_scanned_joinIDs[!kb_scanned_joinIDs %in% behav_kcon]
