######THIS SCRIPT DEALS WITH NDA STUFF:
if(F){
masterdemo<-bsrc.checkdatabase2(protocol = ptcs$masterdemo,online = T,forceskip = T,batch_size=1000L)




bsrc.nda.getidmap<-function(boxdir="",masterdemo=NULL,)


allIDs<-lapply(list.files("~/Box/skinner/administrative/NDA/IDMap/",full.names = T,recursive = F),readxl::read_xlsx)
ndaIDs<-do.call(rbind,allIDs)
existingID<-unique(masterdemo$data$registration_ndaguid)
IDmap<-ndaIDs[!ndaIDs$`Pseudo GUID` %in% existingID,]
names(IDmap)<-c("GUID_map_ID","NDA_GUID","Date")


bsrc.nda.assignIDs<-function(IDmap=NULL,masterdemoptcs=ptcs$masterdemo,ptcfilter="bsocial",beg_consentdate="2017-08-01",
                             rcIDvar="registration_redcapid",ndaIDvar="registration_ndaguid"){}


#do the filtering first
masterdemoptcs<-ptcs$masterdemo
ptcfilter = "bsocial"
beg_consentdate = "2019-08-01"
rcIDvar="registration_redcapid"
ndaIDvar="registration_ndaguid"


ptcdf_a<-masterdemo$data[which(masterdemo$data[[paste("registration_ptcstat",ptcfilter,sep = "___")]]==1),]

ptcdf_b<-ptcdf_a[which(as.Date(ptcdf_a[[paste("reg_condate",ptcfilter,sep = "_")]]) > as.Date(beg_consentdate)),]

ptcdf_c<-ptcdf_b[which(is.na(ptcdf_b$registration_ndaguid) | ptcdf_b$registration_ndaguid==""),]

#do filter on IDmap,

#Assign ID;
ptcdf_c$registration_ndaguid<-IDmap$NDA_GUID[1:nrow(ptcdf_c)]
rcUpload<-ptcdf_c[c(rcIDvar,ndaIDvar)]
REDCapR::redcap_write_oneshot(rcUpload,redcap_uri = masterdemoptcs$redcap_uri,token = masterdemoptcs$token)



InstruMap<-read.csv(file.choose(),stringsAsFactors = F)
requiredMap<-InstruMap[which(InstruMap$Required=="Required"),]

requiredDF<-ptcdf_b[c("registration_ndaguid","registration_redcapid","registration_gender","registration_dob")]
names(requiredDF)<-c("subjectkey","src_subject_id","gender","dob")
#Gender:
requiredDF$gender<-sapply(requiredDF$gender,function(x){if(nchar(x)>1){strsplit(x,split = "")[[1]][1]}else{x}},simplify = T,USE.NAMES = F)

#Get ctq first
referencedf<-data.frame(evt=c("baseline_arm_1","catchup_17_renewal_arm_1"),variname=c("demo_visitdate","registration_catchupdate"),stringsAsFactors = F)

bsocial<-bsrc.checkdatabase2(protocol = ptcs$bsocial)
ref_a<-bsocial$data[c(rcIDvar,"redcap_event_name",referencedf$variname)];ref_a[ref_a==""]<-NA

ref_b<-reshape2::melt(ref_a,id.vars=c(rcIDvar,"redcap_event_name"))

ref_b<-ref_b[!is.na(ref_b$value),]
ref_b$redcap_event_name<-referencedf$evt[match(ref_b$variable,referencedf$variname)]
ref_ba<-ref_b[ref_b$redcap_event_name=="baseline_arm_1",]
#requiredDF$baseline_date<-
dat_a <- bsrc.getform(curdb = bsocial,formname = "ctq")


dat_a$interview_date<-ref_b$value[match(interaction(dat_a$registration_redcapid,dat_a$redcap_event_name),interaction(ref_b$registration_redcapid,ref_b$redcap_event_name))]


ContinMap<-InstruMap[which(InstruMap$Required!="Required"),]
ContinMap$Aliases_List<-strsplit(ContinMap$Aliases,split = ",")
names(dat_a)[which(!names(dat_a) %in% c(rcIDvar,"redcap_event_name","interview_date"))]<-sapply(names(dat_a)[which(!names(dat_a) %in% c(rcIDvar,"redcap_event_name","interview_date"))],function(xn){
  if(xn %in% ContinMap$ElementName){return(xn)}
  while (i < nrow(ContinMap)) {
    #print(i)
    if(xn %in% ContinMap$Aliases_List[[i]]){return(ContinMap$ElementName[i])}
    i <- i+1
  }
  return(NA)
})

dat_b<-dat_a[which(!is.na(names(dat_a)))]
dat_b[dat_b==999] <- -99
dat_merge <- merge(dat_b,requiredDF,by.x = rcIDvar,by.y = "src_subject_id",all.y = T)
dat_merge$registration_redcapid<-NULL;dat_merge$redcap_event_name<-NULL
write.csv(dat_merge,file = "ctq_out.csv",row.names = F)






}



#EVT_DATEFIELD
# 
# CSSIR<-read.csv(file.choose())
# IDs<-c(220989,
# 222121,
# 207989,
# 215232,
# 217630,
# 222165,
# 218219,
# 220363,
# 221672,
# 220426,
# 210685,
# 219809,
# 210079)
# 
# filteredCSSIR<-CSSIR[which(CSSIR$registration_redcapid %in% IDs),]
# 
# 


