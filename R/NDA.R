######THIS SCRIPT DEALS WITH NDA STUFF:

masterdemo<-bsrc.checkdatabase2(protocol = ptcs$masterdemo,online = T,forceskip = T,batch_size=1000L)




bsrc.nda.getidmap<-function(boxdir="",masterdemo=NULL,)


allIDs<-lapply(list.files("~/Box/skinner/administrative/NDA/IDMap/",full.names = T,recursive = F),readxl::read_xlsx)
ndaIDs<-do.call(rbind,allIDs)
existingID<-unique(masterdemo$data$registration_ndaguid)
ndaIDs[!ndaIDs$`Pseudo GUID` %in% existingID,]



bsrc.nda.assignIDs<-function(IDmap=NULL,masterdemoptcs=ptcs$masterdemo,ptcfilter="bsocial",beg_consentdate="2017-08-01",
                             rcIDvar="registration_redcapid",ndaIDvar="registration_ndaguid"){}


#do the filtering first
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

requiredDF<-ptcdf_b[c("registration_ndaguid","registration_redcapid","registration_gender")]
names(requiredDF)<-c("subjectkey","src_subject_id","gender")
#Gender:
requiredDF$gender<-sapply(requiredDF$gender,function(x){if(nchar(x)>1){strsplit(x,split = "")[[1]][1]}else{x}},simplify = T,USE.NAMES = F)






requiredMap$ElementName %in% c("subjectkey","src_subject_id","interview_date","interview_age","gender")



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


