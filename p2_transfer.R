startup()
###Migration Scripts:
boolFalse<-F
while(boolFalse==F)
{
  tryCatch({
    a<-matrix(NA,ncol=1,nrow=sample(1:5,1))
    a[sample(1:6,1),1]<-10;
    boolFalse<-T
  },error=function(e){
  },finally={})
}

unpack<-function(dtx_r){
  gx<-list(
  map=data.frame(OG_name=names(dtx_r),RC_name=as.character(dtx_r[1,]),stringsAsFactors = F),
  data=dtx_r[-1,]
  )
  gx$map[gx$map=="NA" | gx$map=="NaN"]<-NA
  return(gx)
}

getevt<-function(ID=NULL,CDATE=NULL,PROTONAME=NULL,sp_lookup=NULL){
  idxz<-sp_lookup[[ID]]
  rtx<-idxz[which.min(abs(as.Date(idxz$CDATE) - as.Date(CDATE))),c(PROTONAME,"CDATE")]
  names(rtx)<-c("EVT","OG_CDATE")
  rtx$DIFFDAY<-abs(as.Date(rtx$OG_CDATE) - as.Date(CDATE) )
  return(rtx)
}

#Local boxdir:
rootdir = "~/Box/skinner/data/Redcap Transfer/redcap outputs"

#DEMO;
#######
#In this trunk we deal with demo transfer:
DEMO_trans<-unpack(readxl::read_xlsx(file.path(rootdir,"Demo (Master Demographic Form).xlsx")))
names(DEMO_trans$data)<-gsub("registration_race_","registration_race___",names(DEMO_trans$data))
DEMO_trans$data$registration_wpicid<-DEMO_trans$data$registration_dnplid
DEMO_trans$data[as.numeric(DEMO_trans$data$registration_dnplid)>440000,1]<-gsub("88","43",unlist(DEMO_trans$data[as.numeric(DEMO_trans$data$registration_dnplid)>440000,1]))
DEMO_trans$data$ssn_Full<-NULL
DEMO_trans$data$registration_lastfour<-DEMO_trans$data$ssn_last4
DEMO_trans$data$ssn_last4<-NULL
# DEMO_trans$data$registration_redcapid<-DEMO_trans$data$registration_dnplid
# DEMO_trans$data$registration_dnplid<-NULL
REDCapR::redcap_write(DEMO_trans$data,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
#REDCapR::redcap_write(masterdemo$data,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
#######

p2evtmap<-redcap.eventmapping(redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)$data
#Do look-up table:
lookuptable<-readxl::read_xlsx(file.path(rootdir,"Subject Visits Table","S_CONTACTS.xlsx"))
sp_lookup<-split(lookuptable,lookuptable$ID)
#Do the self-reports cuz they are ezzzzzz
SRpath = file.path(rootdir,"To be transferred","SRs")
list.files(SRpath)[9]
protocol_name<-"PROTECT2"
ID_fieldname<-"registration_redcapid"
arm_num<-2
misscodeallowed<-c("1")

dtx_r = readxl::read_xlsx(list.files(SRpath,full.names = T)[9])
maps<-dtx_r[1,]
dtx<-dtx_r[-1,]

dtx_rp<-unpack(dtx_r)
dtx_i<-cbind(dtx_rp$data,do.call(rbind,lapply(1:nrow(dtx),function(x){getevt(dtx$ID[x],dtx$CDATE[x],protocol_name,sp_lookup)})))
#Let's think about what to do with people with duplicated ID EVT

#Right now we don't do anything with that 
duplicated(dtx_i[c("ID","EVT")])
dtx_i$EVT[grepl("adda",tolower(dtx_i$EVT))]<-paste0(dtx_i$EVT[grepl("adda",tolower(dtx_i$EVT))],1)
dtx_ii<-dtx_i

dtls<-list(
  excluded = dtx_ii[which(!dtx_ii$MISSCODE %in% misscodeallowed | is.na(dtx_ii$EVT)),],
  transfer = dtx_ii[which(dtx_ii$MISSCODE %in% misscodeallowed & !is.na(dtx_ii$EVT)),]
)



proc_transfer<-function(dty,map,upload=T) {
  #Change event name to match redcap
  dty$EVT[grepl("mo",tolower(dty$EVT))]<-paste("month",gsub("mo","",tolower(dty$EVT)[grepl("mo",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
  dty$EVT[grepl("yr",tolower(dty$EVT))]<-paste("year",gsub("yr","",tolower(dty$EVT)[grepl("yr",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
  dty$EVT[grepl("adda",tolower(dty$EVT))]<-paste("additional",gsub("adda","",tolower(dty$EVT)[grepl("adda",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
  dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("baseline_arm",arm_num,sep="_")
  
  idmap<-REDCapR::redcap_read(batch_size = 1000,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token,fields = c("registration_redcapid","registration_wpicid"))$data
  
  dty_i<-bsrc.findid(dty,idmap,id.var = "ID")
  
  dty_rc<-dty_i[c(ID_fieldname,"EVT")]
  names(dty_rc)<-c(ID_fieldname,"redcap_event_name")
      
  dty_dt<-dty_i[map$OG_name[!is.na(map$RC_name)]]
  names(dty_dt)<-map$RC_name[match(names(dty_dt),map$OG_name)]
  
  dty_combo<-cbind(dty_rc,dty_dt)
  dty_combo$ID<-NULL
  if(upload){
    REDCapR::redcap_write(dty_combo,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)
  }
  return(dty_combo)
}



#Change EVT
#Putting things aside;
masterdemo_backup<-bsrc.checkdatabase2(protocol = ptcs$masterdemo)















