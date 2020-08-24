## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
source('~/Documents/github/UPMC/startup.R')
############startuptransfer#############
rootdir="~/Box/skinner/data/Redcap Transfer/PT transfer/final_0103_baseline/"
oridir="~/Box/skinner/data/Redcap Transfer/All protect data/"
#allsub<-read.csv(paste0(rootdir,"ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt2.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
rcform<-unique(var_map$Form_name)

for (i in 1:length(rcform)){
  rcfm<-rcform[i]
  vm<-subset(var_map,Form_name==rcfm)
  acform <- gsub(".csv","",unique(subset(var_map,Form_name==rcfm)$path))
  final_fm_list<-lapply(acform, function(acform){read.csv(paste0(rootdir,"form_",acform,"_2020-01-03.csv"), stringsAsFactors = F)});names(final_fm_list)<-acform # grab forms 
  ori_fm<-read.csv(paste0(oridir,acform[1],".csv"),stringsAsFactors = F)
  ori_fm$CDATE<-lubridate::mdy(ori_fm$CDATE)
  iddate<-Reduce(function(x,y){dplyr::full_join(x,y,by=c("registration_redcapid","CDATE"))},lapply(final_fm_list,function(x){x[c("registration_redcapid","CDATE")]}))
  colnames(iddate)<-c("ID","CDATE")
  iddate$CDATE<-as.Date(iddate$CDATE)
  iddate<-dplyr::right_join(iddate,ori_fm[c("ID","CDATE","MISSCODE")],by=c("ID","CDATE"))#get misscode based on the main form 
  colnames(iddate)<-plyr::mapvalues(colnames(iddate),from = vm$access_var,to = vm$redcap_var,warn_missing = F)
  colnames(iddate)[2]<-"CDATE"
  assign(paste0(strsplit(rcfm," ")[[1]],collapse = "_"),iddate)
}
write.csv(Study_Startup_form_demo,file="~/Documents/github/UPMC/TRANSFER/PT/misscodes.csv")


#check negoutcome SPECIAL
rcfm<-rcform[3]
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt.csv',stringsAsFactors = FALSE) 
var_map<-subset(var_map,Form_name==rcfm)
acform <- gsub(".csv","",unique(subset(var_map,Form_name==rcfm)$path))
log_replace<-read.csv("~/Documents/github/UPMC/TRANSFER/PT/log_replace2020-01-06.csv",stringsAsFactors = F)
log_replace<-log_replace[which(log_replace$which_form%in%acform),]
ori_fm_list<-lapply(acform,function(acfm){
  x<-read.csv(paste0(oridir,acfm,".csv"),stringsAsFactors = F)
  acvar<-plyr::mapvalues(unique(log_replace$var_name),from = var_map$redcap_var,to = var_map$access_var)
  x<-x[,c(1,2,3,which(colnames(x)%in%acvar))]
  x[which(x=="",arr.ind = T)]<-NA
  x["CDATE"]<-lubridate::mdy(x[["CDATE"]])
  x<-data.frame(x,IDDATE=paste0(x[["ID"]],x[["CDATE"]]),stringsAsFactors = F)
  x<-x[which(x[["IDDATE"]]%in%log_replace$id),]
  x
  })
orifm<-Reduce(function(x,y){dplyr::full_join(x,y,by=c("ID","CDATE","IDDATE"))},ori_fm_list)
orifm[-2][which(is.na(orifm[-2]),arr.ind = T)]<-""
#write.csv(orifm,file="~/Documents/github/UPMC/TRANSFER/PT/negout_outofrange.csv")


