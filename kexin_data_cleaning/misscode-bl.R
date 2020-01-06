## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
source('~/Documents/github/UPMC/startup.R')
############startuptransfer#############
rootdir="~/Box/skinner/data/Redcap Transfer/PT transfer/final_1218_baseline/"
#allsub<-read.csv(paste0(rootdir,"ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt2.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
rcform<-unique(var_map$Form_name)

for (i in 1:length(rcform)){
  rcfm<-rcform[i]
  acform <- gsub(".csv","",unique(subset(var_map,Form_name==rcfm)$path))
  comb_fm_list<-lapply(acform, function(acform){read.csv(paste0(rootdir,"form_",acform,"_2019-12-18.csv"), stringsAsFactors = F)});names(comb_fm_list)<-acform # grab forms 
assign(paste0("id",i),unlist(sapply(comb_fm_list,function(x){x[["registration_redcapid"]]})))
  }


comb_fm_list<-lapply(fm_dir, function(fm_dir){read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)});names(comb_fm_list)<-cb$path # grab forms 