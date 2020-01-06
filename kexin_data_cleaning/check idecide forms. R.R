setwd("~/Documents/github/UPMC/TRANSFER/PT")
source("~/Documents/github/UPMC/startup.R")
skinnerdir<-"~/Box/skinner/data/"

idecide<-read.csv(paste0(skinnerdir,"iDecide subs/Ver2-iDECIDE Participation Flow-COUNT.csv"),stringsAsFactors = F)
idecide<-bsrc.findid(idecide,idmap = idmap)
IDECIDE<-bsrc.getform(formname = "idecide_completion_and_data_form",protocol = ptcs$protect)
library(dplyr)
colnames(idecide)<-gsub("masterdemoid","registration_redcapid",colnames(idecide))
IDECIDE$registration_redcapid<-as.integer(IDECIDE$registration_redcapid)
idecide<-left_join(idecide,IDECIDE,by="registration_redcapid");rm(IDECIDE)
idecide[which(idecide=="",arr.ind = T)]<-NA
sum(is.na(idecide$ID)) #anyone w/o id
any(rowSums(is.na(idecide[-1]),na.rm = T)==ncol(idecide)) # anyone has no data?
