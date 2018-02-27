---
Title: "Automator"
Author: "Jiazhou Chen"
Version: 1.0
---

#This script use Mac's Automator and calendar event to automatically refresh the b-social RedCap Database

#This part of the script contains the main functions:
  #0.5/1 Try to package it so call library(bsrc) can load all functions
  #1/1 start up function: 
jiazhou.startup()  ###!!NOT INCLUDED D/T SECURITY CONCERNS!###
  #1/1 REFRESH main function, now with single ID mode:
    #Force update, force run, if output maxevent db, if upload
  #1/1 Backup RedCap full database that can be uploaded online if anything goes wrong
  #0/1 MetricWire Data Grab and progress update
  #1/1 EMA and MRI status update 

#########################
##  Project AUTOMATOR  ##  
##  Part I, Functions  ##
#########################

library(REDCapR)
library(data.table)
library(lubridate)

######refresh######
bsrc.refresh<-function (forcerun,token, forceupdate=T, output, upload, ID) {
  if (missing(ID)) {ID<-NA}
  if (missing(output)) {output<-FALSE
  print("No output by default")}
  if (!exists("input.token")) {input.token <- readline(prompt = "Please input the RedCap api token: ")}
  if (missing(token)){token<-input.token}
  if (missing(upload)) {upload<-TRUE
  print("By default the refresh function will always upload to RedCap")}
  ifrun<-bsrc.checkdatabase(forcerun = forcerun,token = token, forceupdate = forceupdate)
  if (ifrun){
    #get info from registration
    subreg<-funbsrc[funbsrc$redcap_event_name=="enrollment_arm_1",] #take out only enrollment for efficiency
    subreg<-subreg[,1:50] #take only the regi part
    subreg$curage<-as.period(interval(start = as.Date(subreg$registration_dob), end = Sys.Date()))$year #Get current age
    subreg$sincelastfu<-as.period(interval(start = as.Date(subreg$registration_consentdate), end = Sys.Date())) #get since date 
    subreg$fudue<-round((as.numeric(as.period(interval(start = as.Date(subreg$registration_consentdate), end = Sys.Date()),unit = "month")$month)/12)/0.5)*0.5 #Fu due
    regitemp<-subreg[,c(grep("registration_redcapid",names(subreg)),grep("registration_consentdate",names(subreg)),grep("curage",names(subreg)):length(names(subreg)))]
    
    #find max fudate:
    funbsrc$fudemo_visitdate[which(funbsrc$fudemo_visitdate=="")]<-NA
    maxfudate<-aggregate(na.exclude(as.Date(funbsrc$fudemo_visitdate)),by=list(funbsrc$registration_redcapid[!is.na(funbsrc$fudemo_visitdate)]),max)
    names(maxfudate)<-c("registration_redcapid","fudemo_visitdate")
    
    #find max fuevent:
    subevent<-subset(funbsrc,select = c("registration_redcapid","redcap_event_name","fudemo_visitdate"))
    maxevent<-subevent[match(interaction(maxfudate$registration_redcapid,maxfudate$fudemo_visitdate),interaction(subevent$registration_redcapid,subevent$fudemo_visitdate)),]
    maxevent<-merge(regitemp,maxevent,all.x = T)
    maxevent$fudue[maxevent$sincelastfu$year==0 & maxevent$sincelastfu$month < 6 & !is.na(maxevent$sincelastfu) & maxevent$fudue == 0]<-0.25
    maxevent$fudemo_visitdate[is.na(maxevent$fudemo_visitdate)]<-maxevent$registration_consentdate[is.na(maxevent$fudemo_visitdate)]
    maxevent$months<-as.numeric(gsub("_months_.*$","",maxevent$redcap_event_name))
    maxevent$months[is.na(maxevent$months)]<-0
    maxevent$years<-maxevent$months/12
    maxevent$diff<-maxevent$fudue-maxevent$years
    maxevent$daysincefu<-as.numeric(Sys.Date()-as.Date(maxevent$fudemo_visitdate))
    maxevent$registration_consentdate<-NULL
    maxevent$redcap_event_name<-NULL
    maxevent$months<-NULL
    maxevent$fudemo_visitdate<-NULL
      #Refine indicator so folks who got in early will not:
      maxevent$diff[which(maxevent$daysincefu < 60)]<-0
    
    #find IPDE Date:
    funbsrc$ipde_date[which(funbsrc$ipde_date=="")]<-NA
    funbsrc$ipde_bpd_date[which(funbsrc$ipde_bpd_date=="")]<-NA
    ipdedateonly<-data.frame(funbsrc$registration_redcapid,as.Date(funbsrc$ipde_date),as.Date(funbsrc$ipde_bpd_date))
    names(ipdedateonly)<-c("registration_redcapid","ipde_date","ipde_bpd_date")
    ipdedateonly<-ipdedateonly[which(!is.na(ipdedateonly$ipde_date) | !is.na(ipdedateonly$ipde_bpd_date)),]
    ipdedateonly$ipde_date[is.na(ipdedateonly$ipde_date)]<-ipdedateonly$ipde_bpd_date[is.na(ipdedateonly$ipde_date)]
    ipdedateonly$ipde_bpd_date<-NULL
    ipdedate<-aggregate(ipdedateonly$ipde_date,by=list(ipdedateonly$registration_redcapid), FUN=max)
    names(ipdedate)<-c("registration_redcapid","ipde_date")
    maxevent<-merge(maxevent,ipdedate,all = T)
    
    
    #Get the names of progress report [remember to organize the dataframe in RedCap order; NOPE BAD IDEA HARD CODE IT]
    names(maxevent)<-c("registration_redcapid","prog_cage","prog_endor","prog_endor_y","prog_lastfollow","prog_diff","prog_endorfu","prog_latestipdedate")
    
    idmatch<-data.frame(subreg$registration_id,subreg$registration_soloffid,subreg$registration_redcapid)
    names(idmatch)<-c('id','soloffid','redcapid')
    
    if (!is.na(ID)){
    if(any(ID %in% idmatch$soloffid | ID %in% idmatch$id)){
      if(ID %in% idmatch$soloffid) {ID<-as.character(idmatch$redcapid[which(idmatch$soloffid==ID)])}
      else{ID<-as.character(idmatch$redcapid[which(idmatch$id==ID)])}
    }
    if (ID %in% maxevent$registration_redcapid) {
      singleid<-maxevent[which(maxevent$registration_redcapid==ID),]
      ids<-idmatch[which(idmatch$redcapid==ID),]
      upload<-FALSE
      print("Single ID mode will not refresh RedCap")
      print(ids)
      print(singleid)}}
    
    if (output) {return(maxevent) 
      print("Here you go, you asked for it.")
    }
       
    if (upload) {
    result.maxevent<-redcap_write(maxevent,token = input.token,redcap_uri = input.uri)
    return(result.maxevent)
    if (result.maxevent$success) {
      Print("Congrats, Upload was successful")}
    else ("Something went wrong during uploading, double check")
  }  
  }
}

####EMA/fMRI Status#####
Variable: 
bsrc.emamri<-function (forcerun,token,forcerun,token, forceupdate, output, upload)
{
  
}

####Missing Assessment####


####Back-up######
bsrc.backup<-function(forcerun,token, path,clean=T,expiration=30) {
  if (missing(token)){token<-input.token}
  ifrun<-bsrc.checkdatabase(forcerun = forcerun,token = token)
  if (missing(path)) {path<-"/Volumes/llmd/BPD Database/RedCap Database Back-Up"
    print("Default Location is L Drive/BPD Database")}
  if (ifrun) {
    csvname<-paste(sep = "_","RedCapFullDataBackUp",Sys.Date(),"RAW.csv")
    csv<-paste(path,csvname,sep = "/")
    write.csv(funbsrc,csv)
    print("Success")
    
    lfile<-list.files(path=path,pattern="*.csv")
    yur<-as.numeric(Sys.Date()-as.Date(as.character(as.data.table(strsplit(lfile,split = "_"))[2])))
    delfile<-lfile[which(yur>expiration)]  
    if (clean & length(delfile)>0) {
      print("By default, function also clean out database backup thats 30 days old; use clean=F or expiration = (numeric)")
      print("Removing old files")
      file.remove(delfile)
    }
  print("DONE")
  }
}

      



####MetricWire:#####
bsrc.metric2redcap <- function(forcerun,token) {
  if (missing(token)){token<-input.token}
  ifrun<-bsrc.checkdatabase(forcerun = forcerun,token = token)
  llist<-list.files(path=)
}


#Find ID:

bsrc.name2id <- function(ID,forcerun=FALSE){
  ifrun<-bsrc.checkdatabase(forcerun = forcerun)
  if (ifrun==TRUE){
    
  }

  
}



####################
###   Part II:   ###
###  Main Script ###
####################

#Packages:
library(REDCapR)
library(lubridate)

#Load configuration 
jiazhou.startup()

#Refresh RedCap values
result<-bsrc.refresh(forceupdate = T,token = input.token)
if(result$success) {
  #MailR
  #Send an confirmation of the process to myself

csvname<-paste("Redcap_Log",Sys.Date(),".csv",sep = "_")
write.csv(as.data.frame(result$outcome_message,result$affected_ids,result$status_code), file = csvname)}

#Backup
bsrc.backup()

#Grab metricwire function:
bsrc.metric2redcap()

#produce excel function:
bsrc.excelproduction()


