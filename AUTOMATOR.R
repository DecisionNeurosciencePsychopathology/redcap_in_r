---
Title: "Automator"
Author: "Jiazhou Chen"
Version: 1.31
---

#This script use Mac's Automator and calendar event to automatically refresh the b-social RedCap Database

#Version 1.31:
  #Updated bsrc.refresh() to skip 6 mon if 3 mon is completed. 
  #Remove the main script from AUTOMATOR so that it only contains functions
  
#Version 1.3:
  #Just some updates to the good old bsrc.refresh()
  
#Version 1.2:
  #bsrc.refresh() gains the update to fMRI status function
  
#Version 1.1:
  #bsrc.refresh() gains the update to EMA protocl and latest IPDE function

#Version 1:
  #1/1 start up function: 
#jiazhou.startup()  ###!!NOT INCLUDED D/T SECURITY CONCERNS!###
  #1/1 REFRESH main function, now with single ID mode:
    #Force update, force run, if output maxevent db, if upload
  #1/1 Backup RedCap full database that can be uploaded online if anything goes wrong
  #0/0 MetricWire Data Grab and progress update: #See Ecologist Re; bsrc.ema.main
  #1/1 EMA and MRI status update 

#########################
##  Project AUTOMATOR  ##  
##  Part I, Functions  ##
#########################

library(REDCapR)
library(data.table)
library(lubridate)

######refresh######
bsrc.refresh<-function (forcerun.e=F,token.e, forceupdate.e=T, output=F, upload=T, ID) {
  if (missing(ID)) {ID<-NA}
  if (!exists("input.token")) {input.token <- readline(prompt = "Please input the RedCap api token: ")}
  if (missing(token.e)){token.e<-input.token}
  print("By default the refresh function will always upload to RedCap")
  ifrun<-bsrc.checkdatabase(forcerun = forcerun.e,forceupdate = forceupdate.e,token = token.e)
  if (ifrun){
    #get info from registration
    subreg<-bsrc.getevent(eventname = "enrollment_arm_1",forcerun = T,subreg = T)
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
    #if 3 month is completed, skip 6 month.
    maxevent$diff[which(maxevent$fudue==0.5 & maxevent$years==0.25)]<-0
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
    
    #rename variables:
    names(maxevent)<-c("registration_redcapid","prog_cage","prog_endor","prog_endor_y","prog_lastfollow","prog_diff","prog_endorfu","prog_latestipdedate")
    
    #For fMRI Status:
    #Dates:
    mripgonly<-bsrc.getform(formname = c("fmri_screening_form","fmri_session_checklist"),forcerun.e = T) 
    mripgonly.a<-subset(mripgonly,select = c("registration_redcapid","mriscreen_yesno","mricheck_scheudleddate","mricheck_scanneddate","mricheck_mricomplete___0118","mricheck_mricomplete___p16")) 
    #mripgonly.b<-mripgonly.a[which(!is.na(mripgonly.a$mriscreen_yesno) | !is.na(mripgonly.a$mricheck_scheudleddate) | !is.na(mripgonly.a$mricheck_mricomplete___p16)),]
    mripgonly.c<-mripgonly.a
    mripgonly.c$prog_fmristatus<-NA
    mripgonly.c$prog_fmristatus[which(mripgonly.c$mriscreen_yesno==0)]<-"REFUSED/INELIGIBLE"
    mripgonly.c$prog_fmristatus[which(mripgonly.c$mriscreen_yesno==1)]<-"Screened/MaybeEligible"
    mripgonly.c$prog_fmristatus[which(mripgonly.c$mricheck_mricomplete___p16==1)]<-"2016 Pilot"
    mripgonly.c$prog_fmristatus[which(!is.na(mripgonly.c$mricheck_scheudleddate))]<-paste("Scheduled:",mripgonly.c$mricheck_scheudleddate[which(!is.na(mripgonly.c$mricheck_scheudleddate))])
    mripgonly.c$prog_fmristatus[which(!is.na(mripgonly.c$mricheck_scanneddate))]<-paste("Scanned:",mripgonly.c$mricheck_scanneddate[which(!is.na(mripgonly.c$mricheck_scanneddate))])
    #Subset:
    mripgonly.d<-subset(mripgonly.c,select = c("registration_redcapid","prog_fmristatus"))
    mripgonly.e<-na.omit(mripgonly.d)
    #Merge:
    maxevent<-merge(maxevent,mripgonly.d, all = T)
    
    #For EMA Status:
    #Due Date:
    funbsrc$ema_setuptime[which(funbsrc$ema_setuptime=="")]<-NA
    emaonly<-bsrc.getform(formname = "ema_session_checklist", forcerun.e = T)
    emaonly<-emaonly[which(!is.na(emaonly$ema_setuptime)),]
    emaonly.s<-subset(emaonly,select = c("registration_redcapid","redcap_event_name","ema_setuptime","ema_completed___2"))
    emaonly.s$ema_setuptime<-as.Date(emaonly.s$ema_setuptime)
    emaonly.s$prog_emadued<-emaonly.s$ema_setuptime+3
    emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptimeema.+22]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+22]+22
    emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptime+15]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+15]+15
    emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptime+8]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+8]+8
    emaonly.s$prog_emadued[Sys.Date() >= emaonly.s$ema_setuptime+21]<-emaonly.s$ema_setuptime[Sys.Date() >= emaonly.s$ema_setuptime+21]+21
    
    emaonly.s$prog_emastatus<-paste("IP3d:",emaonly.s$ema_setuptime+3)
    emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+22)]<-paste("IP21d:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+22)]+22)
    emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+15)]<-paste("IP14d:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+15)]+15)
    emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+8)]<-paste("IP7d:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+8)]+8)
    emaonly.s$prog_emastatus[which(Sys.Date() >= emaonly.s$ema_setuptime+21)]<-paste("DONE:",emaonly.s$ema_setuptime[which(Sys.Date() >= emaonly.s$ema_setuptime+21)]+21)
    emaonly.s$prog_emastatus[which(emaonly.s$ema_completed___2==1)]<-paste("Completed:",emaonly.s$ema_setuptime[which(emaonly.s$ema_completed___2==1)]+21)
    emaonly.s$prog_emastatus_di<-emaonly.s$prog_emastatus
    emaonly.s$prog_emastatus_di[emaonly.s$ema_completed___2==1]<-NA
    emaonly.x<-subset(emaonly.s,select = c("registration_redcapid","prog_emastatus","prog_emastatus_di","prog_emadued"))
    #In Progress: 
    emaonly.j<-bsrc.getform(formname = "ema_screening_form", forcerun.e = T)
    emaonly.j<-subset(emaonly.j[!(emaonly.j$registration_redcapid %in% emaonly.x$registration_redcapid) & emaonly.j$ema_yesno==1,],select = c("registration_redcapid"))
    emaonly.j$prog_emastatus<-"Screened&Ready"
    #Merge:
    emaonly.r<-merge(emaonly.x,emaonly.j,all=T)
    #Merge with Main maxevent upload:
    maxevent<-merge(maxevent,emaonly.r,all = T)
    #####Work on early termination folks########
    #Get the names of progress report [remember to organize the dataframe in RedCap order; NOPE BAD IDEA HARD CODE IT]
    #c("registration_redcapid","prog_cage","prog_endor","prog_endor_y","prog_lastfollow","prog_diff","prog_endorfu","prog_latestipdedate","prog_emastatus","prog_emastatus_di","prog_emadued")
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

####Missing Assessment Given a Arm####
#Pending

####Back-up######
bsrc.backup<-function(forcerun.e=F, forceupdate.e=F,token, path,clean=T,expiration=30) {
  if (missing(token)){token<-input.token}
  ifrun<-bsrc.checkdatabase(forcerun = forcerun.e,token = token,forceupdate = forceupdate.e)
  if (missing(path)) {path<-"/Users/jiazhouchen/Box Sync/skinner/projects_analyses/Project BPD Longitudinal/Redcap Database Backup"
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


