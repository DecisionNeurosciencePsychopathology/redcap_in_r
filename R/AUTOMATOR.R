#---
#Title: "Automator"
#Author: "Jiazhou Chen"
#Version: 1.6.1
#---
#This script use Mac's Automator and calendar event to automatically refresh the b-social RedCap Database
#Version 1.6 & 1.6.1 Changelog:
#Revision to refresh function to be compatible with the new data organization method
#Refresh function and backup function now fully functional when used in Automator
#Version 1.5:
#Updated the refresh to better process EMA pt who completed EMA v3; and those who terminated early.
#Updated the bsrc.refresh() to update local copy of the database & eliminate terminated subs
#Updated the bsrc.backup() to use new mech to grab date since creataion of files
#Updated bsrc.refresh() to skip 6 mon if 3 mon is completed.
#Remove the main script from AUTOMATOR so that it only contains functions
#Just some updates to the good old bsrc.refresh()
#bsrc.refresh() gains the update to fMRI status function
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

######refresh######
bsrc.refresh<-function (ptcs=ptcs,forceskip=F,forceupdate=F, output=F, upload=T, ...) {
  curdb <-bsrc.checkdatabase2(protocol = ptcs$bsocial, forceskip = forceskip, forceupdate = forceupdate)
  masterdemo <- bsrc.checkdatabase2(protocol = ptcs$masterdemo, forceskip = forceskip, forceupdate = forceupdate)
  funbsrc<-curdb$data
  funevent<-curdb$eventmap
  funstrc<-curdb$metadata
  ifrun<-curdb$success
  ptcs$bsocial$redcap_uri->input.uri
  ptcs$bsocial$token->input.token
  if(upload) {message("By default the refresh function will always upload to RedCap")}
  if (ifrun){
    #get info from registration
    subreg<-bsrc.getform(formname = "record_registration",curdb = masterdemo)
    subreg$curage<-lubridate::as.period(lubridate::interval(start = as.Date(subreg$registration_dob,format = "%Y-%m-%d"), end = Sys.Date()))$year #Get current age
    subreg$sincelastfu<-lubridate::as.period(lubridate::interval(start = as.Date(subreg$reg_condate_bsocial,format = "%Y-%m-%d"), end = Sys.Date())) #get since date
    subreg$fudue<-round((as.numeric(lubridate::as.period(lubridate::interval(start = as.Date(subreg$reg_condate_bsocial,format = "%Y-%m-%d"), end = Sys.Date()),unit = "month")$month)/12)/0.5)*0.5 #Fu due
    regitemp<-subreg[,c(grep("registration_redcapid",names(subreg)),grep("reg_condate_bsocial",names(subreg)),grep("curage",names(subreg)):length(names(subreg)))]
    regitemp <- regitemp[which(regitemp$reg_condate_bsocial!="" & !is.na(regitemp$reg_condate_bsocial)),]
    regitemp$registration_consentdate <- regitemp$reg_condate_bsocial
    #find max fudate:
    funbsrc$fudemo_visitdate[which(funbsrc$fudemo_visitdate=="")]<-NA
    maxfudate<-aggregate(na.exclude(as.Date(funbsrc$fudemo_visitdate)),by=list(funbsrc$registration_redcapid[!is.na(funbsrc$fudemo_visitdate)]),max)
    names(maxfudate)<-c("registration_redcapid","fudemo_visitdate")

    #find max fuevent:
    subevent<-subset(funbsrc,select = c("registration_redcapid","redcap_event_name","fudemo_visitdate"))
    maxevent<-subevent[match(interaction(maxfudate$registration_redcapid,maxfudate$fudemo_visitdate),interaction(subevent$registration_redcapid,subevent$fudemo_visitdate)),]
    maxevent<-merge(regitemp,maxevent,all.x = T)
    maxevent$registration_consentdate[which(maxevent$registration_consentdate=="")]<-NA
    maxevent$fudemo_visitdate[is.na(maxevent$fudemo_visitdate)]<-maxevent$registration_consentdate[is.na(maxevent$fudemo_visitdate)]
    maxevent$months<-as.numeric(gsub("_months_.*$","",maxevent$redcap_event_name))
    maxevent$months[is.na(maxevent$months)]<-0
    maxevent$years<-maxevent$months/12
    maxevent$diff<-maxevent$fudue-maxevent$years
    maxevent$daysincefu<-as.numeric(Sys.Date()-as.Date(maxevent$fudemo_visitdate))
    maxevent$sincelastfu<-lubridate::as.period(lubridate::interval(start = as.Date(maxevent$fudemo_visitdate,format = "%Y-%m-%d"), end = Sys.Date())) #get since date
    maxevent$registration_consentdate<-NULL
    maxevent$redcap_event_name<-NULL
    maxevent$months<-NULL
    #maxevent$fudemo_visitdate<-NULL

    #Get 3 month
    maxevent$fudue[maxevent$sincelastfu$year==0 & maxevent$sincelastfu$month < 6 & !is.na(maxevent$sincelastfu) & maxevent$fudue == 0 & maxevent$sincelastfu$month > 1]<-0.25
    #if 3 month is completed, skip 6 month.
    maxevent$fudue[which(maxevent$fudue==0.5 & maxevent$years==0.25)]<-1
    #RECALCULATE
    maxevent$diff<-maxevent$fudue-maxevent$years
    maxevent$diff[which(maxevent$fudue==0.5 & maxevent$years==0.25)]<-0
    #Refine indicator so folks who got in early will not:
    maxevent$diff[which(maxevent$daysincefu < 60)]<-0

    #find IPDE Date:
    funbsrc$ipde_date[which(funbsrc$ipde_date=="")]<-NA
    funbsrc$ipde_bpd_date[which(funbsrc$ipde_bpd_date=="")]<-NA
    ipdedateonly<-data.frame(funbsrc$registration_redcapid,as.Date(funbsrc$ipde_date),funbsrc$ipde_cm_borderline,funbsrc$ipde_dxc_borderline,as.Date(funbsrc$ipde_bpd_date),funbsrc$ipde_bpd_cm_borderline,funbsrc$ipde_bpd_dxc_borderline)
    names(ipdedateonly)<-c("registration_redcapid","ipde_date","ipde_cm","ipde_dxc","ipde_bpd_date","ipde_bpd_cm","ipde_bpd_dxc")
    ipdedateonly<-ipdedateonly[which(!is.na(ipdedateonly$ipde_date) | !is.na(ipdedateonly$ipde_bpd_date)),]
    ipdedateonly$ipde_date[is.na(ipdedateonly$ipde_date)]<-ipdedateonly$ipde_bpd_date[is.na(ipdedateonly$ipde_date)]
    ipdedateonly$ipde_cm[is.na(ipdedateonly$ipde_cm)]<-ipdedateonly$ipde_bpd_cm[is.na(ipdedateonly$ipde_cm)]
    ipdedateonly$ipde_dxc[is.na(ipdedateonly$ipde_dxc)]<-ipdedateonly$ipde_bpd_dxc[is.na(ipdedateonly$ipde_dxc)]
    ipdedateonly$ipde_bpd_date<-NULL
    ipdedateonly$ipde_bpd_cm<-NULL
    ipdedateonly$ipde_bpd_dxc<-NULL
    ipdedate<-aggregate(ipdedateonly$ipde_date,by=list(ipdedateonly$registration_redcapid), FUN=max)
    names(ipdedate)<-c("registration_redcapid","ipde_date")
    ipdedate$ipde_cm <-ipdedateonly$ipde_cm [match(interaction(ipdedate$registration_redcapid,ipdedate$ipde_date),interaction(ipdedateonly$registration_redcapid,ipdedateonly$ipde_date))]
    ipdedate$ipde_dxc<-ipdedateonly$ipde_dxc[match(interaction(ipdedate$registration_redcapid,ipdedate$ipde_date),interaction(ipdedateonly$registration_redcapid,ipdedateonly$ipde_date))]
    maxevent<-merge(maxevent,ipdedate,all = T)

    #rename variables:
    names(maxevent)<-c("registration_redcapid","registration_consentdate","prog_cage","prog_endor","prog_endor_y","prog_l_visitdate","prog_lastfollow",
                       "prog_diff","prog_endorfu","prog_latestipdedate","prog_latestipdes_cm","prog_latestipdes_dx")

    #For fMRI Status:
    #Dates:
    mripgonly<-bsrc.getform(formname = c("fmri_screening_form","fmri_session_checklist"),mod = F,aggressivecog=0,curdb = curdb)
    mripgonly.a<-subset(mripgonly,select = c("registration_redcapid","mriscreen_yesno","mricheck_scheudleddate","mricheck_scanneddate","mricheck_mricomplete___0118","mricheck_mricomplete___p16"))
    #mripgonly.b<-mripgonly.a[which(!is.na(mripgonly.a$mriscreen_yesno) | !is.na(mripgonly.a$mricheck_scheudleddate) | !is.na(mripgonly.a$mricheck_mricomplete___p16)),]
    mripgonly.c<-mripgonly.a
    mripgonly.c$prog_fmristatus<-NA
    mripgonly.c$mricheck_scheudleddate[which(mripgonly.c$mricheck_scheudleddate=="")]<-NA
    mripgonly.c$mricheck_scanneddate[which(mripgonly.c$mricheck_scanneddate=="")]<-NA
    mripgonly.c$prog_fmristatus[which(mripgonly.c$mriscreen_yesno==0)]<-"REFUSED/INELIGIBLE"
    mripgonly.c$prog_fmristatus[which(mripgonly.c$mriscreen_yesno==1)]<-"Screened/MaybeEligible"
    mripgonly.c$prog_fmristatus[which(!is.na(mripgonly.c$mricheck_scheudleddate))]<-paste("Scheduled:",mripgonly.c$mricheck_scheudleddate[which(!is.na(mripgonly.c$mricheck_scheudleddate))])
    mripgonly.c$prog_fmristatus[which(mripgonly.c$mricheck_mricomplete___p16==1)]<-"2016 Pilot"
    mripgonly.c$prog_fmristatus[which(!is.na(mripgonly.c$mricheck_scanneddate))]<-paste("Scanned:",mripgonly.c$mricheck_scanneddate[which(!is.na(mripgonly.c$mricheck_scanneddate))])
    #Subset:
    mripgonly.d<-subset(mripgonly.c,select = c("registration_redcapid","prog_fmristatus"))
    mripgonly.e<-na.omit(mripgonly.d)
    #Merge:
    maxevent<-merge(maxevent,mripgonly.d, all = T)

    #For EMA Status:
    #Due Date:

    emaonly<-bsrc.getform(formname = "ema_session_checklist", curdb=curdb)
    emaonly$ema_setuptime[which(emaonly$ema_setuptime=="")]<-NA
    emaonly<-emaonly[which(!is.na(emaonly$ema_setuptime)),]
    emaonly.s<-subset(emaonly,select = c("registration_redcapid","redcap_event_name","ema_setuptime","ema_completed___2","ema_completed___3","ema_completed___999","ema_termdate"))
    emaonly.s$ema_setuptime<-as.Date(emaonly.s$ema_setuptime)
    emaonly.s$prog_emadued<-emaonly.s$ema_setuptime+3
    emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptime+22 & Sys.Date() > emaonly.s$ema_setuptime+15]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+22 & Sys.Date() > emaonly.s$ema_setuptime+15]+22
    emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptime+15 & Sys.Date() > emaonly.s$ema_setuptime+8]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+15 & Sys.Date() > emaonly.s$ema_setuptime+8]+15
    emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptime+8 & Sys.Date() > emaonly.s$ema_setuptime+4]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+8 & Sys.Date() > emaonly.s$ema_setuptime+4]+8
    emaonly.s$prog_emadued[Sys.Date() > emaonly.s$ema_setuptime+21]<-emaonly.s$ema_setuptime[Sys.Date() > emaonly.s$ema_setuptime+21]+21
    emaonly.s$prog_emadued[which(Sys.Date() >= emaonly.s$ema_termdate & emaonly.s$ema_completed___999==1)]<-emaonly.s$ema_termdate[which(Sys.Date() >= emaonly.s$ema_termdate & emaonly.s$ema_completed___999==1)]
    #EMA IP:
    emaonly.s$prog_emastatus<-paste("3days",emaonly.s$ema_setuptime+3)
    emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+22 & Sys.Date() > emaonly.s$ema_setuptime+15)]<-paste("3wks:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+22 & Sys.Date() > emaonly.s$ema_setuptime+15)]+22)
    emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+15 & Sys.Date() > emaonly.s$ema_setuptime+8)]<-paste("2wks:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+15 & Sys.Date() > emaonly.s$ema_setuptime+8)]+15)
    emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+8 & Sys.Date() > emaonly.s$ema_setuptime+4)]<-paste("1wks:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+8 & Sys.Date() > emaonly.s$ema_setuptime+4)]+8)
    emaonly.s$prog_emastatus[which(Sys.Date() > emaonly.s$ema_setuptime+21)]<-paste("DONE:",emaonly.s$ema_setuptime[which(Sys.Date() > emaonly.s$ema_setuptime+21)]+21)
    emaonly.s$prog_emastatus[which(emaonly.s$ema_completed___2==1)]<-paste("Completed:",emaonly.s$ema_setuptime[which(emaonly.s$ema_completed___2==1)]+21)
    emaonly.s$prog_emastatus[which(emaonly.s$ema_completed___3==1)]<-paste("Completed:",emaonly.s$ema_setuptime[which(emaonly.s$ema_completed___3==1)]+21)
    emaonly.s$prog_emastatus[which(Sys.Date() >= emaonly.s$ema_termdate & emaonly.s$ema_completed___999==1)]<-"EMA:999"
    emaonly.s$prog_emastatus_di<-emaonly.s$prog_emastatus
    emaonly.s$prog_emastatus_di[emaonly.s$ema_completed___2==1]<-NA
    emaonly.s$prog_emastatus_di[emaonly.s$ema_completed___3==1]<-NA
    emaonly.x<-subset(emaonly.s,select = c("registration_redcapid","prog_emastatus","prog_emastatus_di","prog_emadued"))
    #Screened:
    emaonly.j<-bsrc.getform(formname = "ema_screening_form", curdb = curdb)
    emaonly.j<-data.frame(registration_redcapid=emaonly.j[which(!(emaonly.j$registration_redcapid %in% emaonly.x$registration_redcapid) & emaonly.j$ema_yesno==1),c("registration_redcapid")])
    emaonly.j$prog_emastatus<-"Screened&Ready"
    #Merge:
    emaonly.r<-merge(emaonly.x,emaonly.j,all=T)
    #Merge with Main maxevent upload:
    maxevent<-merge(maxevent,emaonly.r,all = T)
    maxevent$prog_endor<-as.character(maxevent$prog_endor)
    maxevent$prog_latestipdedate<-as.character(maxevent$prog_latestipdedate)
    maxevent$prog_emadued<-as.character(maxevent$prog_emadued)

    #Take out terminated folks
    terminatedsublist<-subreg$registration_redcapid[which(!is.na(subreg$reg_term_bsocial))]
    maxevent<-maxevent[which(!maxevent$registration_redcapid %in% terminatedsublist),]

    #Update back to local database so no need to reload:
    #print("Updating Local Database")
    #funbsrc[match(maxevent$registration_redcapid,funbsrc$registration_redcapid),match(names(maxevent),names(funbsrc))]<-maxevent
    #funbsrc<<-funbsrc
    #subreg<<-bsrc.getevent(eventname = "enrollment_arm_1",forcerun = T,subreg = T)
    if (output) {return(maxevent)
      print("Here you go, you asked for it.")
    }

    if (upload) {
      result.maxevent<-redcap_upload(maxevent,token = input.token,redcap_uri = input.uri,NAoverwrite = F,retry_whenfailed = T)
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
bsrc.backup<-function(protocol=protocol.cur,forceskip=F,forceupdate=T,curdb=NULL,path=NULL,clean=T,expiration=30,...) {
  protocol$rdpath->rdpath
  if (is.null(curdb)){
    curdb<-bsrc.checkdatabase2(protocol = protocol, forceskip = forceskip, forceupdate = forceupdate,... = ...)}
  funbsrc<-curdb$data
  if (is.null(path)) {
    if(!is.null(rdpath)){
      path<-paste(paste(strsplit(rdpath,.Platform$file.sep)[[1]][1:(length(strsplit(rdpath,.Platform$file.sep)[[1]])-1)],collapse = .Platform$file.sep),"Backup",sep = .Platform$file.sep)
      print("Default Location is BOXSYNC")
    } else {stop("No Path")}
  }

  backupname<-paste(sep = "_","RedCapFullDataBackUp",Sys.Date(),"RAW.rdata")
  topath<-paste(path,backupname,sep = "/")
  file.copy(from = rdpath, to = topath, overwrite = T)


  lfile<-list.files(path=path,pattern="*RAW.rdata")
  yur<-as.numeric(Sys.Date()-as.Date(sapply(strsplit(lfile,split = "_"), "[[",2)))
  delfile<-lfile[which(yur>expiration)]
  if (clean & length(delfile)>0) {
    print("By default, function also clean out database backup thats 30 days old; use clean=F or expiration = (numeric)")
    print("Removing old files")
    delfile<-paste(path,delfile,sep="/")
    file.remove(delfile)

    print("DONE")
  }
}

#####################Sync Stuff #############################
# bsrc.sync<-function(ptc.from=NULL){}
#
# ptc.from = ptcs$masterdemo
# ptc.to = ptcs$bsocial
# id.var = "registration_redcapid"
# id.from = "registration_redcapid"
# id.to = "registration_redcapid"
# ref_df = data.frame(from = c("reg_condate_bsocial"),to=c("registration_consentdate"),stringsAsFactors = F)
# add.new.id = FALSE
#
# df_from<-redcap_read(fields = c(id.var,ref_df$from),redcap_uri = ptc.from$redcap_uri,token = ptc.from$token,batch_size = 1000L)
# lapply(1:nrow(ref_df),function(rx){
#
#
#
# })






