#########################
##  Project AUTOMATOR  ##  
##  Part I, Functions  ##
#########################

#This script use Mac's Automator and calendar event to automatically refresh the b-social RedCap Database
#Also contain function to generate csv calendar event (good for google) for EMA participants
#Version: 0.1
#This part of the script contains the main functions:
  #0/1 Try to package it so call library(bsrc) can load all functions
  #1/1 start up function



#connection
bsrc.conredcap <- function(uri,token,batch_size,output) {
  if (missing(uri)) {uri<-'DNPL'
  print("By default, the location is set to Pitt's RedCap.")}
  if (missing(batch_size)) {batch_size<-"50" 
  print("By default, the batch size is 50 unique records")}
  if (missing(output)) {output<-F
  print("By default, the database will be assigned to `funbsrc` as a data frame and returns nothing if wish to assign db to something, use arguement output = T")}
  if (uri == 'DNPL'|uri == 'PITT') {input.uri='https://www.ctsiredcap.pitt.edu/redcap/api/'}
  else (input.uri<-uri)
  if (missing(token)) {input.token <- readline(prompt = "Please input the RedCap api token: ")}
  redcap<-redcap_project$new(redcap_uri=input.uri, token=input.token)
  uri<<-input.uri
  token<<-input.token
  #test connection:
  funbsrc<-redcap$read(batch_size = batch_size)
  funbsrc<<-funbsrc$data
  strc<-redcap_metadata_read(redcap_uri = input.uri,token = input.token)
  funstrc<<-data.frame(strc$data$field_name,strc$data$form_name,strc$data$field_label)
  names(strc)<-c('field_name','form_name','field_label')
  if (length(funbsrc$data$registration_redcapid)>0) {
    print("Success! Database Loaded")
    jzc.connection.yesno<<-1
    jzc.connection.date<<-Sys.Date()} 
  else {
    print("Connection Failed, Please Try Again.") 
    jzc.connection.yesno<<-0}
  if (output==T){
    return(funbsrc)}
}

#checkdatebase
bsrc.checkdatabase<-function(replace,forcerun, token, forceupdate) {
  if(missing(token)){token<-input.token}
  if(missing(forcerun)){forcerun=FALSE}
  if(missing(forceupdate)){forceupdate=FALSE}
  if(!missing(replace)){funbsrc<-replace}
  if (exists('jzc.connection.date')==FALSE | exists('jzc.connection.date')==FALSE){
    jzc.connection.yesno<-0 
    jzc.connection.date<-NA}
  if (forceupdate==TRUE) {
    print("FORCEUPDATE")
    bsrc.conredcap(token = token)
  }
  if (jzc.connection.yesno == 1) {
    if (forcerun==TRUE | jzc.connection.date==Sys.Date()) {
      print("Database is loaded or was loaded today")
      ifrun<-TRUE
    }
    else {print("Local database is out of date, redownload now")
      ifrun<-FALSE
      bsrc.conredcap(token = token)
      ifrun<-bsrc.checkdatabase()}
  }
  else {print("RedCap Connection is not loaded, Retry Now")
    ifrun<-FALSE
    bsrc.conredcap(token = token)
    ifrun<-bsrc.checkdatabase()
    }
  
  return(ifrun)
}


#refresh
bsrc.refresh<-function (forcerun,token, forceupdate, output, upload, ID="all") {
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
    
    
    
    if(any(ID %in% idmatch$soloffid | ID %in% idmatch$id)){
      if(ID %in% idmatch$soloffid){ID<-as.character(idmatch$redcapid[which(idmatch$soloffid==ID)])}
      else{ID<-as.character(idmatch$redcapid[which(idmatch$id==ID)])}
    
    if (ID %in% maxevent$registration_redcapid) {
      singleid<-maxevent[which(maxevent$registration_redcapid==ID),]
      upload<-FALSE
      print("Single ID mode will not refresh RedCap")
      print(singleid)
    }
    
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

#MetricWire:
bsrc.metric2redcap <- function(forcerun,token) {
  if (missing(token)){token<-input.token}
  ifrun<-bsrc.checkdatabase(forcerun = forcerun,token = token)
  read
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

#Connect to RedCap
bsrc.checkdatabase(forceupdate = T, token = input.token)

#Refresh RedCap values
result<-bsrc.refresh(forceupdate = T,token = input.token)
if(result$success) {
#MailR
  #Send an confirmation of the process to myself
}
csvname<-paste("Redcap_Log",Sys.Date(),".csv",sep = "_")
write.csv(as.data.frame(result$outcome_message,result$affected_ids,result$status_code), file = csvname)


#Grab metricwire function:
bsrc.metric2redcap()

#produce excel function:
bsrc.excelproduction()


