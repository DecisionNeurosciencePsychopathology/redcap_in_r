#########################
##  Project AUTOMATOR  ##  
##  Part I, Functions  ##
#########################

#This script use Mac's Automator and calendar event to automatically refresh the b-social RedCap Database
#Also contain function to generate csv calendar event (good for google) for EMA participants
#Version: 0.1
#This part of the script contains the main function:

#connection
bsrc.conredcap <- function(uri,token,batch_size,ouput) {
  if (missing(uri)) {uri<-'DNPL'
  print("By default, the location is set to Pitt's RedCap.")}
  if (missing(batch_size)) {batch_size<-"50"}
  if (missing(ouput)) {output<-F
  print("By default, the database will be assigned to `funbsrc` as a data frame.")}
  if (uri == 'DNPL'|uri == 'PITT') {input.uri='https://www.ctsiredcap.pitt.edu/redcap/api/'}
  else (input.uri<-uri)
  if (missing(token)) {input.token <- readline(prompt = "Please input the RedCap api token: ")}
  redcap<-redcap_project$new(redcap_uri=input.uri, token=input.token)
  uri<<-input.uri
  token<<-input.token
  #test connection:
  funbsrc<-redcap$read(batch_size = batch_size)
  funbsrc<<-funbsrc$data
  funstrc<-redcap_metadata_read(redcap_uri = input.uri,token = input.token)
  funstrc<<-data.frame(strc$data$field_name,strc$data$form_name,strc$data$field_label)
  names(strc)<-c('field_name','form_name','field_label')
  if (length(funbsrc$data$registration_redcapid)>0) {
    print("Success! Database Loaded")
    jzc.connection.yesno<<-1
    jzc.connection.date<<-Sys.Date()} 
  else {
    print("Connection Failed, Please Try Again.") 
    jzc.connection.yesno<<-0}
  if (ouput<-F){
    return(funbsrc)}
}

#checkdatebase
bsrc.checkdatabase<-function(replace,forcerun, token) {
  if(missing(token)){token<-input.token}
  if(missing(forcerun)){forcerun=FALSE}
  if(!missing(replace)){funbsrc<-replace}
  if (exists('jzc.connection.date')==FALSE | exists('jzc.connection.date')==FALSE){
    jzc.connection.yesno<-0 
    jzc.connection.date<-NA}
  if (jzc.connection.yesno == 1) {
    if (forcerun==TRUE | jzc.connection.date==Sys.Date()) {
      print("Database is loaded or was loaded today")
      ifrun<-TRUE
    }
    else {print("Local database is out of date, redownload now")
      bsrc.conredcap(token = token)
      bsrc.checkdatabase()}
  }
  else {print("RedCap Connection is not loaded, Retry Now")
    bsrc.conredcap(token = token)
    bsrc.checkdatabase()}
  
  return(ifrun)
}

#refresh
bsrc.refresh<-function (forcerun,token) {
  if (missing(token)){token<-input.token}
  ifrun<-bsrc.checkdatabase(forcerun = forcerun,token = token)
  if (ifrun){
    subreg<-funbsrc[funbsrc$redcap_event_name=="enrollment_arm_1",] #take out only enrollment for efficiency
    subreg$curage<-as.period(interval(start = as.Date(subreg$registration_dob), end = Sys.Date()))$year #Get age
    subreg$as.period(interval(start = as.Date(subreg$registration_consentdate), end = Sys.Date())) #get since date 
    as.period(interval(start = as.Date(subreg$registration_consentdate), end = Sys.Date()))$year #get yrs
    as.period(interval(start = as.Date(subreg$registration_consentdate), end = Sys.Date()))$month #get month
    #find max fudate:
    maxfudate<-aggregate(na.exclude(as.Date(funbsrc$fudemo_visitdate)),by=list(funbsrc$registration_redcapid[!is.na(funbsrc$fudemo_visitdate)]),max)
    names(maxfudate)<-c("registration_redcapid","fudemo_visitdate")
    #find max fuevent:
    subevent<-subset(funbsrc,select = c("registration_redcapid","redcap_event_name","fudemo_visitdate"))
    maxevent<-subevent[match(interaction(maxfudate$registration_redcapid,maxfudate$fudemo_visitdate),interaction(subevent$registration_redcapid,subevent$fudemo_visitdate)),]
    maxevent$months<-gsub("_months_.*$","",maxevent$redcap_event_name)
    maxevent$daysincefu<-as.numeric(Sys.Date()-as.Date(maxevent$fudemo_visitdate))

  }
}