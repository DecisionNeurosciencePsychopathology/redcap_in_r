#########################
##  Project AUTOMATOR  ##  
##  Part I, Functions  ##
#########################

#This script use Mac's Automator and calendar event to automatically refresh the b-social RedCap Database
#Also contain function to generate csv calendar event (good for google) for EMA participants
#Version: 0.1
#This part of the script contains the main function:

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

bsrc.checkdatabase<-function(replace,forcerun) {
  if(missing(forcerun)){forcerun=FALSE}
  if(!missing(replace)){funbsrc<-replace}
  if (exists('jzc.connection.date')==FALSE | exists('jzc.connection.date')==FALSE){
    jzc.connection.yesno<-0 
    jzc.connection.date<-NA}
  if (jzc.connection.yesno == 1) {
    if (forcerun==TRUE | jzc.connection.date==Sys.Date()) {
      ifrun<-TRUE
    }
    else {print("Local database is out of date, redownload now")
      bsrc.conredcap()
      bsrc.checkdatabase()}
  }
  else {print("RedCap Connection is not loaded, Retry Now")
    bsrc.conredcap()
    bsrc.checkdatabase()}
  
  return(ifrun)
}


bsrc.refresh<-function (forcerun) {
  ifrun<-bsrc.checkdatabase(forcerun = forcerun)
  if (ifrun){
  }
}