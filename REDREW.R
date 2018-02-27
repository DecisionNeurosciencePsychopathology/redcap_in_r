---
Title: "REDREW"
Author: "Jiazhou Chen"
Version: 1.0
---

#Version 1.0
#1/1 Pull RedCap record into a whole datatable [pretty efficient in R, don't export, will break your pc]
#1/1 Pull Demo for given sinlge ID
#1/1 Check if environment has up to date local redcap database
#1/1 Get all data of given form
#1/1 Get RedCap ID for Soloff ID [V useful]
#1/1 Get MetircWire Identifier
#1/1 Get IRB Report Numbers (Total)
  #0/1 Get IRB Report Numbers, since last IRB Renewal
#1/1 Race/Gender/Status Processing 
#1/1 Missingness check ID specific
#0/1 Numbes for annual report
#0.5/1 Attach demo info for given list of IDs
#0/1 EMA end of study report generating
#0/1 Clean up funbsrc, #- Within ID, find event that doesn't have a FUDate, remove the whole event
#0/1 Missingness check arm specific 
#- Event variable name: "redcap_event_name"
#------------Over-arching goals------------
#0/1 Shiny App to create interactive data presentation and retrival tool

#------------Import of Legacy Data--------
#0/1 Try to export data frame to RedCap
#0/1 Match ID to ensure ID is not in RedCap
#0.5/1 function to bridge current and pass db

#------------Notes-------------
#might be useful:
#string as code:
eval(parse(text='test'))
#find string to replace:
gsub("^.*?test.single.","",k)
gsub("_months_.*$","",maxevent$redcap_event_name)

##### Install Packages####
install.packages('REDCapR')
install.packages('shiny')

###RedCap connection#####

#pull all data
library(redcapAPI)
library(REDCapR)
library(RCurl)
library(lubridate)

#-------Make sure to remove below before publish----------------#

#-------END-----#

#example of how the data is structured:
#bsocial.all<-redcap.bsocial$read(batch=batch_size = '200')
#bsocial.enrollment<-bsocial.all$data[bsocial.all$data$redcap_event_name=="enrollment_arm_1",]


#prompt user input
#test<-as.character(readline(prompt = "Please Enter Assessment: "))

#######customized functions#######
#all function need to pre-define bsocial.all&redcap.bsocial 
# jk, no more, run bsrc.conredcap to download a local copy
##################################

##1, connection function, download a copy of redcap database for manipulation,## 
##so far so good with so many variable and 200 data, it's working good so far ##

##jk, with all new ppl, it can't handle it anymore##
##            default batch size to 50            ##

bsrc.conredcap <- function(uri,token,batch_size,output) {
  if (missing(uri)) {uri<-'DNPL'
  print("By default, the location is set to Pitt's RedCap.")}
  if (missing(batch_size)) {batch_size<-"50" 
  print("By default, the batch size is 50 unique records")}
  if (missing(output)) {output<-F
  print("By default, the database will be assigned to `funbsrc` as a data frame and returns nothing if wish to assign db to something, use arguement 'output = T'")}
  if (uri == 'DNPL'|uri == 'PITT') {input.uri='https://www.ctsiredcap.pitt.edu/redcap/api/'}
  else (input.uri<-uri)
  if (missing(token)) {input.token <- readline(prompt = "Please input the RedCap api token: ")}
  redcap<-redcap_project$new(redcap_uri=input.uri, token=input.token)
  uri<<-input.uri
  token<<-input.token
  #test connection:
  funbsrc<-redcap$read(batch_size = batch_size)
  strc<-redcap_metadata_read(redcap_uri = input.uri,token = input.token)
  funstrc<<-data.frame(strc$data$field_name,strc$data$form_name,strc$data$field_label)
  names(strc)<-c('field_name','form_name','field_label')
  funbsrc<<-funbsrc$data
  subreg<-funbsrc$data[funbsrc$data$redcap_event_name=="enrollment_arm_1",] #take out only enrollment for efficiency
  subreg<<-subreg[,1:50] #take only the regi part
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
##############################
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
        ifrun<-TRUE}
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



##############################
bsrc.findid.mass <- function (df, IDfieldname) {
  if (class(df)=="data.frame"){success<-T} else{print("I AM YELLING AT YOU BC IT'S NOT A DATABASE!!!!!")}
  if (missing(IDfieldname)){print("I ASSUMED YOUR ID FIELD NAME IS 'ID', OKAY?!")} {success<-T; IDfieldname<-"ID"}
  if (success) {
    
  }
  
}


##############################
bsrc.getlabel <- function(field_name) {
  
}
  
  ###############################
# use the info intergraded in redcap for more elegant solution:
# fundsrc$timeretrived
bsrc.getdemo <- function(id,flavor="single",forcerun=FALSE,replace){
  if (missing(replace)){replace=F} else {replace->funbsrc} 
  ifrun<-bsrc.checkdatabase(forcerun = forcerun)
  if (ifrun==TRUE){
    if (flavor == 'single'){
      if(missing(id)){idt<-readline(prompt = "Please enter the participant's 6 digits or 4 digits ID: ")}else{idt<-id}
      idmatch<-data.frame(funbsrc$registration_id,funbsrc$registration_soloffid,funbsrc$registration_redcapid)
      names(idmatch)<-c('id','soloffid','redcapid')
      if(any(idt %in% idmatch$soloffid | idt %in% idmatch$id)){
        if(idt %in% idmatch$soloffid){rid<-as.character(idmatch$redcapid[which(idmatch$soloffid==idt)])}
        else{rid<-as.character(idmatch$redcapid[which(idmatch$id==idt)])}
        idonly<-funbsrc[which(funbsrc$registration_redcapid==rid & funbsrc$redcap_event_name=='enrollment_arm_1'),]
        #ID, Names, 
        curage<-as.period(interval(start = as.Date(idonly$registration_dob), end = Sys.Date()))$year
        bsg<-data.frame(idonly$registration_id,idonly$registration_soloffid,idonly$registration_initials,
                        idonly$registration_dob,idonly$registration_consentdate,curage)
        names(bsg)<-c('ID',"Soloffid","Initials","Date of Birth","Consent Date" ,"AgeToday")
        
        
        
        #output
        print(as.character('======================'))
        print(bsg)
      }
      else {print("NO ID FOUND, PLEASE DOUBLE CHECK")}
    }}
}
#######
#Combined use of the following allow extraction of data within
############################
#Function to get all data of given event:

bsrc.getevent<-function(eventname,replace,forcerun=FALSE, whivarform="default"){
  ifrun<-bsrc.checkdatabase(forcerun = forcerun)
  if (missing(replace)){replace=F} else {replace->funbsrc} 
  if(ifrun) {
    if (missing(eventname)){
      print(as.character(unique(funbsrc$redcap_event_name)))
      eventname<-readline(prompt = "Please type in the event name that you wish to extract (use argument eventname=c('','') for multiple): ")}
    funbsrc$redcap_event_name[which(funbsrc$redcap_event_name %in% eventname)]
    switch(whivarform,
    default=getvariable<-read.csv("/Users/jiazhouchen/Box Sync/skinner/projects_analyses/Project BPD Longitudinal/BPD Database/JC/RE/BSocial_InstrumentDesignations.csv"),
    user=getvariable<-read.csv(readline(prompt = "Please paste the path of the vari file, inclduing the file name in .csv form: ")),
    anyfile=getvariable<-read.csv(file.choose())
    )
    return(getvariable)
    
  }
}

  
#####################################
#Functions to get all data from given forms: 
bsrc.getform<-function(formname,replace,forcerun=FALSE) {
  ifrun<-bsrc.checkdatabase(forcerun = forcerun)
  if (missing(replace)){replace=F} else {replace->funbsrc} 
  if (ifrun) {
  if (missing(formname)){
  print(as.character(unique(funstrc$strc.data.form_name)))
  formname<-readline(prompt = "Please type in one form name that you wish to get, if multiple, use argument formname = c('',''): ")
  }
  raw<-funbsrc[,c(1,2,which(names(funbsrc) %in% as.character(funstrc$strc.data.field_name[which(as.character(funstrc$strc.data.form_name) %in% formname)])))]
  new_raw<-raw[rowSums(is.na(raw[,3:length(names(raw))])) < (length(names(raw))-3),]
  return(new_raw)
  }
}
  
#############################


##############################
## Notes when exporting db  ##
## Red Cap                  ##
##############################

bsrc.getidmatchdb<-function(db) {
  idmatch.k<-data.frame(funbsrc$registration_id,funbsrc$registration_soloffid,funbsrc$registration_redcapid)
  names(idmatch.k)<-c('id','soloffid','redcapid')
  if (("ID" %in% names(db))) {
    db$registration_redcapid<-idmatch.k$redcapid[match(db$ID,idmatch.k$soloffid)]
    db$iftranx<-is.na(match(db$ID,idmatch.k$soloffid))
    }
  else stop("NO ID COLUMN FOUND IN SPECIFIED DATAFRAME")
  
  return(db)
}


bsrc.sanitize_onlyidentifier<-function(db,only=F) {
  if(missing(db)) stop("No DB")
  mwidonly<-data.frame(funbsrc$registration_redcapid,funbsrc$ema_studyidentifier)
  names(mwidonly)<-c('redcapid','mwidentifier')
  mwidonly$mwidentifier<-as.character(mwidonly$mwidentifier)
  mwidonly$mwidentifier[which(mwidonly$mwidentifier=="")]<-NA
  mwidonly<-na.omit(mwidonly)
  db$mwidentifier<-mwidonly$mwidentifier[match(db$registration_redcapid,mwidonly$redcapid)]
  if (only) {db<-db[which(!is.na(db$mwidentifier)),]}
  return(db)
}


bsrc.process.race<-function(odk,Race) {
  
  for (i in 1:length(odk$ID)) {
    if  (is.na(odk$Race[i])) {
      odk$registration_race___1[i]<-NA
      odk$registration_race___2[i]<-NA
      odk$registration_race___3[i]<-NA
      odk$registration_race___4[i]<-NA
      odk$registration_race___5[i]<-NA
      odk$registration_race___999[i]<-NA
    }
    else {
      if  (odk$Race[i]==1) {
        odk$registration_race___1[i]<-1
        odk$registration_race___2[i]<-0
        odk$registration_race___3[i]<-0
        odk$registration_race___4[i]<-0
        odk$registration_race___5[i]<-0
        odk$registration_race___999[i]<-0
      }
      
      if  (odk$Race[i]==2) {
        odk$registration_race___1[i]<-0
        odk$registration_race___2[i]<-1
        odk$registration_race___3[i]<-0
        odk$registration_race___4[i]<-0
        odk$registration_race___5[i]<-0
        odk$registration_race___999[i]<-0
      }
      
      if  (odk$Race[i]==3) {
        odk$registration_race___1[i]<-0
        odk$registration_race___2[i]<-0
        odk$registration_race___3[i]<-1
        odk$registration_race___4[i]<-0
        odk$registration_race___5[i]<-0
        odk$registration_race___999[i]<-0
      }
      if  (odk$Race[i]==4) {
        odk$registration_race___1[i]<-0
        odk$registration_race___2[i]<-0
        odk$registration_race___3[i]<-0
        odk$registration_race___4[i]<-1
        odk$registration_race___5[i]<-0
        odk$registration_race___999[i]<-0
      }
      if  (odk$Race[i]==5) {
        odk$registration_race___1[i]<-0
        odk$registration_race___2[i]<-0
        odk$registration_race___3[i]<-0
        odk$registration_race___4[i]<-0
        odk$registration_race___5[i]<-1
        odk$registration_race___999[i]<-0
      }
      if  (odk$Race[i]==6) {
        odk$registration_race___1[i]<-1
        odk$registration_race___2[i]<-1
        odk$registration_race___3[i]<-1
        odk$registration_race___4[i]<-1
        odk$registration_race___5[i]<-1
        odk$registration_race___999[i]<-0
      }
      if  (odk$Race[i]==7) {
        odk$registration_race___1[i]<-0
        odk$registration_race___2[i]<-0
        odk$registration_race___3[i]<-0
        odk$registration_race___4[i]<-0
        odk$registration_race___5[i]<-0
        odk$registration_race___999[i]<-1
      }
    }
  }
}

###################################
######  Admin Number Cal ##########
###################################



#Following is for Shiny Web App, 
#0/1 Single Participant Record Display


#This chunk is for shiny web app
library(shiny)

#Will run the database here and generate informaiton in a dataframe
#Get code from chuck 2

#Define UI here:
ui <- fluidPage(
  titlePanel("B-Social Single Participant "),
  
  sidebarLayout(
    numericInput(inputId = "id", label = h3("Please input their 6 digits ID"), value = 1),
    helpText("Note: help text isn't a true widget,", 
             "but it provides an easy way to add text to",
             "accompany other widgets."),
    actionButton(inputId = "changeid", "Submit")),
  
  mainPanel(tableOutput("view"))
)

# Define server logic here:
server <- function(input, output) {
  
  randomVals <- eventReactive(input$changeid, {
    runif(input$id)
  })
  
  output$plot <- renderPlot({
    hist(randomVals())
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)