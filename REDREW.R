---
title: "REDREW"
author: "Jiazhou Chen"
---
#Current Version: 0.1

#This R Notebook records Jiazhou's effort to pull/input RedCap data.


#Version 0.1:
#0/0 Pull RedCap record into separate csv files (Not sure if that's the most efficient way; it's not)
#1/1 Pull RedCap record into a whole datatable [pretty efficient in R, don't export, will break your pc]
#0/1 Elimination Function: only take arms that has data if all 
#- Within ID, find event that doesn't have a FUDate, remove the whole event
#0/1 Missingness check arm specific
#- Event variable name: "redcap_event_name"
#-------------- Data Function ------------- c
#0/1 Audit & basic demographic 
#0/1 EMA & fMRI status
#0/1 Match list of ID & list of assessments
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
bsocial.all<-redcap.bsocial$read(batch=batch_size = '200')
bsocial.enrollment<-bsocial.all$data[bsocial.all$data$redcap_event_name=="enrollment_arm_1",]


#prompt user input
test<-as.character(readline(prompt = "Please Enter Assessment: "))

#######customized functions#######
#all function need to pre-define bsocial.all&redcap.bsocial 
# jk, no more, run bsrc.conredcap to download a local copy
##################################

##1, connection function, download a copy of redcap database for manipulation,## 
##so far so good with so many variable and 200 data, it's working good so far ##

##jk, with all new ppl, it can't handle it anymore##
##            default batch size to 50            ##

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
  strc<-redcap_metadata_read(redcap_uri = input.uri,token = input.token)
  strc<<-data.frame(strc$data$field_name,strc$data$form_name,strc$data$field_label)
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

bsrc.getinst <- function(form_name)
  
  ###############################
# use the info intergraded in redcap for more elegant solution:
# fundsrc$timeretrived
bsrc.getdemo <- function(id,funbsrc,flavor,forcerun=FALSE){
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

############################


bsrc.getevent.single<-
  
  bsrc.getform.single<-
  
#############################



###########################

bsrc.report<-function(flavor,forcerun=FALSE){
  if (exists('jzc.connection.date')==FALSE | exists('jzc.connection.date')==FALSE){
    jzc.connection.yesno<-0 
    jzc.connection.date<-NA}
  if (jzc.connection.yesno == 1) {
    
    if (forcerun==TRUE | jzc.connection.date==Sys.Date()) {
      if (missing(flavor)){flavor<-readline(prompt = "Please input report type: ")}
      ifrun<-TRUE
    }
    else {print("Local database is out of date, redownload now")
      bsrc.conredcap()
      bsrc.report(flavor)}
  }
  else {print("RedCap Connection is not loaded, Retry Now")
    bsrc.conredcap()
    bsrc.report(flavor)}
  
  
  if (ifrun==T){
    if (flavor == 'renewal'|flavor == 'Renewal') {print("Renewal Report")
    }
    if (flavor == 'monthly'|flavor == 'Monthly') {print("Monthly Report")
    }
  }
}  

##############################
## Notes when exporting db  ##
## Red Cap                  ##
##############################

opu$registration_initials <- paste(toupper(substr(opu$`First Name`,0,1)),toupper(substr(opu$`Last Name`,0,3)))
opu$registration_initials[which(opu$registration_initials=="NA NA")]<-"NA"
odz$registration_redcapid<-idmatch$redcapid[match(odz$ID,idmatch$soloffid)]
odz$iftranx<-is.na( match(odz$ID,idmatch$soloffid))

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

bsrc.writerecord<-function(flavor,) {}



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
bsrc.irb.numsum<-function() {
  ID_SUPREME <- read_excel("Box Sync/skinner/projects_analyses/Project BPD Longitudinal/BPD Database/JC/RE/ID_ SUPREME.xlsx")
  ID_SUPREME[,5:8]<-NULL
  tkj<-bsrc.getidmatchdb(ID_SUPREME)
  tkj<-as.data.frame(tkj)
  newid<-as.data.frame(subreg$registration_redcapid[! subreg$registration_redcapid %in% tkj$registration_redcapid])
  names(newid)<-c("registration_redcapid")
  jrk<-merge(tkj,newid,all = T)
  nui<-subset(subreg,select = c("registration_redcapid","registration_status","registration_soloffid"))
  nui<-merge(jrk,nui,all = T)
  if (length(nui$Status[which(!nui$Status==nui$registration_status)])>0) {
        #Info user the conflict:
        return(as.data.frame(nui$registration_redcapid[which(!nui$Status==nui$registration_status)],nui$Status[which(!nui$Status==nui$registration_status)],nui$registration_status[which(!nui$Status==nui$registration_status)]))
        #which direction:
        direct.r<-readline(prompt = "Please type 'RC' for picking RedCap Status, or 'OG' for picking legacy status: ")
        direct.r<-as.numeric(direct.r)
        switch (direct.r,RC = nui$Status[which(!nui$Status==nui$registration_status)]<-nui$registration_status[which(!nui$Status==nui$registration_status)],
                OG = nui$Status[which(!nui$Status==nui$registration_status)]->nui$registration_status[which(!nui$Status==nui$registration_status)])}
  nui$Status[which(is.na(nui$Status))]<-nui$registration_status[which(is.na(nui$Status))]
  nui$iftranx<-NULL
  nui$StatusWord[nui$Status==88]<-"Ineligible Drop"
  nui$StatusWord[nui$Status==7]<-"IRB Admin Drop"
  nui$StatusWord[nui$Status==6]<-"Lost Contact/Drop"
  nui$StatusWord[nui$Status==5]<-"Deceased"
  nui$StatusWord[nui$Status==4]<-"Do Not Contact"
  nui$StatusWord[nui$Status==3]<-"In Jail"
  nui$StatusWord[nui$Status==2]<-"Missing"
  nui$StatusWord[nui$Status==1]<-"Active"

}



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




############
#Past Code:#
############

  #main function:
  result <- postForm(
    uri='https://www.ctsiredcap.pitt.edu/redcap/api/',
    token='F4D36C656D822DF09832B5A4A8F323E6',
    content='record',
    format='csv',
    type='flat',
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='csv'
  )

#Special pull of specific assessment:
result <- postForm(
  uri='https://www.ctsiredcap.pitt.edu/redcap/api/',
  token='F4D36C656D822DF09832B5A4A8F323E6',
  content='record',
  format='csv',
  type='flat',
  'records[0]'='6510',
  'records[1]'='6515',
  'forms[0]'='hamd24',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='csv'
)
print(result)

#Get names
jrs<-NULL
jrs<-data.frame(bsocial.enrollment$registration_id,bsocial.enrollment$registration_soloffid,
                bsocial.enrollment$registration_ln,bsocial.enrollment$registration_fn,
                paste(bsocial.enrollment$registration_fn,bsocial.enrollment$registration_ln))
names(jrs)<-c("ID","Soloff ID","Last Name","First Name","Full Name")
write.csv(jrs,'bsocial_idname.csv')

bsrc.getdemo.single.a <- function(flavor){
  idt<-readline(prompt = "Please enter the participant's 6 digits or 4 digits ID: ")
  idmatch<-redcap.bsocial$read(batch_size = '200', fields = c("registration_soloffid","registration_id"))
  idmatch<-data.frame(idmatch$data$registration_id,idmatch$data$registration_soloffid)
  names(idmatch)<-c('id','soloffid')
  if(idt %in% idmatch$id){id<-idt}else{id<-as.character(na.omit(idmatch$id[idmatch$soloffid==idt]))}
  id<-as.character(id)
  idonly<-redcap.bsocial$read(batch_size = '200', records = as.list(id))
  bsg<-data.frame(id,idonly$data$registration_soloffid,idonly$data$registration_initials)
  names(bsg)<-c('ID',"Soloffid","Initials")
  print(bsg)
}

