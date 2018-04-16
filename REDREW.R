#---
#Title: "REDREW"
#Author: "Jiazhou Chen"
#Version: 1.7
#---
#[Task List]  
#0/1 Missingness check arm specific 
#0.5/1 Attach demo info for given list of IDs [NO NEED]
#0.5/1 function to bridge current and pass db

#Version 1.7 Changelog: 
  #Full revision of bsrc.connredcap(), which now is unstable. 

#Version 1.6 Changelog:
  #Universal function: bsrc.updatedb() 
    #Deal with updating information in one df using info in another.
  
#Version 1.5 Changelog:
  #Introduction of universal function: bsrc.checkbox() 
    #Deal with checkbox items
  
#Version 1.4 Changelog: 
  #bsrc.getidmatchdb() now has a way more elegant way of getting redcap ID, read for upload in the future. 
  #bsrc.getmwidentifier() is the function that help getting mwidentifier to a database 
  
#Version 1.3 Changelog:
  #bsrc.getform() and bsrc.getevent() have a better aggressive datasubsetting rule
  #bsrc.getform() has new argument on how agressive it should be and if datamodification should happen.
  #bsrc.conredcap() configured to subset subreg
  
#version 1.2 Changelog:
  #Introduction of universal function: redcap.eventmapping
    #Very useful fucntion in longitudinal study.
  #Refinement of bsrc.getform(): 
    #Now using the new event mapping instead of relying on aggresive subsetting.
    #Aggressive subsetting is automatically off to preserve data. It's still recommand to turn on for simplicity.
  #New function: bsrc.getevent()
    #Subset the database to only certain event and their according forms
    #Aggressive subsetting is automatically off to preserve data. DO NOT recommand to turn on, only there for efficiency. 
  
#Version 1.1.1 Changelog:
  #fixed an error in bsrc.redcapcon which result in subreg only taking 1:50 variables. Insterad now it's dynamic
  
#Version 1.1 Changelog:
  #reformed bsrc.getform() with following changes:
    #aggressive argument to aggressively remove irrelavent data, by default on
    #Mechnisim to protect function from error
    #Error message if no form found
    #intergration with bsrc.ema.redcapreform()
    #New mechnisim to match variable names, migth sometimes result in errorous collection of variable names
      #benefit is that it will now include all checkbox items correctly. This is more important. 
      #Since removing irrelavent ones are not so difficult. 
  #Refined bsrc.getevent() and bsrc.getdemo()
    #bsrc.getevent() functional again 
    #bsrc.getdemo() has new arguments and compatible with findduplicate 
  #New bsrc.findduplicate() function to identify duplicated records in RedCap caused by ID transition
  
#Version 1.0 [Completed]
#1/1 Pull RedCap record into a whole datatable [pretty efficient in R, don't export, will break your pc]
#1/1 Pull Demo for given sinlge ID
#1/1 Check if environment has up to date local redcap database
#1/1 Get all data of given form
#1/1 Get RedCap ID for Soloff ID [V useful]
#1/1 Get MetircWire Identifier
#1/1 Get IRB Report Numbers (Total)
  #0/1 Get IRB Report Numbers, since last IRB Renewal -> SEE ADMINISTRATOR project
#1/1 Race/Gender/Status Processing 
#1/1 Missingness check ID specific

#------------Notes-------------
#might be useful:
#string as code:
#eval(parse(text='test'))
#find string to replace:
#gsub("^.*?test.single.","",k)
#gsub("_months_.*$","",maxevent$redcap_event_name)

#- Event variable name: "redcap_event_name"

###############Get Event Mapping from RedCap:
redcap.eventmapping<-function (redcap_uri, token, arms = NULL, message = TRUE, config_options = NULL) {
  start_time <- Sys.time()
  if (missing(redcap_uri)) 
    stop("The required parameter `redcap_uri` was missing from the call to `redcap.eventmapping`.")
  if (missing(token)) 
    stop("The required parameter `token` was missing from the call to `redcap.eventmapping`.")
  token <- sanitize_token(token)
  post_body <- list(token = token, content = "formEventMapping", format = "csv", arms = arms)
  result <- httr::POST(url = redcap_uri, body = post_body, 
                       config = config_options)
  status_code <- result$status
  success <- (status_code == 200L)
  raw_text <- httr::content(result, "text")
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (success) {
    try(ds <- utils::read.csv(text = raw_text, stringsAsFactors = FALSE), 
        silent = TRUE)
    if (exists("ds") & inherits(ds, "data.frame")) {
      outcome_message <- paste0("The data dictionary describing ", 
                                format(nrow(ds), big.mark = ",", scientific = FALSE, trim = TRUE), 
                                " fields was read from REDCap in ", 
                                round(elapsed_seconds, 1), " seconds.  The http status code was ", 
                                status_code, ".")
      raw_text <- ""}
    else {success <- FALSE
      ds <- data.frame()
      outcome_message <- paste0("The REDCap metadata export failed.  The http status code was ", 
                                status_code, ".  The 'raw_text' returned was '", 
                                raw_text, "'.")}
  }
  else {
    ds <- data.frame()
    outcome_message <- paste0("The REDCapR metadata export operation was not successful.  The error message was:\n", 
                              raw_text)}
  if (message){ 
    message(outcome_message)}
  return(list(data = ds, success = success, status_code = status_code, 
              outcome_message = outcome_message, elapsed_seconds = elapsed_seconds, 
              raw_text = raw_text))
}
###############Connect RedCap db for processing:
bsrc.conredcap <- function(uri,token,batch_size,output=F,notfullupdate=F) {
  if (missing(uri)) {uri<-'DNPL'
  print("By default, the location is set to Pitt's RedCap.")}
  if (missing(batch_size)) {batch_size<-"50" 
  print("By default, the batch size is 50 unique records")}
  if (uri == 'DNPL'|uri == 'PITT') {input.uri='https://www.ctsiredcap.pitt.edu/redcap/api/'}
  else (input.uri<-uri)
  if (missing(token)) {input.token <- readline(prompt = "Please input the RedCap api token: ")}
  if (!output){
    uri<<-input.uri
    token<<-input.token
    #test connection:
    funstrc<<-redcap_metadata_read(redcap_uri = input.uri,token = input.token)$data
    funevent<<-redcap.eventmapping(redcap_uri = input.uri,token = input.token)$data
  }
  if (!notfullupdate){
  redcap<-redcap_project$new(redcap_uri=input.uri, token=input.token)
  funbsrc.x<-redcap$read(batch_size = batch_size)
  if (funbsrc.x$success) {
    print("Success! Database Loaded")
    jzc.connection.yesno<<-1
    jzc.connection.date<<-Sys.Date()
    funbsrc<<-funbsrc.x$data
    subreg<<-bsrc.getevent(eventname = "enrollment_arm_1",forcerun = T,subreg = T)
    } #take only the regi part
  else {
    print("Connection Failed, Please Try Again.") 
    jzc.connection.yesno<<-0}
  if (output==T){
  return(funbsrc)}
  }
}
##############################Check Date Base
bsrc.checkdatabase<-function(replace,forcerun=F, token, forceupdate=F) {
  if(missing(token)){token<-input.token}
  if(!missing(replace)){funbsrc<-replace}
  if (exists('jzc.connection.yesno')==FALSE | exists('jzc.connection.date')==FALSE){
    jzc.connection.yesno<-0 
    jzc.connection.date<-NA}
  if (is.null(jzc.connection.date) | is.null(jzc.connection.yesno)){
    jzc.connection.yesno<-0 
    jzc.connection.date<-NA}
  if (forceupdate==TRUE) {
    print("FORCEUPDATE")
    bsrc.conredcap(token = token)
    ifrun<-TRUE
  }
  else {ifelse (jzc.connection.yesno == 1, {
      ifelse(forcerun==TRUE | jzc.connection.date==Sys.Date(), {
        print("Database is loaded or was loaded today")
        ifrun<-TRUE}, {print("Local database is out of date, redownload now")
        ifrun<-FALSE
        bsrc.conredcap(token = token)
        ifrun<-bsrc.checkdatabase()})
        }, 
      {print("RedCap Connection is not loaded, Retry Now")
      ifrun<-FALSE
      bsrc.conredcap(token = token)
      ifrun<-bsrc.checkdatabase()
      })
  }
  return(ifrun)
}
###############################
# use the info intergraded in redcap for more elegant solution:
# fundsrc$timeretrived
#Need to be more useful
bsrc.getdemo <- function(id,flavor="single",forcerun=FALSE,replace,output=T){
  if (missing(replace)){replace=F} else {replace->funbsrc} 
  ifrun<-bsrc.checkdatabase(forcerun = forcerun)
  if (ifrun==TRUE){
    if (flavor == 'single'){
      if(missing(id)){idt<-readline(prompt = "Please enter the participant's 6 digits or 4 digits ID: ")}else{idt<-id}
      idmatch<-data.frame(funbsrc$registration_id,funbsrc$registration_soloffid,funbsrc$registration_redcapid)
      names(idmatch)<-c('id','soloffid','redcapid')
      if(any(idt %in% idmatch$soloffid | idt %in% idmatch$id)){
        ifelse(idt %in% idmatch$soloffid, rid<-as.character(idmatch$redcapid[which(idmatch$soloffid==idt)]), 
               rid<-as.character(idmatch$redcapid[which(idmatch$id==idt)]))
        if(length(rid)==1){
        idonly<-funbsrc[which(funbsrc$registration_redcapid==rid & funbsrc$redcap_event_name=='enrollment_arm_1'),]
        #ID, Names, 
        curage<-as.period(interval(start = as.Date(idonly$registration_dob), end = Sys.Date()))$year
        bsg<-data.frame(idonly$registration_id,idonly$registration_soloffid,idonly$registration_initials,
                        idonly$registration_dob,idonly$registration_consentdate,curage)
        names(bsg)<-c('ID',"Soloffid","Initials","Date of Birth","Consent Date" ,"AgeToday")
        if (output==T){
        print(as.character('======================'))
        print(bsg)}
        }
        else {print("Multiple RedCap Record Identified")
              return(rid)}
      }
      else {print("NO ID FOUND, PLEASE DOUBLE CHECK")}
    }}
}
####Find duplicate RedCap IDs
bsrc.findduplicate <- function() {
    dpqid<-data.frame()
    for (i in 1:length(unique(funbsrc$registration_soloffid)) ) {
      tryCatch({
    idq<-unique(funbsrc$registration_soloffid)[i]
    invisible(capture.output(krz<-bsrc.getdemo(id=idq,output = F)))
    if(length(krz)>1){print(idq)
      print(krz)}
     },error=function(x){})
    print(i)
      }
    print("DONE")
}
################# Universal Function to deal with checkbox items:
bsrc.checkbox<-function(x,variablename = "registration_race",returndf = T) {
  raceonly<-x[grep(paste(variablename,"___",sep = ""),names(x))]
  options<-gsub(paste(variablename,"___",sep = ""),"",names(raceonly))
  x$xudjfnx<-apply(raceonly, 1, function(x) {which(x==1)})
  x$xudjfnx<-as.character(x$xudjfnx)
  x$xudjfnx[which(x$xudjfnx=="integer(0)")]<-NA
  x$knxmncua<-sapply(x$xudjfnx,function(x) {options[eval(parse(text= na.omit(x) ))]})
  x$knxmncua[which(x$knxmncua=="character(0)")]<-NA  
  names(x$knxmncua)<-NULL
  x$xudjfnx<-as.character(lapply(x$xudjfnx, function(x) {paste(eval(parse(text=na.omit(x))),collapse = ",")}))
  
  if (returndf) {
    colnames(x)[grep("knxmncua",names(x))]<-variablename
    colnames(x)[grep("xudjfnx",names(x))]<-paste(variablename,"_string",sep = "")
    return(x)}
  else {return(list(Checkbox_text=x$xudjfnx,Checkbox_list=x$knxmncua))}
}
#######
#Combined use of the following allow extraction of data within EVENT and FORM
############################
#Function to get all data of given event:
bsrc.getevent<-function(eventname,replace,forcerun=FALSE, whivarform="default",nocalc=T,uri.e,token.e,subreg=F,mod=F,aggressivecog=1){
  ifrun<-bsrc.checkdatabase(forcerun = forcerun)
  if (missing(replace)){replace=F} else {replace->funbsrc} 
  if(ifrun) {
    if (missing(eventname)){
      print(as.character(unique(funbsrc$redcap_event_name)))
      eventname<-readline(prompt = "Please type in the event name, for ALL FU, use 'allfu': ")
    }
    if (eventname=="allfu") {
    eventname<-as.character(unique(funbsrc$redcap_event_name))[grep("months_follow",as.character(unique(funbsrc$redcap_event_name)))]
    }
    eventonly<-funbsrc[which(funbsrc$redcap_event_name %in% eventname),]
    switch(whivarform,
    default = ifelse(!exists("funevent") | is.null(funevent),funevent<-redcap.eventmapping(redcap_uri = uri.e,token = token.e),print("GOT IT")),
    anyfile = funevent<-read.csv(file.choose())
    )
    formname<-funevent$form[funevent$unique_event_name %in% eventname]
    if (subreg) {formname<-formname[-grep(paste("ipde","scid",sep = "|"),formname)]}    
    variablename<-names(bsrc.getform(formname = formname,forcerun.e = T))
    eventonly.r<-eventonly[,grep(paste(variablename,collapse = "|"),names(eventonly))]
    
    if (mod) {print("By default, NA will replace '' and 0 in checkbox items")
      eventonly.r[eventonly.r==""]<-NA
      if (length(grep("___",names(eventonly.r))) > 0){
        eventonly.r[,grep("___",names(eventonly.r))][eventonly.r[,grep("___",names(eventonly.r))] == "0"]<-NA}
    }
    tempch<-funstrc[which(funstrc$form_name %in% formname),]
    if (nocalc){print("By default, will not take calculated field into consideration.")
      calmove<-length(which(tempch$field_type=="calc"))} else {calmove<-0}
    eventonly.x<-eventonly.r[rowSums(is.na(eventonly.r[,3:length(names(eventonly.r))])) < (length(names(eventonly.r))- (2+aggressivecog+calmove)),]
    
    return(eventonly.x)
  }
}
#####################################
#Functions to get all data from given forms: 
bsrc.getform<-function(formname,replace,forcerun.e=F,forceupdate.e=F,mod=T,aggressivecog=1, nocalc=T, grabnewinfo=F,redcap.uri= input.uri, token.e = input.token) {
  ifrun<-bsrc.checkdatabase(forcerun = forcerun.e, forceupdate = forceupdate.e)
  if (missing(replace)){replace=F} else {replace->funbsrc} 
  if (ifrun) {
  if (missing(formname)){
  print("Here's a list of forms: ")
  print(as.character(unique(as.character(funstrc$form_name))))
  formname<-readline(prompt = "Please type in one form name; if multiple, use ARGUMENT formname = c(,): ")
  }
  if (any(as.character(formname) %in% as.character(funstrc$form_name))) {
    if (grabnewinfo) {print("Updating form names")
      bsrc.conredcap(uri = input.uri,token = input.token,notfullupdate = T)} else {}
   lvariname<-as.character(funstrc$field_name[which(funstrc$form_name %in% formname)])
   raw<-funbsrc[,c(1,2,grep(paste(lvariname,collapse = "|"),names(funbsrc)))]
   eventname<-funevent$unique_event_name[which(funevent$form %in% formname)]
   raw<-raw[which(raw$redcap_event_name %in% eventname),]
   tempch<-funstrc[which(funstrc$form_name %in% formname),]
   if (nocalc){print("By default, will not take calculated field into consideration.")
   calmove<-length(which(tempch$field_type=="calc"))} else {calmove<-0}
   if (grabnewinfo) {print("Grab updated data from RedCap.")
     renew<-redcap_read(redcap_uri = redcap.uri ,token = token.e, fields = names(raw), events = raw$redcap_event_name)
     raw<-renew$data}
   if (mod) {print("By default, NA will replace '' and 0 in checkbox items")
      raw[raw==""]<-NA
      if (length(grep("___",names(raw))) > 0){
      raw[,grep("___",names(raw))][raw[,grep("___",names(raw))] == "0"]<-NA
      }}
    new_raw<-raw[rowSums(is.na(raw[,3:length(names(raw))])) < (length(names(raw))- (2+aggressivecog+calmove)),]
    return(new_raw)
    }
  else print(paste("NO FORM NAMED: ",formname, sep = ""))
    
  }
}
#########################
### MATACH FUNCTIONS ####
######################### Match RedCap ID to df
bsrc.getidmatchdb<-function(db,idfield="ID") {
  idmatch.k<-data.frame(funbsrc$registration_id,funbsrc$registration_soloffid,funbsrc$registration_redcapid)
  names(idmatch.k)<-c('id','soloffid','redcapid')
  
  if ((idfield %in% names(db))) {
    #GET ID FIELD
    if (idfield!="ID"){db$ID<-db[,grep(idfield,names(db))]}
      if(any(db$ID %in% idmatch.k$soloffid | db$ID %in% idmatch.k$id)){
        db$registration_redcapid<-NA
        db$registration_redcapid<-idmatch.k$redcapid[ifelse(db$ID %in% idmatch.k$soloffid,
                                                            match(db$ID,idmatch.k$soloffid),
                                                            match(db$ID,idmatch.k$id))]
        db$iftranx<-is.na(match(db$ID,idmatch.k$soloffid))      
        }
      else stop("NO ID MATCH FOUND,CHECK SOURCE")
    }
  else stop("NO ID COLUMN FOUND IN SPECIFIED DATAFRAME")
  
  if (idfield!="ID"){db$ID<-NULL}
  return(db)
}
########################## Match MericWire Identifier to df
bsrc.getmwidentifier<-function(db,only=F) {
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
########################## Update value from one database to another:
bsrc.updatedb<-function(ndf,df,by="registration_redcapid") {
  if (missing(df)) {df<-funbsrc}
  if (missing(ndf)) stop("HEY! NO REPLACEMENT DATEFRAME")
  excu<-paste("df[match(ndf$",by,"df$",by,"),match(names(ndf),names(df))]<-ndf",sep = "")
  eval(parse(text = excu))
  return(df)
}
########################### Assign aID
bsrc.assignaid<-function(df,idfieldname="redcapID",aidfieldname="aID",allinfo=T) {
  dfname<-as.character(substitute(df))
  exculine<-paste("idtack<-data.frame(unique(",dfname,"$",idfieldname,"),1:length(unique(",dfname,"$",idfieldname,")))",sep = "")
  eval(parse(text=exculine))
  names(idtack)<-c(idfieldname,aidfieldname)
  df<-merge(df,idtack,all = allinfo)
  return(df)  
}

###############################
####### IN DEVELOPMENT ########
###############################

if (FALSE){

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
}

###############################
####### SHINY######### ########
###############################
#Following is for Shiny Web App, 
#0/1 Single Participant Record Display

if (FALSE) {
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
}


