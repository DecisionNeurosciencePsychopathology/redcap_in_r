#---
#Title: "REDREW"
#Author: "Jiazhou Chen"
#Version: 2.2
#---
#[Task List]  
#0/1 Missingness check arm specific 
#0.5/1 Attach demo info for given list of IDs [NO NEED]
#0.5/1 function to bridge current and pass db
#Version 2.2 Changelog:
  #bsec.getchoicemapping() get the choice from metadata object which can be updated according to design, no more fixed codessss
  #General bug fix to redcap connection/check redcap; so that the refresh function can run

#Version 2.1 Changelog: 
  #Some new functions to help backward compatibility and efficiency
  #New version of the bsrc.attachngrab() deals with the new data organization method
  #Updated functions to incooperate changes in Version 2.0 

#Version 2.0 Changelog: [Major Revision]
  #New data orgnization method to the funbsrc for more effective update method and make cross project data migration possible
  #BRAND NEW MECHANISM FOR IMPORTING DATA; NOW COULD BE USED TO AUTOMATE DATA IMPORT FROM THE BACKGROUD YOOOOOOO
  #Universal function: bsrc.updatedb() 
    #Deal with updating information in one df using info in another.
  #Introduction of universal function: bsrc.checkbox() 
    #Deal with checkbox items
  #bsrc.getidmatchdb() now has a way more elegant way of getting redcap ID, read for upload in the future. 
  #bsrc.getmwidentifier() is the function that help getting mwidentifier to a database 
  #bsrc.getform() and bsrc.getevent() have a better aggressive datasubsetting rule
  #bsrc.getform() has new argument on how agressive it should be and if data modification should happen.
  #Introduction of universal function: redcap.eventmapping()
    #Very useful fucntion in longitudinal study.
  #New function: bsrc.getevent()
    #Subset the database to only certain event and their according forms
    #Aggressive subsetting is automatically off to preserve data. DO NOT recommand to turn on, only there for efficiency. 
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
  #New bsrc.findduplicate() function to identify duplicated records in RedCap caused by ID transition
  #Pull RedCap record into a whole datatable [pretty efficient in R, don't export, will break your pc]
  #Pull Demo for given sinlge ID
  #Check if environment has up to date local redcap database
  #Get all data of given form
  #Get RedCap ID for Soloff ID [V useful]
  #Get MetircWire Identifier
  #Race/Gender/Status Processing 
  #Missingness check ID specific

#------------Notes-------------
#might be useful:
#string as code:
#eval(parse(text='test'))
#find string to replace:
#gsub("^.*?test.single.","",k)
#gsub("_months_.*$","",maxevent$redcap_event_name)

#- Event variable name: "redcap_event_name"
cleanuplist<-function(listx){
  if (any(sapply(listx, is.null))){
    listx[sapply(listx, is.null)] <- NULL}
  return(listx)
}
###############Get Event Mapping from RedCap:
redcap.eventmapping<-function (redcap_uri, token, arms = NULL, message = TRUE, config_options = NULL) {
  start_time <- Sys.time()
  if (missing(redcap_uri)) 
    stop("The required parameter `redcap_uri` was missing from the call to `redcap.eventmapping`.")
  if (missing(token)) 
    stop("The required parameter `token` was missing from the call to `redcap.eventmapping`.")
  token <- REDCapR::sanitize_token(token)
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
##########################Switcher
bsrc.switcher<-function(name=NULL,redcap_uri=NULL,token=NULL,rdpath=NULL,protocol.cur=F,
                        regiformname=NULL,forcenewsubinsync=NULL){
  #This is used to switch protocols [hard coding lab protocls]
   if (!is.null(name) & !is.null(redcap_uri) & !is.null(token)){
    print("constructing new one...")
    protocol<-list(name=name,redcap_uri=redcap_uri,token=token,rdpath=rdpath)
   } else {print("Not enough info")}
  if (!is.null(regiformname)) {protocol$regiformname<-regiformname}
  if (!is.null(forcenewsubinsync)) {protocol$forcenewsubinsync<-forcenewsubinsync}
  if (protocol.cur){
    protocol.cur<<-protocol
  } else {return(protocol)}
}
#########################Release to global function
bsrc.globalrelease<-function(protocol=protocol.cur,skipcheck=F) {
  if (file.exists(protocol$rdpath)){
    if (!skipcheck){curdb<-bsrc.checkngrab(protocol=protocol)}
  }else {print("File don't exist...loading")
    bsrc.checkdatabase2(protocol = protocol, glob.release = T)}
  updated.time<-curdb$update.time
  funbsrc<<-curdb$data
  funevent<<-curdb$eventmap
  funstrc<<-curdb$metadata
  jzc.connection.date<<-curdb$update.date
}
#########################Check & Attach
bsrc.attachngrab<-function(rdpath=NULL, protocol=protocol.cur, returnas="envir"){
  if (is.null(rdpath)) {
  if(is.list(protocol)) {protocol.n<-protocol$name
    rdpath<-protocol$rdpath
  } else {stop("ERROR, protocol object is not a list.")}
  } else {print("when rdpath argument available, will always use that")}
  
  lsattach<-grep(rdpath,search())
  if (length(lsattach)>0){ #this chuck removes any active 'attach' of the same file
    for (i in 1:length(lsattach)){
      lsattach.s<-grep(rdpath,search())[1]
      detach(pos = lsattach.s)
    }}
  if(file.exists(rdpath)){
    print("Loading RDATA file....")
    envir.load<-attach(rdpath,warn.conflicts = F)
    detach()
    objectnames<-objects(envir = envir.load)
    list.load<-as.list(envir.load)
    
    switch (returnas,
      "envir" = {return(envir.load)},
      "list"  = {return(list.load)}
    )
    } else {"No such file...."}
}
#########################New Ver in DEV
bsrc.conredcap2<-function(protocol=protocol.cur,updaterd=T,batch_size="50",fullupdate=T,output=F,newfile=F,online=F,...) {
  if (missing(protocol)) {stop("no protocol specified")}
  if (!is.list(protocol)) {print("protocol has not sufficient information, using global variables [input.uri/input.token]")}
  if (is.list(protocol)) {print(paste("Got protocol list object, will load protocol: '",protocol$name,"' now...",sep = ""))
    print(protocol[ protocol != protocol$token ])
    protocol.n<-protocol$name
    input.uri<-protocol$redcap_uri
    input.token<-protocol$token
    rdpath<-protocol$rdpath
  }
  if (is.na(rdpath) | is.null(rdpath)) {online<-TRUE}
  if (!output & updaterd) {
  if (!online) {
  if (file.exists(rdpath) & !newfile) {
    pathsplit<-strsplit(rdpath,split = "/")[[1]]
    topath<-paste(paste(pathsplit[-length(pathsplit)],collapse = "/",sep = ""),"Backup","conredcap.backup.rdata",sep = "/")
    file.copy(from = rdpath, to = topath, overwrite = T)
    cur.envir<-bsrc.attachngrab(protocol = protocol, returnas = "envir")
  }else if (newfile | !file.exists(rdpath)) {"Starting new file..."
    cur.envir<-new.env(parent = emptyenv())
    allobjects<-c(protocol.n)
    fullupdate<-TRUE}
  } else {cur.envir<-new.env(parent = emptyenv())}
  } else {cur.envir<-new.env(parent = emptyenv())}
  anyfailed.s<-FALSE
  anyfailed.e<-FALSE
  anyfailed.d<-FALSE
  funstrc.x<-REDCapR::redcap_metadata_read(redcap_uri = input.uri,token = input.token)
  if (funstrc.x$success){
    funstrc<-funstrc.x$data
  }else{anyfailed.s<-TRUE
  print("Metadata not loaded")}
  funevent.x<-redcap.eventmapping(redcap_uri = input.uri,token = input.token)
  if (funevent.x$success){
    funevent<-funevent.x$data
  }else{anyfailed.e<-TRUE
  print("Event mapping not loaded")}
  if (fullupdate){
    funbsrc.x<-REDCapR::redcap_read(batch_size = batch_size,redcap_uri=input.uri, token=input.token)
    if (funbsrc.x$success){
      funbsrc<-funbsrc.x$data
    }else{anyfailed.d<-TRUE
    print("Main database not loaded")}
  }else {anyfailed.d<-TRUE}
  if (!any(anyfailed.s,anyfailed.e,anyfailed.d)){
    assign("update.date",Sys.Date(),envir = cur.envir)
    assign("update.time",Sys.time(),envir = cur.envir)
    assign("success",TRUE,envir = cur.envir)
  }else{
    print("something went wrong, better go check it out.")
    print("will still update successfully loaded parts.")
    assign("success",FALSE,envir = cur.envir)
  }
  #New way, use environment:
  if (!anyfailed.s){
    assign("metadata",funstrc,envir = cur.envir)
  }
  if (!anyfailed.e){
    assign("eventmap",funevent,envir = cur.envir)
  }
  if (!anyfailed.d){
    assign("data",funbsrc,envir = cur.envir)
  }
  if (updaterd & !online){
    save(list = objects(cur.envir),envir = cur.envir,file = rdpath)
  }
  assign("name",protocol$name,envir = cur.envir)
  if (output | online) {
    return(cur.envir)
  }
}
##############################Check Date Base
bsrc.checkdatabase2<-function(protocol = protocol.cur,forceskip=F, online=F, forceupdate=F, glob.release = F,logicaloutput=F, expiration=3,...) {
  reload<-FALSE
  ifrun<-TRUE
  protocol$rdpath->rdpath
  if(!online){
  if(file.exists(rdpath)){
    curdb<-invisible(bsrc.attachngrab(protocol=protocol,returnas = "envir"))
    if (is.null(curdb$update.time)){updated.time<-"2018-01-15 22:15:01 EST"}else {updated.time<-curdb$update.time}
  if(!forceskip){  
    if (curdb$success) {
      if (difftime(Sys.time(),updated.time,units = "hours") > expiration) {
        print(paste("Whelp...it's been more than ",expiration," hours since the db was updated, let's update it..."))
        reload<-TRUE} }else {print("Something went wrong when loading rdata file...")
      ifso<-readline(prompt = "To continue with the file, type 'T' or to reload type 'F' : ")
      if (!as.logical(ifso)){reload<-T}
      }
  
  }else{print("FORCE SKIP RDATA CHECKS")
  ifrun<-T}
  }else{print("No such file...reloading")
    reload<-T}
  } else {print("Online mode is on")
    reload<-F
    online<-T
    forceupdate<-F}
  
  if (reload | forceupdate) {
    bsrc.conredcap2(protocol = protocol,online=online,... = ...)
    bsrc.checkdatabase2(protocol = protocol,forceupdate = F,...)
  }else {ifrun<-TRUE}
  
  if (online) {
    curdb<-bsrc.conredcap2(protocol = protocol,online=online,... = ...)
    ifrun<-TRUE
  }
  
  if (glob.release) {
    bsrc.globalrelease(skipcheck = T)
  }
  if(ifrun & !logicaloutput) {return(curdb)}
  if(logicaloutput) {return(ifrun)}
}
###############Legacy
bsrc.conredcap<-function(uri,token,batch_size,output=F,notfullupdate=F) {
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
    } #take only the regi part
    else {
      print("Connection Failed, Please Try Again.") 
      jzc.connection.yesno<<-0}
    if (!output) {subreg<<-bsrc.getevent(eventname = "enrollment_arm_1",forcerun = T,subreg = T)}
    if (output){
      return(list(data=funbsrc,metadata=funstrc,eventmapping=funevent))}
  }
}
###############################
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
bsrc.findid<-function(df,idmap,id.var="ID",onlyoutput=NULL){
  t<-lapply(df[[id.var]],function(id) {
    pos<-as.data.frame(which(idmap==id,arr.ind = T))
    dx<-idmap[unique(pos$row),]
    if(length(dx[[1]])>0) {
      dx$ogid<-id
      dx$ifexist<-TRUE
      return(dx)
    }else{
      dk <- data.frame(matrix(ncol = length(names(idmap)), nrow = 1))
      names(dk)<-names(idmap)
      dk$ogid<-id
      dk$ifexist<-FALSE
      return(dk)
    }
  })
  tx<-do.call(rbind,t)
  if (!is.null(onlyoutput)){tx<-tx[c(onlyoutput)]}
  lx<-cbind(df,tx)
  return(lx)
}
#############
bsrc.refineupload<-function(dfx=NULL,id.var="registration_redcapid",perference="redcap",curdb=NULL,onlyrc=T){
  varstodo<-names(dfx)[!names(dfx) %in% c(id.var,"redcap_event_name")]
  varstodo<-varstodo[varstodo %in% curdb$metadata$field_name]
  dbx<-curdb$data
  metd<-curdb$metadata
  if (any(varstodo %in% metd$field_name[metd$field_type=="checkbox"])) {
    cbs<-varstodo[which(varstodo %in% metd$field_name[metd$field_type=="checkbox"])]
    for (cb in cbs) {
      dbx<-bsrc.checkbox(x = dbx,variablename = cb)
    }
  }
  for (vartodo in varstodo) {
    xk<-sapply(1:length(dfx[[id.var]]),function(i){
      as.character(dfx[i,id.var])->id
      as.character(dfx[i,"redcap_event_name"])->event
      dfx[[i,vartodo]]->x
      if (length(event)<1) {
        dbx[which(dbx[[id.var]]==id),vartodo]->xrc
        if (is.list(xrc)){
          xrc<-lapply(xrc,function(x){if(length(x)<1) {return(NULL)} else return(x)})
          xrc<-unlist(cleanuplist(xrc))
        }
        xrc<-xrc[!is.na(xrc)]
        xrc<-xrc[xrc!=""]
      } else {
        dbx[which(dbx[[id.var]]==id & dbx$redcap_event_name==event),vartodo]->xrc}
      
      if(length(xrc)<1 |is.null(xrc) |!any(!is.na(xrc))) {xrc<-NA
      } else if (length(xrc)>1 && any(is.na(xrc))) {xrc<-na.omit(xrc)}
      if(length(x)<1 |is.null(x) |!any(!is.na(x))) {x<-NA
      } else if (length(x)>1 && any(is.na(x))) {x<-na.omit(x)}
      
      
      if (any(is.na(xrc))) {return(x)}
      if (any(is.na(x))) {return(xrc)}
      if (length(xrc)!=length(x) | any(!xrc %in% x)){
        if (perference == "redcap") {return(xrc)}
        if (perference == "data") {return(x)}
        if (perference == "NA") {return(NA)}
      } else return(xrc)
      
    })
    #do duplicate action here:
    dfx[[vartodo]]<-xk
  }
  
  if(onlyrc) {
    if(any(is.null(dfx$redcap_event_name))) {dfx<-dfx[,c(id.var,varstodo)]} else {
      dfx<-dfx[,c(id.var,"redcap_event_name",varstodo)]}
  }
  dfx<-bsrc.choice2checkbox(dfx = dfx,metadata = metd)
  
  return(dfx)
}

##############

bsrc.choice2checkbox<-function(dfx=NULL,metadata=NULL,cleanupog=T){
  varstodo<-names(dfx)[which(metadata$field_type[match(names(dfx),metadata$field_name)] == "checkbox")]
  for (var in varstodo) {
    choicemap<-bsrc.getchoicemapping(variablenames = var,metadata = metadata)
    as.list(dfx[[var]])->lsvar
    lxvar<-lapply(lsvar,function(xa) {
      if (!any(!xa %in% choicemap$choice.code)) {
        xc<-xa
      } else if (!any(!xa %in% choicemap$choice.string)) {
        xc<-choicemap$choice.code[match(xa,choicemap$choice.string)]
      } else if (length(xa) < 2 && is.na(xa)){
        xc<-NA
      } else if (any(is.na(xa))) {
        xc<-na.omit(xa)
      }else {
        warning(paste0("variable ",var," contains unmatched value: ",xa,", will return NA"))
        xc<-NA
      }
      
      dk <- data.frame(matrix(ncol = length(choicemap$choice.code), nrow = 1,data = 0))
      names(dk)<-paste(var,choicemap$choice.code,sep = "___")
      if (!any(is.na(xc))) {
        dk[match(xc,choicemap$choice.code)]<-1
      }
      return(dk)
    })
    dfvar<-do.call(rbind,lxvar)
    dfx<-cbind(dfx,dfvar)
    if (cleanupog) {dfx[[var]]<-NULL}
  }
  return(dfx)
}

######
# use the info intergraded in redcap for more elegant solution:
# fundsrc$timeretrived
#Need to be more useful
bsrc.getdemo <- function(id,flavor="single",protocol = protocol.cur,printout=T,curdb=NULL,...){
  if (is.null(curdb)){
  curdb<-bsrc.checkdatabase2(protocol = protocol, ... = ...)
  }
  ifrun<-curdb$success
  if (ifrun){
    funbsrc<-curdb$data
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
        
        curage<-lubridate::as.period(lubridate::interval(start = as.Date(idonly$registration_dob), end = Sys.Date()))$year
        bsg<-data.frame(idonly$registration_id,idonly$registration_soloffid,idonly$registration_initials,
                        idonly$registration_dob,idonly$registration_consentdate,curage)
        names(bsg)<-c('ID',"Soloffid","Initials","Date of Birth","Consent Date" ,"AgeToday")
        if (printout==T){
        message(as.character('======================'))
        message(bsg)}
        }
        else {message("Multiple RedCap Record Identified")
              return(rid)}
      }
      else {message("NO ID FOUND, PLEASE DOUBLE CHECK")
        return(NULL)}
    }}
}
####Find duplicate RedCap IDs
bsrc.findduplicate <- function(protocol = protocol.cur) {
  curdb<-bsrc.checkdatabase2(protocol = protocol)
  funbsrc<-curdb$data
  dpqid<-data.frame()
    for (i in 1:length(unique(funbsrc$registration_soloffid)) ) {
      tryCatch({
    idq<-unique(funbsrc$registration_soloffid)[i]
    invisible(capture.output(krz<-bsrc.getdemo(id=idq,printout = F,curdb=curdb)))
    if(length(krz)>1){print(idq)
      print(krz)
      print(i)}
     },error=function(x){})
      }
    print("DONE")
}
################# Universal Function to deal with checkbox items:
bsrc.checkbox<-function(x,variablename = "registration_race",returndf = T,collapse=",",...) {
  raceonly<-x[grep(paste(variablename,"___",sep = ""),names(x))]
  options<-gsub(paste(variablename,"___",sep = ""),"",names(raceonly))
  x$xudjfnx<-lapply(1:length(raceonly[[1]]), function(i) {
    gsub(paste(variablename,"___",sep = ""),"",names(raceonly[i,])[which(raceonly[i,]==1)])
    })
  x$knxmncua<-sapply(x$xudjfnx, function(x) {paste(na.omit(x),collapse = collapse)})
  x$vximnucj<-sapply(x$xudjfnx,function(x) {length(x)>1})
  
  if (returndf) {
    colnames(x)[grep("xudjfnx",names(x))]<-variablename
    colnames(x)[grep("knxmncua",names(x))]<-paste(variablename,"_string",sep = "")
    colnames(x)[grep("vximnucj",names(x))]<-paste(variablename,"_ifmultiple",sep = "")
    return(x)}
  else {return(list(Checkbox_text=x$knxmncua,Checkbox_list=x$xudjfnx,Checkbox_ifmultiple=x$vximnucj))}
}
####### get choice mapping and its list varient
bsrc.getchoicemapping<-function(variablenames = NULL ,metadata=NULL,varifield="field_name",choicefield="select_choices_or_calculations",typefield="field_type",protocol=protocol.cur,...){
  if (is.null(variablenames)){stop("No variable name provided. Give me at least one name please!")}
  if (is.null(metadata)){
  curdb<-bsrc.checkdatabase2(protocol = protocol, ... = ...)
  metadata<-curdb$metadata
  }
  metasub<-subset(metadata,select = c(varifield,typefield,choicefield))
  names(metasub)<-c("fieldname","fieldtype","choice")
  variname.list<-as.list(variablenames)
  xzej<-lapply(variname.list,FUN = function(x){
    argk<-which(metasub$fieldname==x)
    if (metasub$fieldtype[argk] %in% c("dropdown","checkbox","radio")){
      tarstr<-metasub$choice[argk]
      firstspilt<-strsplit(tarstr,split = " | ",fixed = T)[[1]]
      secondspilt<-strsplit(firstspilt,split = ", ")
      choice.code<-as.character(sapply(secondspilt,"[[",1))
      choice.string<-as.character(sapply(secondspilt,"[[",2))
      xk<-data.frame(choice.code,choice.string)
      xk$choice.code<-as.character(xk$choice.code)
      xk$choice.string<-as.character(xk$choice.string)
      return(xk)
  } else {print(paste("This variable: '",x,"' has a type of [",metasub$fieldtype[argk],"], which is not supported!",sep = ""))}
  })
  names(xzej)<-variablenames
  if (length(xzej)==1){xzej<-xzej[[1]]}
  return(xzej)
}
########
bsrc.reg.group<-function(x,protocol,reverse=F){
  switch(protocol, 
         "bsocial"={
           f.from = c(1:4,88,89) 
           f.to = c("HC","LL ATT","HL ATT","NON-ATT","NOTSURE BPD","INELIGIBLE")},
         "ksocial"={
           f.from = c(1:7,88,89) 
           f.to = c("HC","DEP","DO NOT USE","IDE","ATT","LL ATT","HL ATT","NOT SURE","INELIGIBLE")
         })
  if (reverse) {
    f.from->from.x
    f.to->to.x
    f.from<-to.x
    f.to<-from.x
  }
  xt<-pylr::mapvalues(x, from = f.from, to = f.to, warn_missing = F)
  return(xt)
}
##############################
bsrc.reg.race<-function(x,reverse=F){
  f.from = c(1:5,999) 
  f.to = c("American Indian/ALNative","Asian","African American","HINative/Pacific Islander","White","NO INFO")
  if (reverse) {
    f.from->from.x
    f.to->to.x
    f.from<-to.x
    f.to<-from.x
  }
  xt<-pylr::mapvalues(x, from = f.from, to = f.to, warn_missing = F)
  return(xt)
}
#Combined use of the following allow extraction of data within EVENT and FORM
############################
#Function to get all data of given event:  #Get form names and then use get form have better flexiblity
bsrc.getevent<-function(eventname,protocol=protocol.cur,curdb=NULL,whivarform="default",nocalc=T,subreg=F,mod=F,aggressivecog=1,...){
  if (is.null(curdb)){curdb<-bsrc.checkdatabase2(protocol = protocol, ... = ...)}
  funbsrc<-curdb$data
  funevent<-curdb$eventmap
  funstrc<-curdb$metadata
  ifrun<-curdb$success
  protocol$redcap_uri->input.uri
  protocol$token->input.token
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
    default = ifelse(!exists("funevent") | is.null(funevent),funevent<-redcap.eventmapping(redcap_uri = input.uri,token = input.token),print("GOT IT")),
    anyfile = funevent<-read.csv(file.choose())
    )
    formname<-funevent$form[funevent$unique_event_name %in% eventname]
    if (subreg) {formname<-formname[-grep(paste("ipde","scid",sep = "|"),formname)]}  
    
    variablename<-names(bsrc.getform(formname = formname,curdb = curdb,forceskip = T))
    
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
bsrc.getform<-function(protocol = protocol.cur,formname,mod=T,aggressivecog=1, nocalc=T, grabnewinfo=F,res.event=NULL,curdb = NULL,...) {
  if (is.null(curdb)) {
  if (grabnewinfo) {
  curdb<-bsrc.conredcap2(protocol = protocol, fullupdate = F, output = T, updaterd = F,... = ...)
  ifrun<-TRUE
  }else if (!grabnewinfo) {
    curdb<-bsrc.checkdatabase2(protocol = protocol,... = ...)
    funbsrc<-curdb$data
    ifrun<-curdb$success
  }
  }else {funbsrc<-curdb$data
  ifrun<-curdb$success} 
  if (ifrun) {
  funstrc<-curdb$metadata
  funevent<-curdb$eventmap
  if (missing(formname)){
    message("Here's a list of forms: ")
    print(as.character(unique(as.character(funstrc$form_name))))
  formname<-readline(prompt = "Please type in one form name; if multiple, use ARGUMENT formname = c(,): ")
  }
  if (any(as.character(formname) %in% as.character(funstrc$form_name))) {
   if (grabnewinfo) {
     message("Grab updated data from RedCap.")
     lvariname<-as.character(funstrc$field_name[which(funstrc$form_name %in% formname)])
     lvariname<-c("registration_redcapid","redcap_event_name",lvariname)
     eventname<-funevent$unique_event_name[which(funevent$form %in% formname)]
      if (!is.null(res.event)) {
        #New feature: event restriction; for better subsetting when doing multiple forms
        eventname<-eventname[which(eventname %in% res.event)]
      }
     renew<-REDCapR::redcap_read(redcap_uri = protocol$redcap_uri ,token = protocol$token, fields = lvariname, events = eventname)
     if (renew$success){
       raw<-renew$data
     }else {stop("Update failed...;_; Try again?")}
   }else {
     lvariname<-as.character(funstrc$field_name[which(funstrc$form_name %in% formname)])
     raw<-funbsrc[,unique(c(1,2,grep(paste(lvariname,collapse = "|"),names(funbsrc))))]
     eventname<-funevent$unique_event_name[which(funevent$form %in% formname)]
     if (!is.null(res.event)) {
       #New feature: event restriction; for better subsetting when doing multiple forms
       eventname<-eventname[which(eventname %in% res.event)]
     }
     raw<-raw[which(raw$redcap_event_name %in% eventname),]
   }
   tempch<-funstrc[which(funstrc$form_name %in% formname),]
   if (nocalc){
     message("By default, will not take calculated field into consideration.")
     calmove<-length(which(tempch$field_type=="calc"))
     } else {calmove<-0}
   if (mod) {
     message("By default, NA will replace '' and 0 in checkbox items")
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
bsrc.getidmatchdb<-function(db,idfield="ID",protocol=protocol.cur,...) {
  curdb<-bsrc.checkdatabase2(protocol = protocol,... = ...)
  funbsrc<-curdb$data
  ifrun<-curdb$success
  if (ifrun) {
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
  }else stop(";_;")
}
##########################Working progress
dnpl.mappingtransfer<-function(map,spiltsign="."){
  getnthobject<-function(x,n) {
    sapply(x,"[[",n)
  }
  internmap<-data.frame(direction=getnthobject(strsplit(names(map),split = spiltsign,fixed = T),1),
                        dig=getnthobject(strsplit(names(map),split = spiltsign,fixed = T),2),
                        event=getnthobject(strsplit(names(map),split = spiltsign,fixed = T),3))
  cur.envir<-new.env(parent = emptyenv())
  for (i in unqiue(internmap$dig)) {
    rownames(internmap[internmap$dig==i])->rownum.x
    map[as.numeric(rownum.x)]->newmap
  }
}
########################## Match MericWire Identifier to df
bsrc.getmwidentifier<-function(db,only=F,funbsrc=NULL,protocol = protocol.cur, ...) {
  if (is.null(curdb)){
    curdb<-bsrc.checkdatabase2(protocol = protocol,... = ...)
    funbsrc<-curdb$data
  } else {curdb->funbsrc}
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
  excu<-paste("df[match(ndf$",by,",","df$",by,"),match(names(ndf),names(df))]<-ndf",sep = "")
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

########################## function call

# dnpl.dffunctioncall<-function(lfunc.object=list(
#                                               list(call=NULL, #either this function(x){} or this "function"
#                                                    argument=list(x=NULL,
#                                                                  y=NULL)
#                                                    )
#                                               ), envir=parent.env()) {
#   
#   lapply(lfunc.object, function(x) {print(x)
#   if (class(x$call)=="function") {
#     mode=FALSE
#   } else if (class(x$functionname)=="character") {
#     mode=TRUE
#   }
#   do.call(x$call,args = x$argument,quote = FALSE,envir = envir)
#   lfunc.object=list(list(call=max,argument=list(numtest),functionname="max"))
#   })
# } 
# 
# 
# 



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
