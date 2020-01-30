#---
#Title: "REDREW"
#Author: "Jiazhou Chen"
#Version: 2.2
#---
#[Task List]  
#0/1 Missingness check arm specific 
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
###################
is.empty<-function(...) {
  return(...=="")
}




redcap_upload<-function (ds_to_write, batch_size = 100L, interbatch_delay = 0.5, retry_whenfailed=T,
                         continue_on_error = FALSE, redcap_uri, token, verbose = TRUE, 
                         config_options = NULL) 
{
  start_time <- base::Sys.time()
  if (base::missing(redcap_uri)) 
    base::stop("The required parameter `redcap_uri` was missing from the call to `redcap_write()`.")
  if (base::missing(token)) 
    base::stop("The required parameter `token` was missing from the call to `redcap_write()`.")
  #token <- REDCapR::sanitize_token(token)
  ds_glossary <- REDCapR::create_batch_glossary(row_count = base::nrow(ds_to_write), 
                                                batch_size = batch_size)
  affected_ids <- character(0)
  excluded_ids <- 
    lst_status_code <- NULL
  lst_outcome_message <- NULL
  success_combined <- TRUE
  message("Starting to update ", format(nrow(ds_to_write), 
                                        big.mark = ",", scientific = F, trim = T), " records to be written at ", 
          Sys.time())
  for (i in seq_along(ds_glossary$id)) {
    selected_indices <- seq(from = ds_glossary[i, "start_index"], 
                            to = ds_glossary[i, "stop_index"])
    if (i > 0) 
    Sys.sleep(time = interbatch_delay)
    message("Writing batch ", i, " of ", nrow(ds_glossary), 
            ", with indices ", min(selected_indices), " through ", 
            max(selected_indices), ".")
    
    write_result <- redcap_oneshot_upload(ds = ds_to_write[selected_indices,], previousIDs = NULL,retry_whenfailed = T,
                                          redcap_uri = redcap_uri, token = token, verbose = verbose, 
                                          config_options = config_options)
    lst_status_code[[i]] <- write_result$status_code
    lst_outcome_message[[i]] <- write_result$outcome_message
    if (!write_result$success) {
      error_message <- paste0("The `redcap_write()` call failed on iteration ", 
                              i, ".")
      error_message <- paste(error_message, ifelse(!verbose, 
                                                   "Set the `verbose` parameter to TRUE and rerun for additional information.", 
                                                   ""))
      if (continue_on_error) 
        warning(error_message)
      else stop(error_message)
    }
    affected_ids <- c(affected_ids, write_result$affected_ids)
    success_combined <- success_combined | write_result$success
    excluded_ids <- c(excluded_ids, write_result$excludedIDs)
    rm(write_result)
  }
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, 
                                         units = "secs"))
  status_code_combined <- paste(lst_status_code, collapse = "; ")
  outcome_message_combined <- paste(lst_outcome_message, collapse = "; ")
  excluded_ids <- excluded_ids[excluded_ids!=""]
  return(list(success = success_combined, status_code = status_code_combined,excluded_ids=excluded_ids,
              outcome_message = outcome_message_combined, records_affected_count = length(affected_ids), 
              affected_ids = affected_ids, elapsed_seconds = elapsed_seconds))
}

clean_str <- function(dfx,remove=T,replace_text="") {
  dfx_back<-dfx
  logi_frame<-as.data.frame(apply(dfx,2,function(x){grepl("-xIT_WAS_NOT_ASCIIx-",iconv(x, "latin1", "ASCII", sub="-xIT_WAS_NOT_ASCIIx-"))}))
  vari_to_replace<-apply(logi_frame,2,any)
  message("These variables: ",paste(names(vari_to_replace)[vari_to_replace],collapse = ", "),", contain data_points that contain illegal characters.")  
  for (vax in names(vari_to_replace)[vari_to_replace]) {
    message("For variable '",vax,"', the following rows has illegal characters: \n",paste(which(logi_frame[[vax]]),collapse = ","))
    if(remove){
      dfx[which(logi_frame[[vax]]),vax] <- iconv(dfx[which(logi_frame[[vax]]),vax], "latin1", "ASCII", sub=replace_text)
    }
  }
  return(list(original_df=dfx_back,clean_df=dfx,logical_df=logi_frame))
}

redcap_oneshot_upload<-function (ds, redcap_uri, token, verbose = TRUE, config_options = NULL,retry_whenfailed=F,previousIDs=NULL) 
{
  start_time <- Sys.time()
  csvElements <- NULL
  if (missing(redcap_uri)) 
    stop("The required parameter `redcap_uri` was missing from the call to `redcap_write_oneshot()`.")
  if (missing(token)) 
    stop("The required parameter `token` was missing from the call to `redcap_write_oneshot()`.")
  #token <- REDCapR::sanitize_token(token)
  con <- base::textConnection(object = "csvElements", open = "w", 
                              local = TRUE)
  utils::write.csv(ds, con, row.names = FALSE, na = "")
  close(con)
  csv <- paste(csvElements, collapse = "\n")
  rm(csvElements, con)
  post_body <- list(token = token, content = "record", format = "csv", 
                    type = "flat", data = csv, overwriteBehavior = "overwrite", 
                    returnContent = "ids", returnFormat = "csv")
  result <- httr::POST(url = redcap_uri, body = post_body, 
                       config = config_options)
  status_code <- result$status_code
  raw_text <- httr::content(result, type = "text")
  
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, 
                                         units = "secs"))
  success <- (status_code == 200L)
  if (success) {
    elements <- unlist(strsplit(raw_text, split = "\\n"))
    affectedIDs <- elements[-1]
    recordsAffectedCount <- length(affectedIDs)
    outcome_message <- paste0(format(recordsAffectedCount, 
                                     big.mark = ",", scientific = FALSE, trim = TRUE), 
                              " records were written to REDCap in ", round(elapsed_seconds, 
                                                                           1), " seconds.")
    raw_text <- ""
  }
  else {
    if(retry_whenfailed){
      outcome_message<-outcome_message <- paste0("The upload was not successful:\n", 
                                                 raw_text,"\n","But we will try again...\n")
      sp_rawtext<-strsplit(raw_text,split = "\\n")[[1]]
      allgx<-lapply(sp_rawtext,function(x){xa<-strsplit(gsub("\"","",x),",")[[1]];})
      mxID<-sapply(allgx,function(sp_rawtext){gsub("ERROR: ","",sp_rawtext[1])})
      allIDs<-c(previousIDs,mxID)
      negPos<-as.numeric(na.omit(sapply(allIDs,function(IDX){
        #print(IDX)
        a<-unique(which(ds==IDX,arr.ind = T)[,1]);
        if(length(a)>0){a}else{NA}
      })))
      ds_new<-ds[-negPos,]
      gx<-redcap_oneshot_upload(ds = ds_new, redcap_uri = redcap_uri, token = token, verbose = verbose, 
                                retry_whenfailed = T,previousIDs = allIDs,
                                config_options = config_options)
      raw_text<-paste(raw_text,gx$raw_text,sep = "re-try: ")
      success<-gx$success
      status_code<-gx$status_code
      outcome_message<-paste(outcome_message,gx$outcome_message,sep = "re-try: ")
      recordsAffectedCount<-gx$records_affected_count
      affectedIDs<-gx$affected_ids
      elapsed_seconds<-gx$elapsed_seconds
      previousIDs<-gx$excludedIDs
    } else {
      affectedIDs <- numeric(0)
      recordsAffectedCount <- NA_integer_
      outcome_message <- paste0("The REDCapR write/import operation was not successful.  The error message was:\n", 
                                raw_text)
    }
  }
  if (verbose) 
    message(outcome_message)
  if (!is.null(previousIDs)){excludedIDs<-previousIDs}else {excludedIDs<-""}
  return(list(success = success, status_code = status_code, 
              outcome_message = outcome_message, records_affected_count = recordsAffectedCount, 
              affected_ids = affectedIDs, elapsed_seconds = elapsed_seconds, excludedIDs = excludedIDs,
              raw_text = raw_text))
}


redcap_seq_uplaod<-function(ds,id.var,redcap_uri,token,batch_size=1000L) {
  ds_sp<-split(ds,do.call(paste,c(ds[id.var],sep="_")))
  gt <- lapply(ds_sp,function(gx){gx[!apply(gx,2,is.na)]})
  gt_u_names<-unique(lapply(gt,names))
  gt_u_index <- sapply(gt,function(gtx){match(list(names(gtx)),gt_u_names)})
  gyat<-lapply(1:length(gt_u_names),function(g){
    message("uploading ",g," out of ",length(gt_u_names))
    redcap_write(ds_to_write = do.call(rbind,gt[which(gt_u_index == g)]),batch_size = batch_size,redcap_uri = redcap_uri,token = token,continue_on_error = T)
  })
  return(
    list(affected_ids=unlist(sapply(gyat,`[[`,"affected_ids"),use.names = F),outcome_message = unlist(sapply(gyat,`[[`,"outcome_message"),use.names = F))
         )
}


###############Get Event Mapping from RedCap:


redcap.getreport<-function(redcap_uri, token, reportid = NULL, message = TRUE, config_options = NULL) {
  start_time <- Sys.time()
  if (missing(redcap_uri))  {
    stop("The required parameter `redcap_uri` was missing from the call to `redcap.getreport`.") }
  if (missing(token)) {
    stop("The required parameter `token` was missing from the call to `redcap.getreport`.") }
  #token <- REDCapR::sanitize_token(token)
  post_body <- list(token = token, content = "report", format = "csv", report_id=as.character(reportid))
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
      raw_text <- ""} else {success <- FALSE
      ds <- data.frame()
      outcome_message <- paste0("The REDCap metadata export failed.  The http status code was ", 
                                status_code, ".  The 'raw_text' returned was '", 
                                raw_text, "'.")}
  } else {
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
    message("constructing new one for: ",name)
    protocol<-list(name=name,redcap_uri=redcap_uri,token=token,rdpath=rdpath)
  } else {message("Not enough info")}
  if (!is.null(regiformname)) {protocol$regiformname<-regiformname}
  if (!is.null(forcenewsubinsync)) {protocol$forcenewsubinsync<-forcenewsubinsync}
  if (protocol.cur){
    protocol.cur<<-protocol
  } else {return(protocol)}
}
#########################Check & Attach
bsrc.attachngrab<-function(rdpath=NULL, protocol=protocol.cur, returnas="envir",envir=new.env()){
  if (is.null(rdpath)) {
    if(is.list(protocol)) {protocol.n<-protocol$name
    rdpath<-protocol$rdpath
    } else {stop("ERROR, protocol object is not a list.")}
  } else {message("when rdpath argument available, will always use that")}
  
  if(file.exists(rdpath)){
    loadrdata(rdpath=rdpath,returnas=returnas,envir=envir)
  } else {"No such file...."}
}
##########################

loadrdata<-function(rdpath=NULL,returnas="envir",envir=new.env()) {
  load(rdpath,envir = envir)
  switch (returnas,
          "envir" = {return(envir)},
          "list"  = {return(as.list(envir))}
  )
}
#################################
bsrc.valuetostring<-function(variname=NULL,valuein=NULL,metadata=NULL){
  fieldmap<-bsrc.getchoicemapping(variablenames = variname,metadata = metadata)
  return(plyr::mapvalues(x = valuein,from = fieldmap$choice.code,to=fieldmap$choice.string))
}

##########################




#########################New Ver in DEV
bsrc.conredcap2<-function(protocol=protocol.cur,updaterd=T,batch_size=1000L,output=F,newfile=F,online=F,...) {
  if (missing(protocol)) {stop("no protocol specified")}
  
  if (is.list(protocol)) {
    message(paste(names(protocol[ protocol != protocol$token ]),protocol[ protocol != protocol$token ],sep = ": ",collapse = "\n"))
    protocol.n<-protocol$name
    input.uri<-protocol$redcap_uri
    input.token<-protocol$token
    rdpath<-protocol$rdpath
  } else {message("protocol has not sufficient information, using global variables [input.uri/input.token]")}
  
  if (is.na(rdpath) | is.null(rdpath)) {online<-TRUE}
  
  if (!output & updaterd) {
    if (!online) {
      if (file.exists(rdpath) & !newfile) {
        pathsplit<-strsplit(rdpath,split = "/")[[1]]
        topath<-paste(paste(pathsplit[-length(pathsplit)],collapse = "/",sep = ""),"Backup","conredcap.backup.rdata",sep = "/")
        dir.create(dirname(topath),recursive = T,showWarnings = F)
        file.copy(from = rdpath, to = topath, overwrite = T)
        cur.envir<-bsrc.attachngrab(protocol = protocol, returnas = "envir")
      }else if (newfile | !file.exists(rdpath)) {
        message("Starting new file...")
        cur.envir<-new.env(parent = emptyenv())
        allobjects<-c(protocol.n)
        }
    } else {cur.envir<-new.env(parent = emptyenv())}
  } else {cur.envir<-new.env(parent = emptyenv())}
  
  project_info <- redcap_api_call(redcap_uri = input.uri,token = input.token,content = "project")$output
  
  if(as.logical(project_info$is_longitudinal)) {
    toget <- c("metadata","data","eventmap")
  } else {
    toget <- c("metadata","data")
  }
  
  success_sequence<-sapply(toget,function(xe){
    if(xe == "data") {
      dxe <- "record"
    } else if (xe == "eventmap") {
      dxe <- "formEventMapping"
    } else {dxe <- xe}
    message("loading: ",xe)
    output <- redcap_api_call(redcap_uri = input.uri,token = input.token,content = dxe,batch_size = batch_size)
    if (!output$success) {
      message(xe," did not load successfully. error message is \n",output)
      return(FALSE)
    } else {
      assign(xe,output$output,envir = cur.envir)
      return(TRUE)
    }
  })
  
  assign("name",protocol$name,envir = cur.envir)
  assign("is_longitudinal",as.logical(project_info$is_longitudinal),envir = cur.envir)
  assign("success",!any(!success_sequence),envir = cur.envir)
  assign("update.date",Sys.Date(),envir = cur.envir)
  assign("update.time",Sys.time(),envir = cur.envir)
  if(any(!success_sequence)) {
    message(paste(names(success_sequence)[which(!success_sequence)],"did not load successfully.",sep=": ",collapse = "\n"))
    message("will output the rest.")
  } else {
    message("success")
  }
  
  if (updaterd && !online && !any(!success_sequence)){
    save(list = objects(cur.envir),envir = cur.envir,file = rdpath)
  }
  
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
            message(paste("Whelp...it's been more than ",expiration," hours since the db was updated, let's update it..."))
            reload<-TRUE
          } 
        }else {
          message("Something went wrong when loading rdata file...")
          ifso<-readline(prompt = "To continue with the file, type 'T' or to reload type 'F' : ")
          if (!as.logical(ifso)){
            reload<-T
          }
        }
        
      }else{message("FORCE SKIP RDATA CHECKS")
        ifrun<-T}
    }else{message("No such file...reloading")
      reload<-T}
  } else {message("Online mode is on")
    reload<-F
    online<-T
    forceupdate<-F
    }
  
  if (reload | forceupdate) {
    bsrc.conredcap2(protocol = protocol,online=online,... = ...)
    bsrc.checkdatabase2(protocol = protocol,forceupdate = F,...)
  }else {ifrun<-TRUE}
  
  if (online) {
    curdb<-bsrc.conredcap2(protocol = protocol,online=online,... = ...)
    ifrun<-TRUE
  }
  
  if(ifrun & !logicaloutput) {return(curdb)}
  if(logicaloutput) {return(ifrun)}
}


rc_na_remove <- function(raw,mod=TRUE,IDvar=NULL,at_least=1) {
  if(mod) {
    message("NA will replace '' and 0 in checkbox items, Set 'mod' to FALSE to avoid modificaiton to data frame.")
    #raw[raw==""]<-NA
    if (length(grep("___",names(raw))) > 0){
      raw[,grep("___",names(raw))][raw[,grep("___",names(raw))] == "0"]<-NA
    }
    value_vari<-names(raw)[!names(raw) %in% c(IDvar,"redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")]
    
    valid_nums<-which(rowSums(is.na(raw[value_vari])) <= (length(value_vari) - (at_least) ))
    message("Using of ",length(value_vari)," value variables, ",(nrow(raw) - length(valid_nums))," observations were removed.")
    raw_new <- raw[valid_nums,]
  } else {raw_new <- raw}
  
  return(raw_new)
}

# 
# iconv(dd, 
#       from = "utf8", 
#       to = "ASCII", 
#       sub = "")


###############################
bsrc.getform<-function(protocol = NULL,formname,online=F,filter_events=NULL,curdb = NULL,IDvar="registration_redcapid",mod=T,at_least=1, no_calc=T,batch_size=1000L,...) {
  project_info <- redcap_api_call(redcap_uri = protocol$redcap_uri,token = protocol$token,content = "project")
  #Get necessary data
  if (online) {
    metadata <- redcap_api_call(redcap_uri = protocol$redcap_uri,token = protocol$token,content = "metadata")
    if(as.logical(project_info$is_longitudinal)){
      eventdata <- redcap_api_call(redcap_uri = protocol$redcap_uri,token = protocol$token,content = "formEventMapping")
    } else {eventdata <- NULL}

  } else {
    if (is.null(curdb) ) {curdb <- bsrc.checkdatabase2(protocol = protocol,forceskip = T)} 
    stopifnot(exprs = {curdb$success})
    data <- curdb$data
    metadata <- curdb$metadata
    eventdata <- curdb$eventmap
  }
  
  #Determine if this project has events
  if(is.null(eventdata)){
    no_evt_rc<-TRUE;message("This RedCap project does not have multiple events.")
    fix_variables<-IDvar
  } else {
    no_evt_rc<-FALSE
    fix_variables<-c(IDvar,"redcap_event_name")
  }
  #Get form name(s) if not specified
  if (missing(formname)){
    message("Here's a list of forms: ")
    print(unique(as.character(metadata$form_name)))
    formname<-readline(prompt = "Please type in form name (single value only, to get multiple forms at once, use argument 'formname'): ")
  }
  
  if (any(as.character(formname) %in% as.character(metadata$form_name))) {
    
    #Get variable names and events if applicable
    lvariname<-as.character(metadata$field_name[which(metadata$form_name %in% formname)])
    #lvariname<-
    if(!no_evt_rc){
      eventname<-eventdata$unique_event_name[which(eventdata$form %in% formname)]
      if (!is.null(filter_events)) {
        eventname<-eventname[which(eventname %in% filter_events)]
      }
    } else {
      eventname <- NULL
    }
    
    
    if (online) {
      #Do online version:
      message("Getting form data directly from RedCap.")
      
      renew<-REDCapR::redcap_read(redcap_uri = protocol$redcap_uri ,token = protocol$token, fields = c(fix_variables,lvariname), events = eventname,batch_size = batch_size)
      if (renew$success){
        raw<-renew$data
      } else if (nrow(renew$data)==0) {
        return(NULL)
      } else {stop("Failed... Try again?")}
      
    } else {
      #Do offline version:
      message("Getting form data from saved RedCap data.")
      #Offline version is a bit problematic with check box thing; spliting the list of variables to get by if they are checkbox or not
      check_box_varis<-split(lvariname,metadata$field_type[match(lvariname, metadata$field_name)] == "checkbox")
      
      #Get a (fixed variables) and b (non-checkbox data)
      #return(list(data,check_box_varis,fix_variables))
      raw_a <- as.data.frame(data[fix_variables])
      
      raw_b <- data[check_box_varis$`FALSE`[which(check_box_varis$`FALSE` %in% names(data))]]
      
      
      if(is.null(check_box_varis$`TRUE`)) {
        #No checkbox 
        raw <- cbind(raw_a,raw_b)
      } else {
        #If there are check box in this form
        raw_c <- data[unique(unlist(lapply(check_box_varis$`TRUE`,function(x){grep(x,names(data),value = T)})))]
        raw <- cbind(raw_a,raw_b,raw_c)
      }
      
      if(!is.null(eventname)) {
        #Event filtering
        raw <- raw[which(raw$redcap_event_name %in% eventname),]
      }
    }
    
    tempch<-metadata[which(metadata$form_name %in% formname),]
    if (no_calc){
      message("Calculated fields are excluded. Set no_calc to FALSE to include them.")
      cal_vari<-tempch$field_name[which(tempch$field_type=="calc")]
      raw <- raw[,which(!names(raw) %in% cal_vari)]
      calmove <- length(cal_vari)
    } else {calmove<-0}

    new_raw<-rc_na_remove(raw = raw,mod=mod,IDvar=IDvar,at_least = at_least)
    return(new_raw)
  }
  else {message("Form [",formname,"] can not be loacted.")}
}

###############################
bsrc.findid<-function(df,idmap=NULL,id.var="ID",onlyoutput=NULL){
  cleanmap<-idmap
  if (!missing(df)){
    t<-lapply(df[[id.var]],function(id) {
      pos<-as.data.frame(which(cleanmap==id,arr.ind = T))
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
    names(t)<-df[[id.var]]
    if(any(sapply(t,nrow)>1)){
      
      message("Duplicated ID map entry found for singular ID, terminate and return list of ID identified.")
      return(t[sapply(t,nrow)>1])
    }
    
    tx<-do.call(rbind,t)
    if (!is.null(onlyoutput)){tx<-tx[c(onlyoutput)]}
    lx<-cbind(df,tx)
    return(lx)
  } else {return(idmap)}
}
#############
bsrc.refineupload<-function(dfx=NULL,id.var="registration_redcapid",perference="redcap",curdb=NULL,onlyrc=T){
  varstodo<-names(dfx)[!names(dfx) %in% c(id.var,"redcap_event_name")]
  varstodo<-varstodo[varstodo %in% curdb$metadata$field_name]
  metd<-curdb$metadata
  formn<-unique(metd$form_name[metd$field_name %in% varstodo])
  dbx<-bsrc.getform(curdb = curdb,formname = formn)
  if (any(varstodo %in% metd$field_name[metd$field_type=="checkbox"])) {
    cbyes<-T
    cbs<-varstodo[which(varstodo %in% metd$field_name[metd$field_type=="checkbox"])]
    for (cb in cbs) {
      dbx<-bsrc.checkbox(x = dbx,variablename = cb)
    }
  } else {cbyes<-F}
  for (vartodo in varstodo) {
    xk<-lapply(1:length(dfx[[id.var]]),function(i){
      as.character(dfx[i,id.var])->id
      as.character(dfx[i,"redcap_event_name"])->event
      dfx[[i,vartodo]]->x
      if (is.list(x)){
        x<-lapply(x,function(x){if(length(x)<1) {return(NULL)} else return(x)})
        x<-unlist(cleanuplist(x))
      }
      x<-x[!is.na(x)]
      if (any(is.character(x))) {x<-x[x!=""]}
      x<-unique(x)
      
      if (length(event)<1) {
        dbx[which(dbx[[id.var]]==id),vartodo]->xrc
      } else {
        dbx[which(dbx[[id.var]]==id & dbx$redcap_event_name==event),vartodo]->xrc}
      
      if (is.list(xrc)){
        xrc<-lapply(xrc,function(x){if(length(x)<1) {return(NULL)} else return(x)})
        xrc<-unlist(cleanuplist(xrc))
      }
      xrc<-xrc[!is.na(xrc)]
      if (any(is.character(xrc))) {xrc<-xrc[xrc!=""]}
      xrc<-unique(xrc)
      
      if(length(xrc)<1 |is.null(xrc) |!any(!is.na(xrc))) {xrc<-NA
      } else if (length(xrc)>1 && any(is.na(xrc))) {xrc<-na.omit(xrc)}
      if(length(x)<1 |is.null(x) |!any(!is.na(x))) {x<-NA
      } else if (length(x)>1 && any(is.na(x))) {x<-na.omit(x)}
      
      if (any(is.na(xrc))) {return(x)
      } else if (any(is.na(x))) {return(xrc)
      } else if (length(xrc)!=length(x) | any(!xrc %in% x)){
        if (perference == "redcap") {return(xrc)}
        if (perference == "data") {return(x)}
        if (perference == "NA") {return(NA)}
      } else return(xrc)
      
    })
    #do duplicate action here:
    if (!any(sapply(xk,length)>1) ) {xk<-unlist(xk)}
    dfx[[vartodo]]<-xk
  }
  
  if(onlyrc) {
    if(any(is.null(dfx$redcap_event_name))) {dfx<-dfx[,c(id.var,varstodo)]} else {
      dfx<-dfx[,c(id.var,"redcap_event_name",varstodo)]}
  }
  if (cbyes){dfx<-bsrc.choice2checkbox(dfx = dfx,metadata = metd)}
  
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

####Find duplicate RedCap IDs
bsrc.findduplicate <- function(protocol = protocol.cur) {
  curdb<-bsrc.checkdatabase2(protocol = protocol)
  funbsrc<-curdb$data
  dpqid<-data.frame()
  for (i in 1:length(unique(funbsrc$registration_soloffid)) ) {
    tryCatch({
      idq<-unique(funbsrc$registration_soloffid)[i]
      invisible(capture.output(krz<-bsrc.getdemo(id=idq,messageout = F,curdb=curdb)))
      if(length(krz)>1){message(idq)
        message(krz)
        message(i)}
    },error=function(x){})
  }
  message("DONE")
}
####################
bsrc.gettimeframe<-function(dfx=NULL,curdb=NULL,returnmap=F,returndfx=T,protocol=protocol.cur,...) {
  if (is.null(curdb)){
    curdb<-bsrc.checkdatabase2(protocol = protocol, ... = ...)
  }
  datesx<-curdb$data[c("registration_redcapid","redcap_event_name","demo_visitdate","fudemo_visitdate")]
  datesx[datesx==""]<-NA
  datesx$event_date<-sapply(1:length(datesx$registration_redcapid), function(iz) {
    if (!is.na(datesx[iz,]$demo_visitdate)) {return(datesx[iz,]$demo_visitdate)
    } else if (!is.na(datesx[iz,]$fudemo_visitdate)) {return(datesx[iz,]$fudemo_visitdate)
    } else {return(NA)} 
  })
  datesz<-datesx[!is.na(datesx$event_date),c("registration_redcapid","redcap_event_name","event_date")]
  datesz$timeframe<-as.numeric(gsub("([0-9]+).*$", "\\1", datesz$redcap_event_name))
  datesz$timeframe[datesz$redcap_event_name=="baseline_arm_1"]<-0
  
  if (is.null(dfx) | returnmap) {
    return(datesz)
  } else if (returndfx) {
    
  } else {message("unable to return based on input argument")}
  
}
#####################
dnpl.bso.getsahx<-function(curdb=NULL) {
  metd<-curdb$metadata
  sahx<-bsrc.getform(formname = "suicide_history",curdb = curdb,mod = F,IDvar = "registration_redcapid")
  varying<-names(sahx)[grep("[0-9]+",names(sahx))]
  notvarying<-names(sahx)[-grep("[0-9]+",names(sahx))]
  dfz<-tolong_multivalue(dfx = sahx, varying = varying, notvarying = notvarying,id.var = c("registration_redcapid"),
                         var.left = "type",var.right="attempt",sep = "_at",timepos = "right")
  dfe<-dfz[!is.na(dfz$sahx_sadate),]
  idmap<-bsrc.getform(curdb = curdb,formname="record_registration")[c("registration_redcapid","registration_soloffid")]
  dff<-bsrc.findid(dfe,idmap,id.var = "registration_redcapid",onlyoutput = "registration_soloffid")
  return(dff)
}
###########################
bsrc.getSUIHX_index<-function(sahx_df){
  sui_names<-names(sahx_df)
  index_df<-data.frame(names=sui_names,rxsim1=gsub(".*_(at[0-9]*$)",'\\1',gsub("___.*","",sui_names),perl = T),stringsAsFactors = F)
  index_df$SingleEntry<-index_df$names==index_df$rxsim1
  index_df$is_checkbox<-grepl("___",index_df$names)
  index_df$root_names<-index_df$names;index_df$checkbox_names<-NA
  index_df$root_names[index_df$is_checkbox]<-gsub("___.*$","",index_df$root_names[index_df$is_checkbox])
  index_df$checkbox_names[index_df$is_checkbox]<-gsub("___.*$","",index_df$root_names[index_df$is_checkbox])
  index_df$root_names<-gsub("_at[0-9]*$","\\1",index_df$root_names)
  return(index_df)
}

###########################
ProcApply<-function(listx=NULL,FUNC=NULL,...,addNAtoNull=T) {
  proc_ls<-lapply(X = listx,FUN = FUNC,... = ...)
  if(addNAtoNull){
    allnames<-unique(unlist(lapply(proc_ls,names)))
    proc_ls<-lapply(proc_ls,function(lsx){
      lsx[allnames[which(!allnames %in% names(lsx))]]<-NA
      return(lsx)
    })
  }
  proc_ls<-cleanuplist(proc_ls)
  return(list(list=proc_ls,
              df=do.call(rbind,proc_ls)))
}

##########################
tolong_multivalue<-function(dfx=NULL,varying=NULL,notvarying=NULL,id.var=c("registration_redcapid","redcap_event_name"),var.left="type",var.right="attempt",sep="_at",timepos="right") {
  dfx_var_melt<-reshape2::melt(data = dfx,id.vars=notvarying)
  dfx_var_melt[[var.left]]<-sapply(strsplit(as.character(dfx_var_melt$variable),split = sep,fixed = T),"[[",1)
  dfx_var_melt[[var.right]]<-sapply(strsplit(as.character(dfx_var_melt$variable),split = sep,fixed = T),"[[",2)
  dfx_var_melt$variable<-NULL
  if (timepos=="right") {
    timevar<-var.left
    id.var<-c(id.var,var.right)
  } else if (timepos=="left") {
    timevar<-var.right
    id.var<-c(id.var,var.left)
  }
  dfx_reshape<-reshape(dfx_var_melt,timevar = timevar,v.names = "value",idvar = id.var,direction = "wide")
  names(dfx_reshape)<-gsub("value.","",names(dfx_reshape),fixed = T)
  
  return(dfx_reshape)
}
################# Universal Function to deal with checkbox items:
bsrc.checkbox<-function(variablename = "registration_race",dfx=NULL,returndf = T,cleandf=T,returnstring=F,collapse=",",...) {
  varionly<-dfx[grep(paste(variablename,"___",sep = ""),names(dfx))]
  options<-gsub(paste(variablename,"___",sep = ""),"",names(varionly))
  dfx[[variablename]]<-lapply(1:length(varionly[[1]]), function(i) {
    ix<-gsub(paste(variablename,"___",sep = ""),"",names(varionly[i,])[which(varionly[i,]==1)])
    if (length(ix)>0) {return(ix)} else {return(NA)}
  })
  dfx[[paste(variablename,"__string",sep = "")]]<-sapply(dfx[[variablename]], function(x) {paste(na.omit(x),collapse = collapse)})
  dfx[[paste(variablename,"__ifmultiple",sep = "")]]<-sapply(dfx[[variablename]],function(x) {length(x)>1})
  if (returndf) {
    if (cleandf) { 
      dfx<-dfx[-c(grep(paste(variablename,"___",sep = ""),names(dfx)),
                  grep(paste(paste(variablename,c("string","ifmultiple"),sep = "__"),collapse = "|"),names(dfx)))]
      if (returnstring) {dfx[[variablename]]<-sapply(dfx[[variablename]], function(x) {paste(na.omit(x),collapse = collapse)})}
    }
    return(dfx)}
  else {return(list(Checkbox_text=dfx[[variablename]],
                    Checkbox_list=dfx[[paste(variablename,"__string",sep = "")]],
                    Checkbox_ifmultiple=dfx[[paste(variablename,"__ifmultiple",sep = "")]]))}
}
####### get choice mapping and its list varient
bsrc.getchoicemapping<-function(variablenames = NULL ,metadata=NULL,
                                varifield="field_name",choicefield="select_choices_or_calculations",typefield="field_type",protocol=protocol.cur,...){
  if (is.null(variablenames)){stop("No variable name provided. Give me at least one name please!")}
  if (is.null(metadata)){
    curdb<-bsrc.checkdatabase2(protocol = protocol, ... = ...)
    metadata<-curdb$metadata
  }
  metasub<-subset(metadata,select = c(varifield,typefield,choicefield))
  names(metasub)<-c("fieldname","fieldtype","choice")
  variname.list<-as.list(variablenames)
  xzej<-lapply(variname.list,FUN = function(x){
    #print(x)
    argk<-which(metasub$fieldname==x)
    if (metasub$fieldtype[argk] %in% c("dropdown","checkbox","radio")){
      tarstr<-metasub$choice[argk]
      tarstr<-gsub("[^ ] \\|[^ ]"," | ",tarstr)
      firstspilt<-strsplit(tarstr,split = " | ",fixed = T)[[1]]
      if(length(grep(", ",firstspilt))!=length(firstspilt)) {firstspilt<-gsub(",",",  ",firstspilt)}
      secondspilt<-strsplit(firstspilt,split = ", ")
      choice.code<-as.character(sapply(secondspilt,"[[",1))
      choice.string<-as.character(sapply(secondspilt,"[[",2))
      xk<-data.frame(choice.code,choice.string)
      xk$choice.code<-as.character(xk$choice.code)
      xk$choice.string<-as.character(xk$choice.string)
      xk$choice.string[xk$choice.string==" "]<-""
      return(xk)
    } else if (metasub$fieldtype[argk] %in% c("yesno")) {
      xk<-data.frame(choice.code=c(1,0),choice.string=c("Yes","No"))
    }else {message(paste("This variable: '",x,"' has a type of [",metasub$fieldtype[argk],"], which is not supported!",sep = ""))}
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
           f.to = c("HC","DEP","DO NOT USE","IDE","ATT","LL ATT","HL ATT","NOTSURE PROTECT","INELIGIBLE")
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
bsrc.getevent<-function(eventname,protocol=protocol.cur,curdb=NULL,nocalc=T,mod=F,aggressivecog=1,...){
  if (is.null(curdb)) {
    if (grabnewinfo) {
      curdb<-bsrc.conredcap2(protocol = protocol, output = T, updaterd = F,... = ...)
      ifrun<-TRUE
    }else if (!grabnewinfo) {
      curdb<-bsrc.checkdatabase2(protocol = protocol,... = ...)
      funbsrc<-curdb$data
      ifrun<-curdb$success
      funevent<-curdb$eventmap
    }
  }else {
    funbsrc<-curdb$data
    ifrun<-curdb$success
    funevent<-curdb$eventmap
  } 
  
  if(ifrun) {
    if (missing(eventname)){
      message(as.character(unique(funbsrc$redcap_event_name)))
      eventname<-readline(prompt = "Please type in the event name: ")
    }
    
    eventonly<-funbsrc[which(funbsrc$redcap_event_name %in% eventname),]
    formname<-funevent$form[funevent$unique_event_name %in% eventname]
    eventonly.r<-bsrc.getform(formname = formname,curdb = curdb,res.event = eventname,forceskip = T,aggressivecog = aggressivecog,
                              nocalc=nocalc,mod=mod)
    
    return(eventonly.r)
  }
}
#####################################
#Functions to get all data from given forms: 


rc_na_checkboxremove<-function(raw){
  message("By default, NA will replace '' and 0 in checkbox items")
  raw[raw==""]<-NA
  if (length(grep("___",names(raw))) > 0){
    raw[,grep("___",names(raw))][raw[,grep("___",names(raw))] == "0"]<-NA
  }
  return(raw)
}

#########################
### MATACH FUNCTIONS ####
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
bsrc.getIDEVTDate<-function(dbx=NULL,rcIDvar="registration_redcapid",evt_filter=NA,evt_fieldtag="EVT_DATEFIELD"){
  metadfx<-dbx$metadata[grep(evt_fieldtag,dbx$metadata$field_note),]
  evtmap<-dbx$eventmap[which(dbx$eventmap$form %in% metadfx$form_name),]
  evtmap$date_variname<-metadfx$field_name[match(evtmap$form,metadfx$form_name)]
  return(evtmap)
  
  IDEVT_a<-dbx$data[which(dbx$data$redcap_event_name %in% evtmap$unique_event_name & !dbx$data$redcap_event_name %in% evt_filter),
                    c(rcIDvar,"redcap_event_name",metadfx$field_name)]
  IDEVT_w<-melt(IDEVT_a,id.vars=c(rcIDvar,"redcap_event_name"))
  IDEVT_wa<-IDEVT_w[which(!is.na(IDEVT_w$value) & IDEVT_w$value!=""),]
  names(IDEVT_wa)<-c(rcIDvar,"redcap_event_name","variname","date")
  return(IDEVT_wa)
}
# dnpl.dffunctioncall<-function(lfunc.object=list(
#                                               list(call=NULL, #either this function(x){} or this "function"
#                                                    argument=list(x=NULL,
#                                                                  y=NULL)
#                                                    )
#                                               ), envir=parent.env()) {
#   
#   lapply(lfunc.object, function(x) {message(x)
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
######MasterDEMO related:

bsrc.masterdemo.checkduplicate<-function(protocol=ptcs$masterdemo,infovars="registration_redcapid",
                                         uniquevars=c("registration_initials","registration_gender","registration_lastfour","registration_dob")){
  masterdemo<-bsrc.conredcap2(protocol = ptcs$masterdemo,batch_size = 1000L,output = T)
  trydf<-masterdemo$data[c(infovars,uniquevars)]
  trydf$uniuqe_identifying_variable<-apply(trydf[uniquevars],1,paste,collapse=" - ")
  #masterdemo$data[masterdemo$data==""]<-NA
  tocleandf<-split(trydf,trydf$uniuqe_identifying_variable)
  tocleandf<-tocleandf[which(sapply(tocleandf,nrow)>1)]
  return(tocleandf)
}

bsrc.change_grp_ptcs<-function(input=NULL,origin=c("bsocial","protect","masterdemo"),destination=c("bsocial","protect","masterdemo")){
  if(is.null(input)){message("No input, supports data.frame (must specify group name) or vector string")}
  if((!origin %in% c("bsocial","protect","masterdemo")) | (!destination %in% c("bsocial","protect","masterdemo")) ) {
    message("Only supports the following:",c("bsocial","protect","masterdemo"))}
  grp_map<-data.frame(masterdemo=c("HC","NON","DEP","IDE","ATT","ATT","ATT","88","89"),
                      bsocial=c("1","4","4","4",NA,"2","3","88","89"),
                      protect=c("HC","NON","DEP","IDE","ATT","ATT","ATT","88","89"),stringsAsFactors = F
  )
  vari_map<-data.frame(masterdemo="registration_group",
                       bsocial="registration_group",
                       masterdemo="startup_group",stringsAsFactors = F
  )
  if(!is.data.frame(input)){input<-as.character(input)}
  
  switch (class(input),
          "character" = {
            noorder<-is.na(input)
            input<-plyr::mapvalues(x = input,from = grp_map[[origin]],to = grp_map[[destination]],warn_missing = F)
            input[noorder]<-NA
          },
          "data.frame" = { noorder<-is.na(input[[vari_map[[origin]]]])
          input[[vari_map[[destination]]]]<-plyr::mapvalues(x = input[[vari_map[[origin]]]],from = grp_map[[origin]],to = grp_map[[destination]],warn_missing = F)
          if (vari_map[[origin]] != vari_map[[destination]]){input[[vari_map[[origin]]]]<-NULL};
          input[[vari_map[[destination]]]][noorder]<-NA
          return(input)},
  )
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
