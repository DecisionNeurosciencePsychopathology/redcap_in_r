#Basics:
#' redcap_uri=ptcs$protect$redcap_uri
#' token=ptcs$protect$token
#' 
#' action = NULL
#' content = NULL
#' arms = NULL
#' records = NULL
#' fields = NULL
#' 
#' message = TRUE




redcap_api_call<-function (redcap_uri=NULL, token=NULL,
                           action = NULL, content = NULL,
                           records = NULL,arms = NULL,events=NULL, forms=NULL, fields = NULL,
                           export_file_path = NULL,batch_size = 500L, carryon = FALSE,
                           message = TRUE,httr_config=NULL,post_body=NULL,upload_file=NULL,...) {
  #Use this space to document
  #List of Contents:
  #formEventMapping report metadata event participantList exportFieldNames project instrument user instrument generateNextRecordName record pdf file
  #List of Actions:
  #delete export import

  if(is.null(redcap_uri) ) {stop("requires redcap_uri")}
  if(is.null(token) && is.null(post_body) ) {stop("requires token or constructed post body")}
  if(is.null(content) ) {
    message("no content type supplied, using default: 'record'.")
    content <- "record"
  }
  ls_add <- list(...)
  if(is.null(post_body)){
    post_body <- list(token = token, content = content, format = "csv")
  }
  post_body$content <- content
  if(!is.null(action) && action!= "record_single_run"){post_body$action = action}
  if(!is.null(arms)){post_body$arms<-paste(arms,sep = "",collapse = ",")}
  if(!is.null(events)){post_body$events<-paste(events,sep = "",collapse = ",")}
  if(!is.null(fields)){post_body$fields<-paste(fields,sep = "",collapse = ",")}
  if(!is.null(forms)){post_body$forms<-paste(forms,sep = "",collapse = ",")}
  if(!is.null(records)){post_body$records<-paste(records,sep = "",collapse = ",")}
  if(!is.null(upload_file)) {
    post_body$file <- httr::upload_file(upload_file)
    names(post_body)[which(names(post_body)=="records")]<-"record"
    names(post_body)[which(names(post_body)=="fields")]<-"field"
  }
  if(is.null(action)) {action <- ""}
  if (content == "record" && action == "") {
    vari_list <-  redcap_api_call(redcap_uri= redcap_uri,post_body = post_body[which(names(post_body)!="fields")],content = "exportFieldNames")
    record_list <-  redcap_api_call(redcap_uri= redcap_uri,post_body = post_body,content = "record",fields=vari_list$output$original_field_name[1],action = "record_single_run")
    
    if (nrow( record_list$output) > batch_size) {
      return(redcap_get_large_records(redcap_uri= redcap_uri,post_body = post_body,record_list = record_list,batch_size = batch_size,carryon = carryon))
    }
  }
  start_time <- Sys.time()
  result <- httr::POST(url = redcap_uri, body = post_body,config = httr_config)
  raw_text <- httr::content(result, "text")
  if(result$status != 200L || any(is.na(raw_text))) {
    message("redcap api call failed\n",raw_text)
    return(list(output=raw_text,success=FALSE))
    }
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  simple_df_contents <- c("formEventMapping","metadata","event","exportFieldNames","participantList","project","instrument","user","record","generateNextRecordName")
  
  if(content %in% simple_df_contents && action %in% c("record_single_run","")) {
    try(ds <- utils::read.csv(text = raw_text, stringsAsFactors = FALSE), silent = TRUE)
    if (!exists("ds")){
      ds <- raw_text
    } else if (inherits(ds, "data.frame")) {
      if(nrow(ds)<1){
        return(list(output=raw_text,success=TRUE))
      } else {
        return(list(output=ds,success=TRUE))
      }
      
    } else if (result$status != 200L) {
      message("redcap api call failed (HTTP code is not 200), returning raw text")
      return(list(output=ds,success=FALSE))
    } else {
      return(output=ds,success=TRUE)
    }
  } else if (action == "export" || content %in% c("pdf")) {
   stop("Function not yet available.")
  } else if (content == "records" && action == "delete"){
    stop("Function not yet available.")
  } else if (content == "file") {
    return(list(output=raw_text,success=FALSE))
  }
  return(list(output=ds,success=FALSE))
}


redcap_get_large_records <- function(redcap_uri = NULL,post_body=NULL,record_list=NULL,batch_size=1000L,carryon = FALSE ) {
  record_list$output$count<-ceiling(1:nrow(record_list$output)/batch_size)
  records_evt_fixed<-do.call(rbind,lapply(split(record_list$output,record_list$output$registration_redcapid),function(dfx){
    if(length(unique(dfx$count))!=1) {
      dfx$count<-round(median(dfx$count),digits = 0)
      }
    return(dfx)
    }))
  records_sp<-split(records_evt_fixed,records_evt_fixed$count)
  message("pulling large records in batchs")  
  ifTerminate <- FALSE
  output_sum<-cleanuplist(lapply(records_sp,function(tgt){
    if(ifTerminate && !carryon) {return(NULL)}
    message("pulling batch ",unique(tgt$count)," out of ",max(records_evt_fixed$count))
    output<-redcap_api_call(redcap_uri = redcap_uri, post_body = post_body, 
                            content = "record",
                            records = unique(tgt$registration_redcapid),
                            action = "record_single_run")
    if(!output$success || !is.data.frame(output$output)) {
      message("failed, error message is: ",output$output)
      ifTerminate <<- TRUE
      return(list(IDarray = unique(tgt$registration_redcapid),batch_number = unique(tgt$count),success = FALSE,output = output$output))
      }
    return(output$output)
  }))
  if(!ifTerminate) {
    return(list(output = do.call(rbind,output_sum),success = TRUE))
  } else if (carryon) {
    message("the function carried on after encounter error. will return reminder and error informations.")
    output_isDone <- split(output_sum,sapply(output_sum,is.data.frame))
    return(list(output = do.call(rbind,output_isDone$`TRUE`),success = FALSE, error_outcome = output_isDone$`FALSE`))
  } else {
    return(list(output = NULL, success = FALSE, error_outcome = last(output_sum)))
  }
}


redcap_upload<-function (ds_to_write, batch_size = 100L, interbatch_delay = 0.5, retry_whenfailed=T,
                         continue_on_error = FALSE, redcap_uri, token, verbose = TRUE, NAoverwrite = F,
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
                                          NAoverwrite = NAoverwrite,
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

redcap_oneshot_upload<-function (ds, redcap_uri, token, verbose = TRUE, NAoverwrite = F,config_options = NULL,retry_whenfailed=F,previousIDs=NULL) {
  overwriteBehavior = ifelse(NAoverwrite,"overwrite","normal")
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
                    type = "flat", data = csv, overwriteBehavior = overwriteBehavior, 
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

redcap_seq_uplaod<-function(ds=NULL,id.var=NULL,redcap_uri,token,batch_size=1000L) {
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