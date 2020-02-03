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
#' 
#' #' param overwriteBehavior Character string.  'normal' prevents blank
#' #' #'   fields from overwriting populated fields.  'overwrite' causes blanks to
#' #' #'   overwrite data in the REDCap database.
#' #' #' param returnContent Character string.  'count' returns the number of
#' #' #'   records imported; 'ids' returns the record ids that are imported;
#' #' #'   'nothing' returns no message.
#' #' #' param returnData Logical.  Prevents the REDCap import and instead
#' #' #'   returns the data frame that would have been given
#' #' #'   for import.  This is sometimes helpful if the API import fails without
#' #' #'   providing an informative message. The data frame can be written to a csv
#' #' #'   and uploaded using the interactive tools to troubleshoot the
#' #' #'   problem.  Please shoot me an e-mail if you find errors I havne't
#' #' #'   accounted for.



redcap_api_call<-function (redcap_uri=NULL, token=NULL,
                           action = NULL, content = NULL,
                           records = NULL,arms = NULL,events=NULL, forms=NULL, fields = NULL,
                           export_file_path = NULL,batch_size = 200L, carryon = FALSE,
                           message = TRUE,httr_config=NULL,...) {
  #Use this space to document
  #List of Contents:
  #formEventMapping report metadata event participantList exportFieldNames project instrument user instrument generateNextRecordName record pdf file
  #List of Actions:
  #delete export

  if(is.null(redcap_uri) || is.null(token)) {stop("must supply both redcap_uri and token.")}
  if(is.null(content) ) {
    message("no content type supplied, using default: 'record'.")
    content <- "record"
  }
  post_body <- list(token = token, content = content, format = "csv")
  if(!is.null(action) && action!= "record_single_run"){}
  if(!is.null(arms)){}
  
  if(!is.null(events)){post_body$events<-paste(events,sep = "",collapse = ",")}
  if(!is.null(fields)){post_body$fields<-paste(fields,sep = "",collapse = ",")}
  if(!is.null(forms)){post_body$forms<-paste(forms,sep = "",collapse = ",")}
  if(!is.null(records)){post_body$records<-paste(records,sep = "",collapse = ",")}
  
  if(is.null(action)) {action <- ""}
  
  if (content == "record" && action == "") {
    return(redcap_get_large_records(post_body = post_body,batch_size = batch_size,carryon = carryon))
  }
  
  start_time <- Sys.time()
  result <- httr::POST(url = redcap_uri, body = post_body,config = httr_config)
  raw_text <- httr::content(result, "text")
  if(result$status != 200L) {message("redcap api call failed\n",raw_text);return(raw_text)}
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  simple_df_contents <- c("formEventMapping","metadata","event","exportFieldNames","participantList","project","instrument","user","record")
  
  if(content %in% simple_df_contents && action %in% c("record_single_run","")) {
    try(ds <- utils::read.csv(text = raw_text, stringsAsFactors = FALSE), silent = TRUE)
    if (exists("ds") & inherits(ds, "data.frame")) {
      return(list(output=ds,success=TRUE))
    } else if (result$status != 200L) {
      message("redcap api call failed at converting stage, returning raw text")
      return(list(output=ds,success=FALSE))
    }
  } else if (action == "export" || content %in% c("pdf")) {
   stop("Function not yet available.")
  } else if (content == "records" && action == "delete"){
    stop("Function not yet available.")
  }
  return(list(output=ds,success=FALSE))
}

redcap_get_large_records <- function(redcap_uri=NULL, token=NULL,batch_size=1000L,carryon = FALSE ) {
  vari_list <-  redcap_api_call(redcap_uri = redcap_uri, token = token, content = "exportFieldNames")
  record_list <-  redcap_api_call(redcap_uri = redcap_uri, token = token, content = "record",fields=vari_list$output$original_field_name[1],action = "record_single_run")
  record_list$output$count<-ceiling(1:nrow(record_list$output)/batch_size)
  if (length(unique(record_list$output$count))==1) {
    return(list(continue=TRUE))
  }
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
    output<-redcap_api_call(redcap_uri = redcap_uri, token = token, content = "record",records = unique(tgt$registration_redcapid),action = "record_single_run")
    if(!output$success) {
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
