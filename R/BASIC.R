#Basics:
redcap_uri=ptcs$protect$redcap_uri
token=ptcs$protect$token

action = NULL
content = NULL
arms = NULL
records = NULL
fields = NULL

message = TRUE

#' @param overwriteBehavior Character string.  'normal' prevents blank
#' #'   fields from overwriting populated fields.  'overwrite' causes blanks to
#' #'   overwrite data in the REDCap database.
#' #' @param returnContent Character string.  'count' returns the number of
#' #'   records imported; 'ids' returns the record ids that are imported;
#' #'   'nothing' returns no message.
#' #' @param returnData Logical.  Prevents the REDCap import and instead
#' #'   returns the data frame that would have been given
#' #'   for import.  This is sometimes helpful if the API import fails without
#' #'   providing an informative message. The data frame can be written to a csv
#' #'   and uploaded using the interactive tools to troubleshoot the
#' #'   problem.  Please shoot me an e-mail if you find errors I havne't
#' #'   accounted for.



redcap_api_call<-function (redcap_uri=NULL, token=NULL,
                           action = NULL, content = NULL,arms = NULL,records = NULL, fields = NULL,
                           export_file_path = NULL,
                           message = TRUE,httr_config=NULL) {
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
  if(!is.null(action)){}
  if(!is.null(arms)){}
  if(!is.null(records)){}
  if(!is.null(fields)){}
  start_time <- Sys.time()
  result <- httr::POST(url = redcap_uri, body = post_body,config = httr_config)
  raw_text <- httr::content(result, "text")
  if(result$status != 200L) {message("redcap api call failed\n",raw_text);return(raw_text)}
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  simple_df_contents <- c("formEventMapping","metadata","event","exportFieldNames","participantList","project","instrument","user")
  
  if(content %in% simple_df_contents && is.null(action)) {
    try(ds <- utils::read.csv(text = raw_text, stringsAsFactors = FALSE), 
        silent = TRUE)
    if (exists("ds") & inherits(ds, "data.frame")) {return(ds)} else {
      if(result$status != 200L) {message("redcap api call failed at converting stage, returning raw text");return(raw_text)}
    }
  } else if (action == "export" || content == "pdf") {
    
  } else if (content == "records" && is.null(action)) {
    
  }
  return(raw_text)
}

