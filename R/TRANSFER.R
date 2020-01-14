#Transfer utility:
#Only functional scripts are put here:




bsrc.verify<-function(df_new=NULL,df_ref=NULL,id.var=NULL,exempt_code=NULL){
  if(any(!names(df_new) %in% names(df_ref))) {stop("New data frame has variables that is not in the RC one. Clean before input.")}
  df_ref <- df_ref[names(df_new)] 
  
  df_new$TYPE<-"NEW"
  df_ref$TYPE<-"REF"

  df_outcomp<-do.call(rbind,lapply(list(df_ref,df_new),reshape2::melt,id.var=c(id.var,"TYPE")))
  df_comp<-reshape2::dcast(df_outcomp,formula =  as.formula(paste(paste(c(id.var,"variable"),collapse = "+"),"~ TYPE")),value.var = "value",fill = NA)
  if(!is.null(exempt_code)){
    message("Exempt code argument is provided, will replace ",paste(exempt_code,collapse = ", ")," with NA.")
    df_comp$REF[df_comp$REF %in% as.character(exempt_code)] <- NA
  }
  is_both_na <- is.na(df_comp$NEW) & is.na(df_comp$REF)
  is_same_value <- as.character(df_comp$NEW) == as.character(df_comp$REF)
  is_same_value[is.na(is_same_value)] <- is_both_na[is.na(is_same_value)]
  
  df_comp_sp<-split(df_comp,ifelse(is_same_value,"SAME","DIFF"))
  
  #Clean up both empty:
  df_comp_sp$SAME<-df_comp_sp$SAME[!is.na(df_comp_sp$SAME$NEW),]
  #clean up things that are NOT in the new entries:
  df_comp_sp$DIFF<-df_comp_sp$DIFF[!is.na(df_comp_sp$DIFF$NEW),]
  #Get value conflict
  df_comp_sp$VALUE_CONFLICT <- df_comp_sp$DIFF[df_comp_sp$DIFF$NEW != df_comp_sp$DIFF$REF & !is.na(df_comp_sp$DIFF$REF),]
  df_comp_sp$NEW_INFO<-df_comp_sp$DIFF[is.na(df_comp_sp$DIFF$REF) & !is.na(df_comp_sp$DIFF$NEW),]
  
  if(is.null(df_toupload) || nrow(df_toupload)<1){message("NO NEW INFO FOUND")}
  df_comp_sp$NEW_INFO<-reshape2::dcast(df_comp_sp$NEW_INFO,formula = as.formula(paste0(paste(id.var,collapse = "+"),"~variable")), drop = T,value.var = "NEW")
  return(df_comp_sp)
}

#####Below are for getting info from the Rx database;
library(httr)
parse_results <- function(result) {
  if(status_code(result) != 200){
    NULL
  } else {
    resContent <- content(result)
    resContent
  }
}

rx_approximateTerm <- function(term, maxEntries = 20, option = 0) {
  params <- list(term = term, maxEntries = maxEntries, option = option)
  r <- GET("https://rxnav.nlm.nih.gov/REST/", path = "REST/approximateTerm.json", query = params)
  parse_results(r)
}

rx_allProperties <- function(rxcui, prop = "all"){
  prams <- list(prop = prop)
  r <- GET("https://rxnav.nlm.nih.gov/REST/", path = paste0("REST/rxcui/", rxcui,"/allProperties"),
           query = prams)
  parse_results(r)
}

get_drug<-function(drugname){
  message(drugname)
  dxt<-rx_approximateTerm(drugname,maxEntries = 3)$approximateGroup$candidate
  c_dxt<-dxt[!duplicated(sapply(dxt,function(xj){xj$rxcui}))]
  m_dxt<-unlist(c_dxt[which.min(sapply(c_dxt,function(xj){xj$rank}))],recursive = F)
  if(length(m_dxt)<1){m_dxt<-list(rxcui=NA,score=NA)}
  return(data.frame(drug_name=drugname,drug_rxcui=m_dxt$rxcui,score=m_dxt$score,stringsAsFactors = F))
}
