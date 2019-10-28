#Transfer utility:
#Only functional scripts are put here:

bsrc.verify<-function(df_new=NULL,df_ref=NULL,id.var=NULL){
  if(any(!names(df_new) %in% names(df_ref))) {stop("New data frame has variables that is not in the RC one. Clean before input.")}
  df_ref <- df_ref[names(df_new)] 
  
  df_new$TYPE<-"NEW"
  df_ref$TYPE<-"REF"
  
  df_outcomp<-do.call(rbind,lapply(list(df_ref,df_new),reshape2::melt,id.var=c(id.var,"TYPE")))
  df_comp<-reshape2::dcast(df_outcomp,formula =  as.formula(paste(paste(c(id.var,"variable"),collapse = "+"),"~ TYPE")),value.var = "value",fill = NA)
  
  is_both_na <- is.na(df_comp$NEW) & is.na(df_comp$REF)
  is_same_value <- df_comp$NEW == df_comp$REF
  is_same_value[is.na(is_same_value)] <- is_both_na[is.na(is_same_value)]
  
  df_comp_sp<-split(df_comp,ifelse(is_same_value,"SAME","DIFF"))
  return(df_comp_sp)
}