# get the correct range
choice_map<-bsrc.getchoicemapping(variablenames = "registration_gender",protocol = ptcs$masterdemo)

df_new$registration_gender [!df_new$registration_gender %in% choice_map$choice.code]

# verify: 
#1. All variables in the new data frame are in the RC
#2.
bsrc.verify<-function(df_new=NULL,df_ref=NULL,id.var=NULL){
  if(any(!names(df_new) %in% names(df_ref))) {stop("New data frame has variables that is not in the RC one. Clean before input.")}
  df_ref <- df_ref[names(df_new)] 
  df_outcomp<-do.call(rbind,lapply(list(df_ref,df_new),reshape2::melt,id.var=c(id.var,"TYPE")))
  df_comp<-reshape2::dcast(df_outcomp,formula =  as.formula(paste(paste(c(id.var,"variable"),collapse = "+"),"~ TYPE")),value.var = "value",fill = NA)
  
  is_both_na <- is.na(df_comp$NEW) & is.na(df_comp$REFERENCE)
  is_same_value <- df_comp$NEW == df_comp$REFERENCE
  is_same_value[is.na(is_same_value)] <- is_both_na[is.na(is_same_value)]
  
  df_comp_sp<-split(df_comp,ifelse(is_same_value,"SAME","DIFF"))
  return(df_comp_sp)
}

