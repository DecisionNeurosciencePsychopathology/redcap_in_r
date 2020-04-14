###Scoring;

#wrapper:
bsrc.score_this_form<-function(df_in=NULL,formname=NULL,...){
  argu <- list(...)
  argu$df_in = df_in
  score_func<-get(paste0("score_",formname),envir = loadNamespace("bsrc"))
  return(do.call(score_func,argu))
}

# protect <- bsrc.checkdatabase2(protocol = ptcs$protect)
# 
df_in = bsrc.getform(formname = "ssi_scale_of_suicidal_ideation",curdb = protect)

df_in<-bsrc.matchIDDate(dfx = df_in,db = protect)


score_bis36 <- function(df_in=NULL,...){
  message("This version of the scoring will only consider the 30 items that are in the \n 
          actual BIS-11, this leave out 6 items  ")
  return(df_in)
}

score_ssi_scale_of_suicidal_ideation <- function(df_in=NULL,aggregate_by_subj=FALSE,agg_func=max(),...){
  if(is.null(df_in$date) && aggregate_by_subj) {
    aggregate_by_subj <- FALSE
    message("df_in object must include date variable to enable aggregating SSI by subject, disabled for now.")
  } 
  vari_name <- c("ssi_1","ssi_2","ssi_3","ssi_4","ssi_5","ssi_6","ssi_7","ssi_8","ssi_9","ssi_10","ssi_11","ssi_12","ssi_13","ssi_14","ssi_15","ssi_16","ssi_17","ssi_18","ssi_19")
  type_name <- c("worst","curr")
  dta<-data.frame(do.call(cbind,lapply(type_name,function(xa){
    dat <- df_in[paste(vari_name,xa,sep = "_")]
    #doing | statment becasue %in% c('') doesn't work 
    dat[dat == "" | dat == "na" | dat == "refuse" | dat == "dk"] <- NA
    apply(dat,1,function(x){sum(as.numeric(x),na.rm = T)})  
  })))
  names(dta) <- paste("ssi_score",type_name,sep = "_")
  df_out <- cbind(df_in[1:(min(grep("ssi_1",names(df_in)))-1)],dta)
  if(aggregate_by_subj) {
    df_out$date <- df_in$date

    df_out<-do.call(rbind,lapply(split(df_out,df_out$registration_redcapid),function(dfb){
      #message(unique(dfb$registration_redcapid))
      dff<-dfb[which.max(dfb$ssi_score_worst),c("registration_redcapid","ssi_score_worst","date")]
      names(dff) <- c("registration_redcapid","ssi_score_worst_lifetime","ssi_wl_date")
      dfh<-dfb[which(dfb$date==max(dfb$date,na.rm = T)),c("ssi_score_curr","date")]
      if(nrow(dfh)<1){dfh[1,]<-NA} 
      names(dfh) <- c("ssi_score_most_recent","ssi_mr_date")
      dfe <- cbind(dff,dfh)
      return(dfe)
    }))
    
  }
  return(df_out)
}

score_uppsp <- function(df_in=NULL,...){
  df_sc <- df_in[names(df_in) %in% c("registration_redcapid","redcap_event_name")]
  #it's just me being lazy ;_;
  
  list(
    negative_urgency = c(2.5,7.5,12.5,17.5,22.5,29.5,34.5,39.5,44.5,50.5,53, 58.5),
    positive_urgency = c(5.5,10.5,15.5,20.5,25.5,30.5,35.5,40.5,45.5,49.5,52.5,54.5,57.5,59.5),
    sensation_seeking = c(3.5,8.5,13.5,18.5,23.5,26.5,31.5,36.5,41.5,46.5,51.5,56.5),
    premeditation =c(1,6,11,16,21,28,33,38,43,48,55),
    perseverance = c(4,9.5,14,19,24,27,32,37,42,47.5)
  )
}

score_neoffi <- function(df_in=NULL,return_subscale=F,...){
  df_sc <- df_in[names(df_in) %in% c("registration_redcapid","redcap_event_name")]
  
  #Neuroticism:
  df_sc$neuroticism_sub_neg_aff <- apply(df_in[c("neoffi_1","neoffi_11","neoffi_16","neoffi_31","neoffi_46")],1,sum,na.rm=T)
  df_sc$neuroticism_sub_self_rep <- apply(df_in[c("neoffi_6","neoffi_21","neoffi_26","neoffi_36","neoffi_41","neoffi_51","neoffi_56")],1,sum,na.rm=T)
  df_sc$neuroticism_total <- df_sc$neuroticism_sub_neg_aff + df_sc$neuroticism_sub_self_rep
  
  #Extraversion
  df_sc$extraversion_sub_pos_aff <- apply(df_in[c("neoffi_7","neoffi_12","neoffi_37","neoffi_42")],1,sum,na.rm=T)
  df_sc$extraversion_sub_soc <- apply(df_in[c("neoffi_2","neoffi_17","neoffi_27","neoffi_57")],1,sum,na.rm=T)
  df_sc$extraversion_sub_act <- apply(df_in[c("neoffi_22","neoffi_32","neoffi_47","neoffi_52")],1,sum,na.rm=T)
  df_sc$extraversion_total <- df_sc$extraversion_sub_pos_aff + df_sc$extraversion_sub_soc + df_sc$extraversion_sub_act
  
  df_sc$openness_sub_aes_int <- apply(df_in[c("neoffi_13","neoffi_23","neoffi_43")],1,sum,na.rm=T)
  df_sc$openness_sub_int_int <- apply(df_in[c("neoffi_48","neoffi_53","neoffi_58")],1,sum,na.rm=T)
  df_sc$openness_sub_unc <- apply(df_in[c("neoffi_3","neoffi_8","neoffi_18","neoffi_38")],1,sum,na.rm=T)
  df_sc$openness_total <- df_sc$openness_sub_aes_int + df_sc$openness_sub_int_int + df_sc$openness_sub_unc
  
  #Agreeableness
  df_sc$agreeableness_sub_nonant_ori <- apply(df_in[c("neoffi_9","neoffi_14","neoffi_19","neoffi_24","neoffi_29","neoffi_44","neoffi_54","neoffi_59")],1,sum,na.rm=T)
  df_sc$agreeableness_sub_prosoci_ori <- apply(df_in[c("neoffi_4","neoffi_34","neoffi_39","neoffi_49")],1,sum,na.rm=T)
  df_sc$agreeableness_total <- df_sc$agreeableness_sub_nonant_ori + df_sc$agreeableness_sub_prosoci_ori
      
  #Conscientiousness
  df_sc$conscientiousness_sub_order <- apply(df_in[c("neoffi_5","neoffi_10","neoffi_15","neoffi_30","neoffi_55")],1,sum,na.rm=T)
  df_sc$conscientiousness_sub_goal_str <- apply(df_in[c("neoffi_25","neoffi_35","neoffi_60")],1,sum,na.rm=T)
  df_sc$conscientiousness_sub_depend <- apply(df_in[c("neoffi_20","neoffi_40","neoffi_45","neoffi_50")],1,sum,na.rm=T)
  df_sc$conscientiousness_total <- df_sc$conscientiousness_sub_order + df_sc$conscientiousness_sub_goal_str + df_sc$conscientiousness_sub_depend
  
  if(!return_subscale) {
    df_sc[which(!grepl("_sub_",names(df_sc)))]
  }
  names(df_sc)[!names(df_sc) %in% c("registration_redcapid","redcap_event_name")] <- paste0("neoffi_",names(df_sc)[!names(df_sc) %in% c("registration_redcapid","redcap_event_name")])
  
  return(df_sc)
}