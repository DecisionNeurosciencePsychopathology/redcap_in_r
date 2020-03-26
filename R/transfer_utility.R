##Transfer utility:

map_evt_single <- function(dfk=NULL,look_up=NULL,max_allowed=NULL,evt_allowed=NULL) {
  dfk$CDATE <- as.Date(dfk$CDATE)
  ID <- as.character(unique(dfk$registration_redcapid))
  lk <- look_up[[ID]]
  lk <- lk[which(lk$EVT %in% evt_allowed),]
  max_num = NA
  leftover_df = NULL
  if(is.null(lk)){
    #message("ID: ",unique(dfk$registration_redcapid),"; NO LOOKUP")
    return(list(output=NULL,code="NO_LOOKUP",leftover_df=NULL,max_num=max_num,ID=ID))
  } else if (nrow(lk)<1) {
    return(list(output=NULL,code="LOOKUP_NO_ALLOWED",leftover_df=NULL,max_num=max_num,ID=ID))
  }
  #Maybe we just stop getting the additional ones for now ?
  #Only do the direct match first. 
  dfk$EVT <- lk$EVT[match(dfk$CDATE,lk$CDATE)]
  
  
  dfj<-dfk[is.na(dfk$EVT),]
  lk$USED <- lk$EVT %in% dfk$EVT
  lk_search <- lk[!lk$USED,]
  is_done = nrow(dfj)<1 || nrow(lk_search)<1
  #If it's all perfect match then great!
  if(is_done) {
    code = "EXACT_MATCH"
  } else {
    lk_search$TIC[is.na(lk_search$TIC)]<- 0
    lk_sg<-do.call(rbind,lapply(1:nrow(lk_search),function(a){
      data.frame(EVT=lk_search$EVT[a],CDATE=lk_search$TIC[[a]] + lk_search$CDATE[a],OG_pos = a,stringsAsFactors = F)
    }))
  }
  #If it's not all perfect matched then oh well...let's try alternative 
  
  #First Alternative is to use the TIC
  if (!is_done && any(!is.na(lk_search$TIC))){
    #Expand the CDATE to range so we can match again
    dfj$EVT <- lk_sg$EVT[match(dfj$CDATE,lk_sg$CDATE)]
    dfk <- rbind(dfk[which(!is.na(dfk$EVT)),],dfj)
    dfj<-dfk[is.na(dfk$EVT),]
    lk$USED <- lk$EVT %in% dfk$EVT
    lk_search <- lk[!lk$USED,]
    is_done = nrow(dfj)<1 || nrow(lk_search)<1
    code = "EXACT_MATCH_a"
  } 
  #Second Alternative is to use aproximation, using the max_allowed argument as the maximum date difference
  if (!is_done) {
    dfj<-cbind(dfj,do.call(rbind,lapply(1:nrow(dfj),function(b){
      bx <- data.frame(dd=abs(dfj$CDATE[b] - lk_sg$CDATE),prox_EVT = lk_sg$EVT,stringsAsFactors = F)
      bx <- bx[which.min(bx$dd),]
      rownames(bx)<-NULL
      return(bx)
    })))
    if (any(dfj$dd <= max_allowed)){
      dfz <- dfj[which(dfj$dd <= max_allowed),]
      dfz<-do.call(rbind,lapply(split(dfz,dfz$prox_EVT),function(ax){
        ax[which.min(ax$dd),]
      }))
      dfz$EVT <- dfz$prox_EVT
      max_num =  max(dfz$dd)
      dfz <- dfz[names(dfk)]
      rownames(dfz)<-NULL
      dfk <- rbind(dfk[which(!is.na(dfk$EVT)),],dfz)
      dfj<-dfk[is.na(dfk$EVT),]
      lk$USED <- lk$EVT %in% dfk$EVT
      lk_search <- lk[!lk$USED,]
      is_done = nrow(dfj)<1 || nrow(lk_search)<1
      code = paste0("PROXI_MATCH_",max_allowed)
      
    } 
  } 
  
  if(!is_done) {
    code = paste0("NO_MATCH_MAXOUT")
    max_num = max(dfj$dd)
    dfk<-dfk[!is.na(dfk$EVT),]
    dfj<-dfk[is.na(dfk$EVT),]
    leftover_df = dfj
  } 
  dfk<-dfk[!is.na(dfk$EVT),]
  #message("ID: ",unique(dfk$registration_redcapid),"; ",code )
  return(list(output=dfk,code=code,max_num=max_num,leftover_df=leftover_df,ID=ID))
}

# id.var = "registration_redcapid"
# idmap = learn_idmap
# idmap_id = "masterdemo_id"
# metadata = protect$metadata
# target_ptc = ptcs$protect
# ID_list = p2_only_ID
# skip_check = F
# toignore = TRUE
# exempt_code = c(999,99,"")
# x_data = NULL
# look_up = sp_rctogo
# max_allowed = 7
# x_data=NULL
# error_outdir=NULL

upload_transfer<-function(xpath,x_data=NULL,eventdata=NULL,error_outdir=NULL,id.var=NULL,idmap=NULL,idmap_id=NULL,metadata=NULL,target_ptc=NULL,target_evt=NULL,ID_list=NULL,toignore=FALSE,skip_check=FALSE,exempt_code=NULL,look_up=NULL,max_allowed = 7) {
  #Read in data;
  if(is.null(x_data)){
    print(basename(xpath))
    if(!grepl(".csv$",xpath)){message("skip");return(NULL)}
    x_data<-read.csv(xpath,stringsAsFactors = F)
  }
  if(is.null(error_outdir)){
    error_outdir = file.path(dirname(xpath),gsub(":","",gsub("-","",gsub(" ","_",Sys.time()))))
  }
  dir.create(file.path(error_outdir,"Problems"),showWarnings = F,recursive = T)
  #Clean up and use ID map for ID matching
  x_data$X<-NULL
  x_data_findid <- bsrc.findid(df = x_data,idmap = idmap,id.var = id.var)
  x_data_sp <- split(x_data_findid,x_data_findid$ifexist)
  #write non-existing ones to folder to figure it out later 
  if(!is.null(x_data_sp$`FALSE`)){
    message("There is a problem with ID matching.")
    write.csv(x_data_sp$`FALSE`,file = file.path(error_outdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_noidmatch.csv")),row.names = F)
  } else {
    message("All ID matched!")
  }
  df_togo<-x_data_sp$`TRUE`
  #Get the variable mapping first;
  vari_ref<-data.frame(variable_name = names(df_togo),stringsAsFactors = F)
  vari_ref$no_tag <- gsub("___.*$","",vari_ref$variable_name)
  vari_ref$form_name <- metadata$form_name[match(vari_ref$no_tag,metadata$field_name)]
  vari_ref$form_name[vari_ref$variable_name %in% c("redcap_event_name","registration_redcapid")] <- "INFO"
  fnames <- as.character(na.omit(unique(vari_ref$form_name[vari_ref$variable_name != id.var])))
  fnames <- fnames[fnames!="INFO"]
  message(fnames)
  
  evt_allowed <- eventdata$unique_event_name[eventdata$form %in% fnames]
  df_tg_sp <- split(df_togo,df_togo$registration_redcapid)
  
  dir.create(file.path(error_outdir,"EVT_Summary","Problems"),showWarnings = F,recursive = T)
  multi_evt<-lapply(df_tg_sp,map_evt_single,look_up=look_up,max_allowed=max_allowed,evt_allowed=evt_allowed)
  mevt_report_df<-data.frame(ID=sapply(multi_evt,`[[`,"ID"),
                             code=sapply(multi_evt,`[[`,"code"),
                             max_num=sapply(multi_evt,`[[`,"max_num"),stringsAsFactors = F)
  df_togo <- do.call(rbind,lapply(multi_evt,`[[`,"output"))
  df_leftover <- do.call(rbind,lapply(multi_evt,`[[`,"leftover_df"))
  write.csv(mevt_report_df,file = file.path(error_outdir,"EVT_Summary",paste0(gsub(".csv","",basename(xpath)),"_EVT_SUMMARY.csv")),row.names = F)
  if(!is.null(df_leftover) && nrow(df_leftover)>0 ){
    write.csv(df_leftover,file = file.path(error_outdir,"EVT_Summary","Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_noidmatch.csv")),row.names = F)
  }
  if(is.null(df_togo) || nrow(df_togo)<1 ){
   return(NULL)
  }
  df_togo$redcap_event_name <- df_togo$EVT
  do_multi <- length(unique(df_togo$redcap_event_name))>1
  
  if (is.null(target_evt)) {
    target_evt <- unique(df_togo$redcap_event_name)
  }
  #Rename ID variables:
  
  df_togo$registration_redcapid<-df_togo[[idmap_id]]
  df_togo<-df_togo[which(!names(df_togo) %in% c("ogid","ifexist",names(idmap)))]
  ID_pos<-match(id.var,names(df_togo))
  
  vari_ref<-data.frame(variable_name = names(df_togo),stringsAsFactors = F)
  vari_ref$no_tag <- gsub("___.*$","",vari_ref$variable_name)
  vari_ref$form_name <- metadata$form_name[match(vari_ref$no_tag,metadata$field_name)]
  vari_ref$form_name[vari_ref$variable_name %in% c("redcap_event_name","registration_redcapid")] <- "INFO"
  #Finding appropriate data to pull;
  vari_ref_sp<-split(vari_ref,is.na(vari_ref$form_name))
  if(!is.null(vari_ref_sp$`TRUE`)){
    message("These variables are not included in the uploading because they have no match for any form on redcap. \n",
            paste(vari_ref_sp$`TRUE`$variable_name,collapse = ", "))
    write.csv(df_togo[c(id.var,vari_ref_sp$`TRUE`$variable_name)],
              file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_no_form_matched.csv")),row.names = F)
    
  } else {
    message("Every variables are accounted for!")
  }
  df_togo <- df_togo[which(names(df_togo) %in% vari_ref_sp$`FALSE`$variable_name)]
  
  if(toignore){
    df_togo<-df_togo[!df_togo$registration_redcapid %in% ID_list,]
  } else {
    df_togo<-df_togo[df_togo$registration_redcapid %in% ID_list,]
  }
  
  
  
  og_forms <- bsrc.getform(protocol = target_ptc,formname = fnames,online = T,batch_size = 1000L,mod = T,at_least = 1,filter_events = target_evt)

  if(!is.null(og_forms) && nrow(og_forms)!=0) {
    message("Checking against existing data...")
    if(do_multi){
      df_togo$IDFIELD <- paste(df_togo$registration_redcapid,df_togo$redcap_event_name,sep = "_x_")
      og_forms$IDFIELD <- paste(og_forms$registration_redcapid,og_forms$redcap_event_name,sep = "_x_")
      df_togo$registration_redcapid<-NULL;df_togo$redcap_event_name<-NULL
      og_forms$registration_redcapid<-NULL;og_forms$redcap_event_name<-NULL
    } else {
      
      df_togo$IDFIELD <-df_togo$registration_redcapid
      og_forms$IDFIELD <- og_forms$registration_redcapid
      df_togo$registration_redcapid<-NULL
      og_forms$registration_redcapid<-NULL
    }
    
    
    
    df_togo_b <- df_togo[,which(names(df_togo) %in% names(og_forms))]
    
    if(any(table(df_togo_b[["IDFIELD"]])>1)){
      dup_id<-names(which(table(df_togo_b[["IDFIELD"]]) > 1))
      mp_df<-df_togo_b[which(df_togo_b[["IDFIELD"]] %in% names(which(table(df_togo_b[["IDFIELD"]]) > 1))),]
      message(nrow(mp_df)," records and ",length(unique(mp_df[["IDFIELD"]]))," IDs have duplicated form. Removed");
      write.csv(mp_df,file = file.path(error_outdir,paste0(gsub(".csv","",basename(xpath)),"_problem_multiple_entry.csv")),row.names=F)
      
      df_togo_b <- df_togo_b[which(!df_togo_b$IDFIELD %in% dup_id),]
    } 
    
    if(nrow(df_togo_b)<1){message("nothing to check...ending");return(NULL)}
    df_tg_v<-bsrc.verify(df_new = df_togo_b,df_ref = og_forms,id.var = "IDFIELD",exempt_code=exempt_code)
    
    #Write out both non-NAs 
    if(any(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW))){
      message("Value conflict identified")
      write.csv(x = df_tg_v$DIFF[which(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW)),],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_valueconflict.csv")),row.names = F)
    }
    
    if(!skip_check){
      df_toupload<-df_tg_v$NEW_INFO
      if(is.null(df_toupload) || nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
      #df_toupload$redcap_event_name<-unique(og_forms$redcap_event_name)
    } else {
      df_toupload <- df_togo_b
      if(nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
    }
    
    if(do_multi){
      df_toupload$IDFIELD <- strsplit(df_toupload$IDFIELD,"_x_")
      df_toupload$registration_redcapid <- sapply(df_toupload$IDFIELD,`[[`,1)
      df_toupload$redcap_event_name <- sapply(df_toupload$IDFIELD,`[[`,2)
      df_toupload$IDFIELD<-NULL
      seq_id = c("registration_redcapid","redcap_event_name")
    } else {
      df_toupload$registration_redcapid <- df_toupload$IDFIELD
      df_toupload$redcap_event_name <- target_evt
      seq_id = c("registration_redcapid")
      df_toupload$IDFIELD<-NULL
    }
    
    #return(df_toupload)
    gx<-redcap_seq_uplaod(ds = df_toupload,id.var = seq_id,redcap_uri = target_ptc$redcap_uri,token = target_ptc$token)
  } else {
    if(nrow(df_togo)<1){message("nothing to upload");return(NULL)}
    message("No data found in destination, directly inject...")
    if(!do_multi){
      df_togo$redcap_event_name <- target_evt
    }
    print(unique(df_togo$redcap_event_name))
    seq_id = c("registration_redcapid","redcap_event_name")
    #return(df_togo)
    gx<-redcap_seq_uplaod(ds = df_togo,id.var = seq_id,redcap_uri = target_ptc$redcap_uri,token = target_ptc$token)
  }
  
  return(list(outcome=gx,og_data=og_forms))
}

