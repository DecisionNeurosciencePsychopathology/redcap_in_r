####B-Social Transfer post cleaning:

rootdir = "/Users/jiazhouchen/Box/skinner/data/Redcap\ Transfer/BSOCIAL\ transfer/datacleaning_FINAL"
dir.create(file.path(rootdir,"Problems"))
bsocial<-bsrc.checkdatabase2(protocol = ptcs$bsocial)
metadata<-bsocial$metadata
f_paths<-list.files(rootdir,full.names = T,include.dirs = F)

bs_idmap <- bsrc.getform(protocol = ptcs$bsocial,formname = "record_registration",online = T,batch_size = 1000L)[c("registration_redcapid","registration_soloffid")]
names(bs_idmap)<-c("bsocial_rc_id","soloff_id")



outcome<-lapply(f_paths,function(xa) {
  print(basename(xa))
  if(!grepl(".csv$",xa)){messsage("skip");return(NULL)}
  x_data<-read.csv(xa,stringsAsFactors = F)
  
  x_data$X<-NULL
  x_data_findid <- bsrc.findid(df = x_data,idmap = bs_idmap,id.var = "registration_redcapid")
  #Deal with existing people first
  x_data_sp <- split(x_data_findid,x_data_findid$ifexist)
  #write non-existing ones to folder to figure it out later 
  if(!is.null(x_data_sp$`FALSE`)){
    message("There is a problem with ID matching.")
    write.csv(x_data_sp$`FALSE`,file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_noidmatch.csv")),row.names = F)
  }
  
  
  df_togo<-x_data_sp$`TRUE`
  df_togo$registration_redcapid<-df_togo$bsocial_rc_id
  df_togo<-df_togo[which(!names(df_togo) %in% c("ogid","ifexist",names(bs_idmap)))]
  ID_pos<-match("registration_redcapid",names(df_togo))
  
  f_names<-unique(metadata$form_name[match(names(df_togo),metadata$field_name)])
  og_forms<-bsrc.getform(protocol = ptcs$bsocial,formname = f_names[f_names!="record_registration"],online = T,batch_size = 1000L,mod = T,at_least = 1)
  
  if(length(unique(og_forms$redcap_event_name))>1){message("multiple events uploading is not supported yet. Will terminate now.");return(NULL)}
  #Write out other problems:
  if(length(which(!names(df_togo) %in% names(og_forms)))>0){
    message("Failed to intake variable: ", paste(names(df_togo)[which(!names(df_togo) %in% names(og_forms))],collapse = "; "))
    write.csv(df_togo[c(ID_pos,which(!names(df_togo) %in% names(og_forms)))],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_nointake.csv")),row.names=F)
  }
  
  df_togo_b <- df_togo[which(names(df_togo) %in% names(og_forms))]
  if(any(table(df_togo_b$registration_redcapid)>1)){
    dup_id<-names(which(table(df_togo_b$registration_redcapid) > 1))
    mp_df<-df_togo_b[which(df_togo_b$registration_redcapid %in% names(which(table(df_togo_b$registration_redcapid) > 1))),]
    message(nrow(mp_df)," records and ",length(unique(mp_df$registration_redcapid))," IDs have duplicated form. Removed");
    write.csv(mp_df,file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_multiple_entry.csv")),row.names=F)
    
    df_togo_b <- df_togo_b[which(!df_togo_b$registration_redcapid %in% dup_id),]
  }
  return(NULL)
  df_tg_v<-bsrc.verify(df_new = df_togo_b,df_ref = og_forms,id.var = "registration_redcapid")
  
  #Write out both non-NAs 
  if(any(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW))){
    message("Value conflict identified")
    write.csv(x = df_tg_v$DIFF[which(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW)),],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_valueconflict.csv")),row.names = F)
  }
  
  df_toupload<-df_tg_v$DIFF[is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW),]
  if(nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
  df_toupload<-reshape2::dcast(df_toupload,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")
  df_toupload$redcap_event_name<-unique(og_forms$redcap_event_name)

  gx<-redcap_seq_uplaod(ds = df_toupload,id.var = "registration_redcapid",redcap_uri = ptcs$bsocial$redcap_uri,token = ptcs$bsocial$token)
  
  return(list(outcome=gx,og_data=og_forms))
})




#Protect Version:

#Import Baseline:
rootdir <- "~/Box/skinner/data/Redcap Transfer/PT transfer/Baseline/"
dir.create(file.path(rootdir,"Problems"),showWarnings = F,recursive = T)
protect<-bsrc.checkdatabase2(protocol = ptcs$protect)
masterdemo_reg <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L)
learn_idmap <- masterdemo_reg[c("registration_redcapid","registration_wpicid")]
names(learn_idmap)<-c("masterdemo_id","wpic_id")

ptc_map<-bsrc.checkbox(variablename = "registration_ptcstat",dfx = masterdemo_reg)
cons_p2 <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,"protect2"),any)
cons_legacy <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,c("protect","suicid2","suicide")),any)
p2_only_ID <- ptc_map$registration_redcapid[cons_p2 & !cons_legacy]


f_paths<-list.files(rootdir,full.names = T,include.dirs = F)


#Do non-p2 people first:
lapply(f_paths,upload_transfer,
       id.var = "registration_redcapid",
       idmap = learn_idmap,
       idmap_id = "masterdemo_id",
       metadata = protect$metadata,
       target_ptc = ptcs$protect,
       target_evt = "baseline_arm_1",
       ID_list = p2_only_ID,skip_check = T,
       toignore = TRUE)

id.var = "registration_redcapid"
idmap = learn_idmap
idmap_id = "masterdemo_id"
metadata = protect$metadata
target_ptc = ptcs$protect
target_evt = "baseline_arm_1"
ID_list = p2_only_ID
skip_check = F
toignore = TRUE

upload_transfer<-function(xpath,id.var,idmap,idmap_id,metadata,target_ptc,target_evt,ID_list,toignore,skip_check) {
  #Read in data;
  print(basename(xpath))
  if(!grepl(".csv$",xpath)){message("skip");return(NULL)}
  x_data<-read.csv(xpath,stringsAsFactors = F)
  #Clean up and use ID map for ID matching
  x_data$X<-NULL
  x_data_findid <- bsrc.findid(df = x_data,idmap = idmap,id.var = id.var)
  x_data_sp <- split(x_data_findid,x_data_findid$ifexist)
  #write non-existing ones to folder to figure it out later 
  if(!is.null(x_data_sp$`FALSE`)){
    message("There is a problem with ID matching.")
    write.csv(x_data_sp$`FALSE`,file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_noidmatch.csv")),row.names = F)
  } else {
    message("All ID matched!")
  }
  
  #Rename ID variables:
  df_togo<-x_data_sp$`TRUE`
  df_togo$registration_redcapid<-df_togo[[idmap_id]]
  df_togo<-df_togo[which(!names(df_togo) %in% c("ogid","ifexist",names(idmap)))]
  ID_pos<-match(id.var,names(df_togo))
  
  #Finding appropriate data to pull;
  vari_ref<-data.frame(variable_name = names(df_togo),stringsAsFactors = F)
  vari_ref$no_tag <- gsub("___.*$","",vari_ref$variable_name)
  vari_ref$form_name <- metadata$form_name[match(vari_ref$no_tag,metadata$field_name)]
  vari_ref_sp<-split(vari_ref,is.na(vari_ref$form_name))
  if(!is.null(vari_ref_sp$`TRUE`)){
    message("These variables are not included in the uploading because they have no match for any form on redcap. \n",
            paste(vari_ref_sp$`TRUE`$variable_name,collapse = ", "))
    write.csv(df_togo[c(id.var,vari_ref_sp$`TRUE`$variable_name)],
              file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_no_form_leftout.csv")),row.names = F)
   
  } else {
    message("Every variables are accounted for!")
  }
  df_togo <- df_togo[which(names(df_togo) %in% vari_ref_sp$`FALSE`$variable_name)]
  if(toignore){
    df_togo<-df_togo[!df_togo$registration_redcapid %in% ID_list,]
  } else {
    df_togo<-df_togo[df_togo$registration_redcapid %in% ID_list,]
  }
  
  fnames <- as.character(na.omit(unique(vari_ref$form_name[vari_ref$variable_name != id.var])))

  og_forms <- bsrc.getform(protocol = target_ptc,formname = fnames,online = T,batch_size = 1000L,mod = T,at_least = 1,filter_events = target_evt)
  
  if(!is.null(og_forms) && nrow(og_forms)!=0) {
    message("Checking against existing data points...")
    if(length(unique(og_forms$redcap_event_name))>1){message("multiple events uploading is not supported yet. Will terminate now.");return(NULL)}
    
    # #Write out other problems:
    # if(length(which(!names(df_togo) %in% names(og_forms)))>0){
    #   message("Failed to intake variable: ", paste(names(df_togo)[which(!names(df_togo) %in% names(og_forms))],collapse = "; "))
    #   write.csv(df_togo[c(ID_pos,which(!names(df_togo) %in% names(og_forms)))],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_nointake.csv")),row.names=F)
    # }
    
    df_togo_b <- df_togo[which(names(df_togo) %in% names(og_forms))]
    
    if(any(table(df_togo_b[[id.var]])>1)){
      dup_id<-names(which(table(df_togo_b[[id.var]]) > 1))
      mp_df<-df_togo_b[which(df_togo_b[[id.var]] %in% names(which(table(df_togo_b[[id.var]]) > 1))),]
      message(nrow(mp_df)," records and ",length(unique(mp_df[[id.var]]))," IDs have duplicated form. Removed");
      write.csv(mp_df,file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xa)),"_problem_multiple_entry.csv")),row.names=F)
      
      df_togo_b <- df_togo_b[which(!df_togo_b$registration_redcapid %in% dup_id),]
    } 
    df_tg_v<-bsrc.verify(df_new = df_togo_b,df_ref = og_forms,id.var = id.var)
    
    #Write out both non-NAs 
    if(any(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW))){
      message("Value conflict identified")
      write.csv(x = df_tg_v$DIFF[which(!is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW)),],file = file.path(rootdir,"Problems",paste0(gsub(".csv","",basename(xpath)),"_problem_valueconflict.csv")),row.names = F)
    }
    
    if(!skip_check){
      df_toupload<-df_tg_v$DIFF[is.na(df_tg_v$DIFF$REF) & !is.na(df_tg_v$DIFF$NEW),]
      if(is.null(df_toupload) || nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
      df_toupload<-reshape2::dcast(df_toupload,formula = registration_redcapid ~ variable, drop = T,value.var = "NEW")
      df_toupload$redcap_event_name<-unique(og_forms$redcap_event_name)
    } else {
      df_toupload <- df_togo_b
      if(nrow(df_toupload)<1){message("nothing to upload");return(NULL)}
    }
    

    gx<-redcap_seq_uplaod(ds = df_toupload,id.var = id.var,redcap_uri = target_ptc$redcap_uri,token = target_ptc$token)
  } else {
    message("No data found in destination, directly inject...")
    gx<-redcap_seq_uplaod(ds = df_togo,id.var = id.var,redcap_uri = target_ptc$redcap_uri,token = target_ptc$token)
  }
  
  return(list(outcome=gx,og_data=og_forms))
}
 























