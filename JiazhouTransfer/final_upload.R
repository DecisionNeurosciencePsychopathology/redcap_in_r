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










 























