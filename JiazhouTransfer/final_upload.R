####B-Social Transfer post cleaning:
if(F) {
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
}

#Protect Version:
protect<-bsrc.checkdatabase2(protocol = ptcs$protect,online = T)
masterdemo_reg <- bsrc.getform(protocol = ptcs$masterdemo,formname = "record_registration",online = T,batch_size = 1000L)
learn_idmap <- masterdemo_reg[c("registration_redcapid","registration_wpicid")]
names(learn_idmap)<-c("masterdemo_id","wpic_id")
ptc_map<-bsrc.checkbox(variablename = "registration_ptcstat",dfx = masterdemo_reg)
cons_p2 <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,"protect2"),any)
cons_legacy <- sapply(sapply(ptc_map$registration_ptcstat,`%in%`,c("protect","suicid2","suicide")),any)
p2_only_ID <- ptc_map$registration_redcapid[cons_p2 & !cons_legacy]
stop("STOP HERE TO SOURCE THE ESSENTIALS")
#Import Baseline:
rootdir <- "~/Box/skinner/data/Redcap Transfer/PT transfer/Baseline/"
dir.create(file.path(rootdir,"Problems"),showWarnings = F,recursive = T)
f_paths<-list.files(rootdir,full.names = T,include.dirs = F)
#Do non-p2 people first:
gx<-lapply(f_paths,upload_transfer,
           id.var = "registration_redcapid",
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           target_ptc = ptcs$protect,
           target_evt = "baseline_arm_1",
           ID_list = p2_only_ID,skip_check = T,exempt_code = c(999,99,""),
           toignore = TRUE)

#Do the p2 folks here:
gx<-lapply(f_paths,upload_transfer,
           id.var = "registration_redcapid",
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           target_ptc = ptcs$protect,
           target_evt = "baseline_arm_2",
           ID_list = p2_only_ID,skip_check = F,exempt_code = c(999,99,""),
           toignore = FALSE)

###First do the single entry ones;
rootdir <- "~/Box/skinner/data/Redcap Transfer/PT transfer/Baseline2/"
dir.create(file.path(rootdir,"F_Problems"),showWarnings = F,recursive = T)

f_paths<-list.files(rootdir,full.names = T,include.dirs = F)
gx<-lapply(f_paths,upload_transfer,error_outdir=file.path(rootdir,"F_Problems"),
           id.var = "registration_redcapid",
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           target_ptc = ptcs$protect,
           target_evt = "baseline_arm_1",
           ID_list = p2_only_ID,skip_check = FALSE,exempt_code = c(999,99),
           toignore = TRUE)
#
# for (xpath in f_paths) {
#   print(xpath)
#   dfx <- read.csv(xpath,stringsAsFactors = F)
#   print(table(table(dfx$registration_redcapid))[1] / nrow(dfx))
# }

#
#
# protect_cur<-bsrc.checkdatabase2(protocol = ptcs$protect,online = T)
#
#
# gxa<-protect_cur$data[which(protect_cur$data$registration_redcapid %in% p2_only_ID & grepl("_arm_1",protect_cur$data$redcap_event_name)),]
# gxa[-c(1,2)] <- ""
# redcap_seq_uplaod(ds = gxa,id.var = id.var,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)

########################
##### Follow - Up ######
########################
##Updating dates to the legacy database:
lookuptable<-readxl::read_xlsx(file.path("~/Box/skinner/data/Redcap Transfer","redcap outputs","Subject Visits Table","S_CONTACTS.xlsx"))
lookup_melt<-reshape2::melt(lookuptable,id.var=c("ID","CDATE","MISSCODE", "RATER", "CONTACT TYPE", "OUTCOME", "MISSINGNESS", "LOCATION1", "LOCATION2", "COMMENT" ))
lookup_melt<-lookup_melt[which(!is.na(lookup_melt$value)),]
lookup_melt$CDATE <- as.Date(lookup_melt$CDATE)
sp_lookup <- split(lookup_melt,lookup_melt$ID)

f_paths<-list.files("~/Box/skinner/data/Redcap Transfer/PT transfer/otherforms/",pattern = "*.csv",full.names = T,include.dirs = F)
error_outdir = file.path("~/Box/skinner/data/Redcap Transfer/PT transfer/otherforms/"
                         ,gsub(":","",gsub("-","",gsub(" ","_",Sys.time()))))

ptc_toget <- c("SUICIDE","SUICIDE2","PROTECT")
gMAPx<-bsrc.getEVTDATEFIELD(db = protect)
sp_rctogo<-lapply(sp_lookup,function(dfx,same_evt_days = 10){
  dfx<-bsrc.findid(dfx,learn_idmap)
  dfy<-dfx[dfx$variable %in% ptc_toget,]
  if(nrow(dfy)<1) {return(NULL)}
  #return(NULL)
  dfy$value <- toupper(dfy$value)
  dfy$value[dfy$value=="INT"] <- "ADDA"
  dfy$ID<-dfy$masterdemo_id
  dfy<-dfy[order(as.Date(dfy$CDATE)),]
  dfy <- change_evt(dty = dfy,protocol_name = "NUM",arm_num = 1,evtvariname = "value")
  dfy$value[dfy$value == "ADDA"] <- paste0("ADDA",1:length(which(dfy$value=="ADDA")))
  dfy$TIC <- NA
  #Consolidate same visit type; get date duration
  if(nrow(dfy)>1) {
    dfy$unique_evt <- paste(dfy$variable,dfy$value,sep = "_")
    dfy <- do.call(rbind,lapply(split(dfy,dfy$unique_evt),function(dfgx){
      if(nrow(dfgx)==1){return(dfgx)}
      dfgy <-dfgx[which.min(as.Date(dfgx$CDATE)),]
      dfgy$TIC <- list(dfgx$CDATE - dfgy$CDATE)
      return(dfgy)
    }))
    rownames(dfy) <- NULL
    dfy <- dfy[order(as.Date(dfy$CDATE)),]
  }
  #Consolidate possible identical visits
  if(nrow(dfy)>1){
    dfy$CDATE_diff <- (dfy$CDATE - dfy$CDATE[c(NA,1:(nrow(dfy)-1))])
    dfy$CDATE_logi<- dfy$CDATE_diff < same_evt_days
    dfy$CDATE_logi[which(grepl("ADDA",dfy$EVT))] <- dfy$CDATE_diff[which(grepl("ADDA",dfy$EVT))] <= 3
    dfy$CDATE_logi[is.na(dfy$CDATE_logi) ] <- FALSE
    dfy$num <- 1:nrow(dfy)
    dfy$num <- dfy$num - cumsum(as.numeric(dfy$CDATE_logi))
    dfy<-do.call(rbind,lapply(split(dfy,dfy$num),function(dfxa){
      if(nrow(dfxa)==1) {
        return(dfxa)
      }
      if(any(grepl("ADDA",dfxa$EVT))){
        ind_logi <- min(which(grepl("ADDA",dfxa$EVT)))
      } else {
        ind_logi <- which.max(as.numeric(dfxa$EVT))
      }
      dfxb <- dfxa[ind_logi,]
      dfxb$TIC <-  list(dfxa$CDATE - dfxb$CDATE)
      return(dfxb)
    }))
  }
  dfy$value[dfy$EVT == "ADDA"] <- paste0("ADDA",1:length(which(dfy$EVT=="ADDA")))


  if(toupper(dfy$value[which.min(dfy$CDATE)]) == "B" || grepl("ADDA",toupper(dfy$value[which.min(dfy$CDATE)]))) {
    baseline_evt <- as.character(dfy$variable[which.min(dfy$CDATE)])
    #message("Identified first contact as ",as.character(dfy$variable[which.min(dfy$CDATE)])," baseline")
    if(baseline_evt!=ptc_toget[min(match(unique(dfx$variable),ptc_toget),na.rm = T)]) {
      print(unique(dfx$ID))
      print(as.character(unique(dfx$variable)))

      message("#######!!!!!!! MISALIGNED STUDY ORDER!!!!! ",baseline_evt," ",ptc_toget[min(match(unique(dfx$variable),ptc_toget),na.rm = T)],
              ". Don't know what to do yet, pass.")
      return(NULL)
    }
  } else {
    print(unique(dfx$ID))
    print(as.character(unique(dfx$variable)))

    message("#######!!!!!!! First contact identified as ",as.character(dfy$variable[which.min(dfy$CDATE)])," ",as.character(dfy$value[which.min(dfy$CDATE)]),
            ". Don't know what to do yet, pass.")
    return(NULL)
  }
  dfy$EVT_num <- suppressWarnings(as.numeric(dfy$EVT))
  add_yrs<-aggregate(EVT_num~variable,data = dfy,FUN = max)
  add_yrs$EVT_num<-cumsum(add_yrs$EVT_num) - as.numeric(add_yrs$EVT_num)
  add_yrs$EVT_num[add_yrs$EVT_num < 0.5] <- 0

  dfy$EVT_num <- dfy$EVT_num + add_yrs$EVT_num[match(dfy$variable,add_yrs$variable)]
  dfy$EVT_num[dfy$EVT=="ADDA"] <- "ADDA"
  dfy<-change_evt(dty = dfy,protocol_name = "NUM2PROTECT",arm_num = 1,evtvariname = "EVT_num")

  return(dfy[c("ID","CDATE","variable","value","EVT","TIC")])
})
sp_rctogo<-sp_rctogo[!sapply(sp_rctogo,is.null)]

rctogo_fm <- do.call(rbind,sp_rctogo)
rctogo_fm <- bsrc.findid(df = rctogo_fm,idmap = learn_idmap,id.var = "ID")
rctogo_fm$date_vari <- gMAPx$date_variname[match(rctogo_fm$EVT,gMAPx$unique_event_name)]
for (xa in split(rctogo_fm,rctogo_fm$date_vari)){
  d_vari <- unique(xa$date_vari)
  xa<-xa[c("masterdemo_id","EVT","CDATE")]
  xa$CDATE <- as.character(xa$CDATE)
  names(xa) <- c("registration_redcapid","redcap_event_name",d_vari)
  XU<-upload_transfer(x_data = xa,id.var = "registration_redcapid",
                  error_outdir= error_outdir,
                  idmap = learn_idmap,
                  idmap_id = "masterdemo_id",
                  metadata = protect$metadata,
                  eventdata = protect$eventmap,
                  target_ptc = ptcs$protect,
                  ID_list = p2_only_ID,
                  skip_check = F,
                  toignore = TRUE,
                  exempt_code = c(999,99,"")
                  )
}


#sp_rctogo[sapply(sp_rctogo,function(x){any(duplicated(x$EVT))})]

gx<-lapply(f_paths,upload_transfer,
           id.var = "registration_redcapid",
           error_outdir= error_outdir,
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           eventdata = protect$eventmap,
           target_ptc = ptcs$protect,
           ID_list = p2_only_ID,
           skip_check = F,
           toignore = TRUE,
           exempt_code = c(999,99,""),
           x_data = NULL,
           look_up = sp_rctogo,
           max_allowed = 7)


##############################################
#########Do the p2 folks here:################
##############################################

#####P2 - part
ptc_toget <- c("PROTECT2","LEARN","EXPLORE","EYE_DECIDE","SNAKE")
gMAPx<-bsrc.getEVTDATEFIELD(db = protect)



p2_rcgo<-lapply(sp_lookup,function(dfx){
  dfx<-bsrc.findid(dfx,learn_idmap)
  dfy<-dfx[dfx$variable %in% ptc_toget,]
  if(nrow(dfy)<1) {return(NULL)}
  #return(NULL)
  dfy$value <- toupper(dfy$value)
  dfy$value[dfy$value=="INT"] <- "ADDA"
  dfy$ID<-dfy$masterdemo_id
  dfy<-dfy[order(as.Date(dfy$CDATE)),]
  sp_evt <- split(dfy,as.character(dfy$variable))
  dfy <- do.call(rbind,lapply(names(sp_evt),function(x){
    df_todo <- sp_evt[[x]]
    df_todo$value[grep("ADDA",df_todo$value)] <- paste0("ADDA",1:length(grep("ADDA",df_todo$value)))
    change_evt(dty = df_todo,protocol_name = x,arm_num = 2,evtvariname = "value")
  }))
  dfy$TIC <- 0
  if(any(duplicated(dfy$EVT))){
    dfy<-do.call(rbind,lapply(split(dfy,dfy$EVT),function(y){
      y$TIC[1]<-list(as.numeric(y$CDATE - y$CDATE[1]))
      return(y[1,])
    }))
    rownames(dfy)<-NULL
  }
  if(any(is.na(dfy$EVT))){
    stop("ID: ",unique(dfy$ID)," has NA evt.")
  }
  return(dfy[c("ID","CDATE","variable","value","EVT","TIC")])
})
p2_rcgo<-p2_rcgo[!sapply(p2_rcgo,is.null)]

f_paths<-list.files("~/Box/skinner/data/Redcap Transfer/PT transfer/otherforms/",
                    pattern = "*.csv",full.names = T,include.dirs = F)
error_outdir = file.path("~/Box/skinner/data/Redcap Transfer/PT transfer/otherforms/",
                         gsub(":","",gsub("-","",gsub(" ","_",Sys.time()))))
protect_backUP <- bsrc.checkdatabase2(protocol = ptcs$protect,forceupdate = T)
save(protect_backUP,file = file.path(error_outdir,"protect_backup.rdata"))
gx<-lapply(f_paths,upload_transfer,
           id.var = "registration_redcapid",
           error_outdir= error_outdir,
           idmap = learn_idmap,
           idmap_id = "masterdemo_id",
           metadata = protect$metadata,
           eventdata = protect$eventmap,
           target_ptc = ptcs$protect,
           ID_list = NULL,
           skip_check = F,
           toignore = TRUE,
           exempt_code = c(999,99,""),
           x_data = NULL,
           look_up = p2_rcgo,
           max_allowed = 7)

#################neuropsych:
##protect exit:

protect <- bsrc.checkdatabase2(protocol = ptcs$protect,online = T)

exit <- bsrc.getform(curdb = protect,formname = "exit")
IDEvTMap<-bsrc.getIDDateMap(db = protect)
IDEvTMap$unique<-paste(IDEvTMap$registration_redcapid,IDEvTMap$redcap_event_name,sep = "_x_")
exit$unique<-paste(exit$registration_redcapid,exit$redcap_event_name,sep = "_x_")

exit$date<- as.Date(IDEvTMap$date[match(exit$unique,IDEvTMap$unique)])




