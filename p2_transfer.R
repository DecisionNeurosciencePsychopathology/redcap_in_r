########
unpack<-function(dtx_r){
  gx<-list(
    map=data.frame(OG_name=names(dtx_r),RC_name=as.character(dtx_r[1,]),stringsAsFactors = F),
    data=dtx_r[-1,]
  )
  gx$map[gx$map=="NA" | gx$map=="NaN"]<-NA
  return(gx)
}
########
getevt<-function(ID=NULL,CDATE=NULL,PROTONAME=NULL,sp_lookup=NULL){
  #print(ID)
  idxz<-sp_lookup[[ID]]
  if(!is.null(idxz) && nrow(idxz)>0){
    rtx<-idxz[which.min(abs(as.Date(idxz$CDATE) - as.Date(CDATE))),c(PROTONAME,"CDATE")]
    names(rtx)<-c(PROTONAME,"OG_CDATE")
    rtx$DIFFDAY<-abs(as.Date(rtx$OG_CDATE) - as.Date(CDATE) )
  } else {
    rxy<-as.list(rep(NA,length(PROTONAME)))
    names(rxy)<-PROTONAME
    rtx<-cbind(rxy,data.frame(OG_CDATE=CDATE,DIFFDAY=NA))
  }
  return(rtx)
}
########
cleanup<-function(dtx_dt,EVTvari="EVT"){
  dtx_dt_i<-dtx_dt[dtx_dt$DIFFDAY < 30,]
  rx_sp<-split(dtx_dt_i,paste(dtx_dt_i$ID,dtx_dt_i[[EVTvari]],sep = "_"))
  rx_test<-do.call(rbind,lapply(rx_sp,function(dfz){
    if(nrow(dfz)>0){
      if(any(is.na(dfz[[EVTvari]]))){
        return(dfz)
      } else if(nrow(dfz)>1){
        return(dfz[which.min(dfz$DIFFDAY),])
      } else {return(dfz)}
    } else {return(NULL)}
  }))
  rownames(rx_test)<-NULL
  return(rx_test)
}
########
change_evt<-function(dty,protocol_name,arm_num){
  
  switch (protocol_name,
          "PROTECT2" = {
          dty$EVT[grepl("mo",tolower(dty$EVT))]<-paste("month",gsub("mo","",tolower(dty$EVT)[grepl("mo",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
          dty$EVT[grepl("yr",tolower(dty$EVT))]<-paste("year",gsub("yr","",tolower(dty$EVT)[grepl("yr",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
          dty$EVT[grepl("adda",tolower(dty$EVT))]<-paste("additional",gsub("adda","",tolower(dty$EVT)[grepl("adda",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
          dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("baseline_arm",arm_num,sep="_")
          dty$EVT<-gsub(".","",dty$EVT,fixed = T)
          },
          "SNAKE" = {
          dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("snake_arm",arm_num,sep="_")
          dty$EVT[grepl("^adda$",tolower(dty$EVT))]<-NA
          },
          "EYE_DECIDE" = {
            dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("idecide_arm",arm_num,sep="_")
            dty$EVT[grepl("^adda$",tolower(dty$EVT))]<-NA
          }
  )
  
  return(dty)
}
#########
proc_transfer<-function(dtx_rp,idmap,upload=T,metals,misscodeallowed=c(1),cleanup=T,protocol_name,ID_fieldname,arm_num) {
  # excluded = dtx_ii
  # dty = dtx_ii[which(dtx_ii$MISSCODE %in% misscodeallowed & !is.na(dtx_ii$EVT)),]
  map<-dtx_rp$map
  dty<-dtx_rp$data

  
  formname<-unique(metals$varimap$form_name[match(map$RC_name,metals$varimap$field_name,nomatch = 0)])
  supposedevtname<-metals$evtmap$unique_event_name[which(metals$evtmap$form %in% formname)]
  
  dty_i<-bsrc.findid(dty,idmap,id.var = "ID")
  dty_dt<-dty_i[map$OG_name[!is.na(map$RC_name)]]
  names(dty_dt)<-map$RC_name[match(names(dty_dt),map$OG_name)]
  
  dty_dt[[paste0(formname,"_miss")]]<-dty$MISSCODE
  
  allcombodf<-do.call(rbind,lapply(protocol_name,function(evt){
    dty_i$EVT<-dty_i[[evt]]
    dtyx<-change_evt(dty_i,evt,arm_num)
    
    dty_rc<-dtyx[c(ID_fieldname,"EVT")]
    names(dty_rc)<-c(ID_fieldname,"redcap_event_name")
    
    dty_combo<-cbind(dty_rc[which(!is.na(dty_rc$redcap_event_name)),],dty_dt[which(!is.na(dty_rc$redcap_event_name)),])
    dty_combo$ID<-NULL
    
    return(dty_combo)
  }))
  
  ndls<-which(apply(dty[protocol_name],1,function(z){length(which(is.na(z)))})==length(protocol_name))
  if(length(ndls)>0) {
  allnadf<-dty_dt[which(apply(dty[protocol_name],1,function(z){length(which(is.na(z)))})==length(protocol_name)),]
  allnadf[[ID_fieldname]]<-allnadf$ID
  allnadf$ID<-NULL
  allnadf$redcap_event_name<-NA
  } else {allnadf<-NULL}
  dty_combo<-rbind(allcombodf,allnadf)
  
  misscodenotallowed<-which(!dty_combo[[paste0(formname,"_miss")]] %in% misscodeallowed)
  noevent<-which(is.na(dty_combo$redcap_event_name))
  eventnotincluded<-which(!dty_combo$redcap_event_name %in% supposedevtname)
  
  whichtoexclude<-unique( c(misscodenotallowed,noevent,eventnotincluded) )
  lsx<-new.env()
  if(length(whichtoexclude)>0) {
    lsx<-list2env(list(transfer=dty_combo[-whichtoexclude,], excluded=dty_combo[whichtoexclude,],excludecodes=list(misscodeallowed=misscodenotallowed,
                                                                                                        noevent=noevent,
                                                                                                        eventnotincluded=eventnotincluded))
                ,envir = lsx)
  } else {
    lsx<-list2env(list(transfer=dty_combo, excluded=dty_combo[0,],excludecodes=list(misscodeallowed=misscodenotallowed,
                                                                               noevent=noevent,
                                                                               eventnotincluded=eventnotincluded))
             ,envir = lsx)
  }                                                                                                     
  
  if(cleanup){
    ahha<-names(lsx$transfer)[which(metals$varimap$field_type[match(names(lsx$transfer),metals$varimap$field_name)] %in% c("radio","yesno","dropdown","checkbox"))]

    templsx<-lapply(ahha,function(xj){
      whichones<-which(!lsx$transfer[[xj]] %in% bsrc.getchoicemapping(variablenames = xj,metadata = metals$varimap)$choice.code & !is.na(lsx$transfer[[xj]]))
      if(length(whichones)>0){
        #We Now Withheld all of their data instance; instead of just changing one of them 
        ogdata<-lsx$transfer[[xj]][whichones]
        ogdatainstance<-lsx$transfer[whichones,]
        IDs<-lsx$transfer[[ID_fieldname]][whichones]
        dfaz<-data.frame(ID=IDs,VariableName=xj,TriggeredOriginalData=ogdata,order=whichones)
      return(list(info=dfaz,ogdata=ogdatainstance))
      } else {NULL}
    })
    lsx$valuemismatch <- list(info=do.call(rbind,lapply(templsx,function(dx){dx$info})),
                              ogdata=do.call(rbind,lapply(templsx,function(dx){dx$ogdata})))
    if(length(lsx$valuemismatch$info$order)>0){
    lsx$transfer<-lsx$transfer[-lsx$valuemismatch$info$order,]
    }
    lsx$valuemismatch$info$order<-NULL
  }
  
  if(upload){
    if(nrow(lsx$transfer)>0){
    tryCatch({
      REDCapR::redcap_write(bsrc.choice2checkbox(dfx = lsx$transfer,metadata = metals$varimap,cleanupog = T),redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)
    },error=function(e){
      message("upload failed, reason: ",e)
      lsx$status<-"Upload Failed"
      })
      
    } else {
      message("No data to upload.")
      lsx$status<-"No Data to be Uploaded"
      }
  } 
  outz<-as.list(lsx)
  rm(lsx)
  return(outz)  
}
##########
transfer2redcap<-function(dtx_r=NULL,idmap=NULL,metals=NULL,misscodeallowed=NULL,arm_num=NULL,ID_fieldname=NULL,protocol_name=NULL,ifupload=T,clean=T) {
  dtx_rp<-unpack(dtx_r)
  dtx_rp$data<-cbind(dtx_rp$data,do.call(rbind,lapply(1:nrow(dtx_rp$data),function(x){getevt(dtx_rp$data$ID[x],dtx_rp$data$CDATE[x],protocol_name,sp_lookup)})))
  for(evt in protocol_name){
    dtx_rp$data<-cleanup(dtx_rp$data,EVTvari = evt)
  }
  output<-proc_transfer(dtx_rp = dtx_rp,idmap = idmap,upload = ifupload,metals = metals,misscodeallowed = misscodeallowed,arm_num=arm_num,
                        cleanup = clean,protocol_name=protocol_name,ID_fieldname=ID_fieldname)
  
  return(output)
}

stop("STOP HERE FUNCTIONS ABOVE")
#Local boxdir:
rootdir = "~/Box/skinner/data/Redcap Transfer/redcap outputs"

#DEMO;
#######
#In this trunk we deal with demo transfer:
DEMO_trans<-unpack(readxl::read_xlsx(file.path(rootdir,"Demo (Master Demographic Form).xlsx")))
names(DEMO_trans$data)<-gsub("registration_race_","registration_race___",names(DEMO_trans$data))
DEMO_trans$data$registration_wpicid<-DEMO_trans$data$registration_dnplid
DEMO_trans$data[as.numeric(DEMO_trans$data$registration_dnplid)>440000,1]<-gsub("88","43",unlist(DEMO_trans$data[as.numeric(DEMO_trans$data$registration_dnplid)>440000,1]))
DEMO_trans$data$ssn_Full<-NULL
DEMO_trans$data$registration_lastfour<-DEMO_trans$data$ssn_last4
DEMO_trans$data$ssn_last4<-NULL
# DEMO_trans$data$registration_redcapid<-DEMO_trans$data$registration_dnplid
# DEMO_trans$data$registration_dnplid<-NULL
REDCapR::redcap_write(DEMO_trans$data,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
#REDCapR::redcap_write(masterdemo$data,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
#######


#FOR HAM: 
dtx_rp$data$Q18B[dtx_rp$data$Q18B %in% c("99","0")]<-NA


jiazhou.startup()
#Transfer starts here:
metals<-list(
  evtmap=redcap.eventmapping(redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)$data,
  varimap=REDCapR::redcap_metadata_read(redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)$data
)
idmap<-REDCapR::redcap_read(batch_size = 1000,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token,fields = c("registration_redcapid","registration_wpicid"))$data
#Do look-up table:
lookuptable<-readxl::read_xlsx(file.path(rootdir,"Subject Visits Table","S_CONTACTS.xlsx"))
lookuptable$CDATE<-as.Date(lookuptable$CDATE)
sp_lookup<-lapply(split(lookuptable,lookuptable$ID),function(krz){
  su<-which(tolower(krz$PROTECT2)=="adda")
  if(length(su)>0) {
    krz$PROTECT2[su] <- paste0("ADDA",seq(su))
  }
  return(krz)
})

#Do the self-reports cuz they are ezzzzzz
txpath = file.path(rootdir,"To be transferred","TransferQueue")
protocol_name = c("PROTECT2","SNAKE","EYE_DECIDE")
forcerun = F

if(file.exists(file.path(rootdir,"outputs","all_uploadresults.rdata"))){
  oldoutput<-bsrc.attachngrab(rdpath = file.path(rootdir,"outputs","all_uploadresults.rdata"))$alloutputx
  completedones<-names(oldoutput)[sapply(oldoutput,`[[`,"status")=="SUCCESS"]
  message("these are NOT DONE: ",paste(names(oldoutput)[!sapply(oldoutput,`[[`,"status")=="SUCCESS"],collapse = ", "))
} else {
  completedones<-c("")
}

alloutputx<-lapply(list.files(txpath,full.names = F),function(rz){
  message(rz)
  if(!rz %in% completedones | forcerun){
    message("processing.....")
    dtx_t <- readxl::read_xlsx(file.path(txpath,rz))
    outputx<-NULL
    tryCatch({
      outputx<-transfer2redcap(dtx_r = dtx_t,idmap = idmap,metals = metals,misscodeallowed = c(as.character(1:26)),arm_num = 2,
                               ID_fieldname = "registration_redcapid",protocol_name = protocol_name,ifupload = T)
    },error=function(e){message("general processing error: ",e)})
    if(is.null(outputx)){outputx<-list(status="FAILED")}
    outputx$sourcefile<-rz
    if(is.null(outputx$status)){outputx$status<-"SUCCESS"}
  } else {
    outputx<-oldoutput[[which(sapply(oldoutput,`[[`,"sourcefile")==rz)]]
    message("already processed, moving on")
  }
  message("Status: ",outputx$status)
  message("#####################")
  return(outputx)
})
names(alloutputx)<-sapply(alloutputx,`[[`,"sourcefile")
save(alloutputx,file = file.path(rootdir,"outputs","all_uploadresults.rdata"))






###################
##We want to double check what's wrong with each thing:
##Start with error codes

unique(do.call(rbind,lapply(alloutputx,function(rx){rx$valuemismatch$info}))[c("VariableName","TriggeredOriginalData")])















