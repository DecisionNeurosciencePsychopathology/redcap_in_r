####HELPER#######

bsrc.getEVTDATEFIELD<-function(protocol,db=NULL) {
  if(is.null(db)){db<-bsrc.checkdatabase2(protocol = ptcs$protect,output = T)}
  datefield_vers<-db$metadata[which(db$metadata$field_note=="EVT_DATEFIELD"),]
  gMAPx<-db$eventmap[which(db$eventmap$form %in% datefield_vers$form_name),]
  gMAPx$date_variname<-datefield_vers$field_name[match(gMAPx$form, datefield_vers$form_name)]
  return(gMAPx)
}




bsrc.idevtdatemap<-function(protocol=NULL,rcIDvar="registration_redcapid",filterevt=NULL,cleanup=T,...){
  db<-bsrc.checkdatabase2(protocol = protocol,output = T,...)
  gMAPx<-bsrc.getEVTDATEFIELD(db = db)
  rMAPa<-db$data[which(db$data$redcap_event_name %in% gMAPx$unique_event_name),c(rcIDvar,"redcap_event_name",unique(gMAPx$date_variname)),]
  rMAPa[rMAPa==""]<-NA
  rMAPb<-reshape2::melt(rMAPa,id.vars=c(rcIDvar,"redcap_event_name"))
  names(rMAPb)<-c("registration_redcapid","redcap_event_name","variable","date")
  if(cleanup){
    rMAPb<-na.omit(rMAPb)
  }
  return(rMAPb)
}
######This is a very helpful function to check before uploading######################
######By check,  I mean verify if certrain informaiton is already in redcap##########
bsrc.uploadcheck<-function(dfa=NULL,uniqueidvars=c("registration_redcapid","redcap_event_name"),db_data=NULL) {
  og_df<-db_data[names(dfa)]
  og_df[og_df==""]<-NA
  og_df<-og_df[which(og_df$redcap_event_name %in% unique(dfa$redcap_event_name)),]
  
  dfa<-as.data.frame(apply(dfa,2,as.character))
  og_df<-as.data.frame(apply(og_df,2,as.character))
  
  dfa$row_num<-1:nrow(dfa)
  df_long<-reshape2::melt(dfa,id.vars=c(uniqueidvars,"row_num"), factorsAsStrings=F)
  df_long<-na.omit(df_long)
  df_long$identity<-do.call(paste,df_long[c(uniqueidvars,"variable")])
  
  og_long<-reshape2::melt(og_df,id.vars=uniqueidvars, factorsAsStrings=F)
  og_long<-na.omit(og_long)
  og_long$identity<-do.call(paste,og_long[c(uniqueidvars,"variable")])
  
  df_long$og_value<-og_long$value[match(df_long$identity,og_long$identity)]
  df_long$ifDiff<-df_long$value != df_long$og_value
  
  
  
  outputx<-df_long[which(is.na(df_long$og_value)),]
  newDF<-reshape2::dcast(data = outputx[c(uniqueidvars,"variable","value")],value.var = "value",formula = as.formula(paste(paste(uniqueidvars,collapse = "+"),"~ variable",sep = " ")))
  
  return(list(DFdifferent=df_long[which(df_long$ifDiff),],DFnew=df_long[which(is.na(df_long$og_value)),],
              uploaddf=newDF))
}
##########################

change_evt<-function(dty,protocol_name,arm_num,evtvariname=NULL){
  if(!is.null(evtvariname)){dty$EVT<-dty[[evtvariname]]}
  
  switch (protocol_name,
          "PROTECT2" = {
            dty$EVT[grepl("mo",tolower(dty$EVT))]<-paste("month",gsub("mo","",tolower(dty$EVT)[grepl("mo",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
            dty$EVT[grepl("yr",tolower(dty$EVT))]<-paste("year",gsub("yr","",tolower(dty$EVT)[grepl("yr",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
            dty$EVT[grepl("adda",tolower(dty$EVT))]<-paste("additional",gsub("adda","",tolower(dty$EVT)[grepl("adda",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
            dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("baseline_arm",arm_num,sep="_")
            dty$EVT<-gsub(".","",dty$EVT,fixed = T)
            if(any(grepl("int",tolower(dty$EVT)))){
              if(!any(grepl("additional_",dty$EVT))){existingADEVT<-0}else{
                existingADEVT<-max(as.numeric(gsub("additional_([0-9+]*)_.*","\\1",dty$EVT[which(grepl("additional_",dty$EVT))])))
              }
              dty$EVT[grepl("int",tolower(dty$EVT))]<-paste("additional",existingADEVT+1:length(which(grepl("int",tolower(dty$EVT)))),"arm",arm_num,sep = "_")   
            }
          },
          "NUM" = {
            
            dty$EVT[grepl("mo",tolower(dty$EVT))]<-as.numeric(gsub("mo","",tolower(dty$EVT)[grepl("mo",tolower(dty$EVT))])) / 12
            dty$EVT[grepl("yr",tolower(dty$EVT))]<-as.numeric(gsub("yr","",tolower(dty$EVT)[grepl("yr",tolower(dty$EVT))]))
            dty$EVT[grepl("^b$",tolower(dty$EVT))]<-0
            dty$EVT[grepl("adda",tolower(dty$EVT))]<-"ADDA"
            #dty$EVT<-gsub(".","_",dty$EVT,fixed = T)
          },
          "PROTECT" = {
            dty$EVT[grepl("mo",tolower(dty$EVT))]<-paste(gsub("mo","",tolower(dty$EVT)[grepl("mo",tolower(dty$EVT))]),"month","arm",arm_num,sep = "_")
            dty$EVT[grepl("yr",tolower(dty$EVT))]<-paste(gsub("yr","",tolower(dty$EVT)[grepl("yr",tolower(dty$EVT))]),"year","arm",arm_num,sep = "_")
            dty$EVT[grepl("adda",tolower(dty$EVT))]<-paste("additional_",gsub("adda","",tolower(dty$EVT)[grepl("adda",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
            dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("baseline_arm",arm_num,sep="_")
            dty$EVT<-gsub(".","_",dty$EVT,fixed = T)
            if(any(grepl("int",tolower(dty$EVT)))){
              if(!any(grepl("additional_",dty$EVT))){existingADEVT<-0}else{
                existingADEVT<-max(as.numeric(gsub("additional_([0-9+]*)_.*","\\1",dty$EVT[which(grepl("additional_",dty$EVT))])))
              }
              dty$EVT[grepl("int",tolower(dty$EVT))]<-paste("additional",existingADEVT+1:length(which(grepl("int",tolower(dty$EVT)))),"arm",arm_num,sep = "_")   
            }
          },
          "NUM2PROTECT" = {
            dty$EVT[dty$EVT == 0L] <- paste("baseline_arm",arm_num,sep="_")
            dty$EVT[which(dty$EVT < 1)] <- paste(as.numeric(dty$EVT[which(dty$EVT < 1)])*12,"month","arm",arm_num,sep = "_")
            even_yrs <- suppressWarnings(which(round(as.numeric(dty$EVT) / 0.5,0) == as.numeric(dty$EVT) / 0.5))
            dty$EVT[even_yrs] <- paste(dty$EVT[even_yrs],"year","arm",arm_num,sep = "_")
            dty$EVT[which(!grepl("arm",dty$EVT))] <- "ADDA"
            
            dty$EVT[grep("adda",tolower(dty$EVT))] <- paste0("ADDA",1:length(grep("adda",tolower(dty$EVT))))
            
            
            dty$EVT[grepl("adda",tolower(dty$EVT))]<-paste("additional",gsub("adda","",tolower(dty$EVT)[grepl("adda",tolower(dty$EVT))]),"arm",arm_num,sep = "_")
            dty$EVT<-gsub(".","_",dty$EVT,fixed = T)
            
          },
          "SNAKE" = {
            dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("snake_arm",arm_num,sep="_")
            dty$EVT[grepl("^adda$",tolower(dty$EVT))]<-NA
          },
          "EYE_DECIDE" = {
            dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("idecide_arm",arm_num,sep="_")
            dty$EVT[grepl("^adda$",tolower(dty$EVT))]<-NA
          },
          "EXPLORE" = {
            dty$EVT[grepl("^b$",tolower(dty$EVT))]<-paste("explore_arm",arm_num,sep="_")
            dty$EVT[grepl("^adda$",tolower(dty$EVT))]<-NA
          }
  )
  
  return(dty)
}

########functions#######
unpack<-function(dtx_r){
  gx<-list(
    map=data.frame(OG_name=names(dtx_r),RC_name=as.character(dtx_r[1,]),stringsAsFactors = F),
    data=dtx_r[-1,]
  )
  gx$map[gx$map=="NA" | gx$map=="NaN"]<-NA
  return(gx)
}

getevt<-function(ID=NULL,CDATE=NULL,PROTONAME=NULL,sp_lookup=NULL){
  #print(ID)
  ID<-as.character(ID)
  if(!ID %in% names(sp_lookup)) {rxy<-as.list(rep(NA,length(PROTONAME)))
  names(rxy)<-PROTONAME
  rtx<-cbind(rxy,data.frame(OG_CDATE=CDATE,DIFFDAY=NA))
  return(rtx)}
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


cleanup<-function(dtx_dt,EVTvari="EVT",maxDay=30){
  dtx_dt_i<-dtx_dt[dtx_dt$DIFFDAY < maxDay,]
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


####for raw data"
match_evt_clean_up<-function(df_a=NULL,TimeDiffMax=30,protocol_name=NULL,sp_lookup=NULL,cleanout=T){
  df_b<-cbind(df_a,do.call(rbind,lapply(1:nrow(df_a),function(x){getevt(df_a$ID[x],df_a$CDATE[x],protocol_name,sp_lookup)})))
  
  df_c<-df_b[df_b$DIFFDAY <= TimeDiffMax,]
  if(nrow(df_b) != nrow(df_c)){timeRemoveDf= df_b[df_b$DIFFDAY > TimeDiffMax,]} else {timeRemoveDf=NULL}
  message("When multiple protocols are matched, program will take the first match, ordered as the protocol_name object.")
  df_c$match_protocol<-unlist(apply(df_c[protocol_name],1,function(x){xa<-which(!is.na(x));if(length(xa)<1){return(NA)} else if(length(xa)>1){xa<-xa[1]};protocol_name[xa]}))
  df_c$multiple_match<-unlist(apply(df_c[protocol_name],1,function(x){xa<-which(!is.na(x));if(length(xa)<1){return(NA)} else if(length(xa)>1){T} else {F}}))
  df_c$all_matches<-unlist(apply(df_c[protocol_name],1,function(x){xa<-which(!is.na(x));if(length(xa)<1){return(NA)};paste(protocol_name[xa],collapse = "&")}))
  if(any(is.na(df_c$match_protocol))) {noProtoclMatchDf=df_c[which(is.na(df_c$match_protocol)),]} else {noProtoclMatchDf=NULL}
  df_d<-df_c[which(!is.na(df_c$match_protocol)),]
  df_d$EVT<-unlist(apply(df_d,1,function(x){x[match(x[match("match_protocol",names(df_d))],names(df_d))]}),use.names = F)
  #match("match_protocol",names(df_d))
  
  if(cleanout){
    df_e<-df_d[-match(protocol_name,names(df_d))]
  } else {df_e<-df_d}
  return(df_e)
}



############Clean-up upload;


trans_cleanup<-function(dfx=NULL,metadata=NULL,ID_fieldname=NULL){
  ahha<-names(dfx)[which(metadata$field_type[match(names(dfx),metadata$field_name)] %in% c("radio","yesno","dropdown","checkbox"))]
  templsx<-lapply(ahha,function(xj){
    whichones<-which(!dfx[[xj]] %in% bsrc.getchoicemapping(variablenames = xj,metadata = metadata)$choice.code & !is.na(dfx[[xj]]))
    if(length(whichones)>0){
      #We Now Withheld all of their data instance; instead of just changing one of them 
      ogdata<-dfx[[xj]][whichones]
      ogdatainstance<-dfx[whichones,]
      IDs<-dfx[[ID_fieldname]][whichones]
      dfaz<-data.frame(ID=IDs,VariableName=xj,TriggeredOriginalData=ogdata,order=whichones)
      return(list(info=dfaz,ogdata=ogdatainstance))
    } else {NULL}
  })
  valuemismatch <- list(info=do.call(rbind,lapply(templsx,function(dx){dx$info})),
                        ogdata=do.call(rbind,lapply(templsx,function(dx){dx$ogdata})))
  if(length(valuemismatch$info$order)>0){
    dfy<-dfx[-valuemismatch$info$order,]
  }else{dfy<-dfx}
  valuemismatch$info$order<-NULL
  return(list(outputdf=dfy,valuemismatch=valuemismatch))
}

get_ThisNotInThat<-function(dfa,dfb,uniquevars=c("ID","CDATE")){
  dfax<-dfa[uniquevars]
  dfa$uID<-paste(dfax$ID,dfax$CDATE)
  message("dfa has duplicated records?: ",any(duplicated(dfax$uID)))
  
  dfbx<-dfb[uniquevars]
  dfb$uID<-paste(dfbx$ID,dfbx$CDATE)
  message("dfb has duplicated records?: ",any(duplicated(dfbx$uID)))
  
  return(list(a_NotIn_b=dfa[which(!dfa$uID %in% dfb$uID),],
              b_NotIn_a=dfb[which(!dfb$uID %in% dfa$uID),]))
  
}

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
    
    cleanoutput<-trans_cleanup(dfx = lsx$transfer,metadata = metals$varimap,ID_fieldname = ID_fieldname)
    lsx$transfer<-cleanoutput$outputdf
    lsx$valuemismatch<-cleanoutput$valuemismatch
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


proc_transfer2<-function(dtx_rp,idmap,upload=T,metals,misscodeallowed=c(1),cleanup=T,protocol_name,ID_fieldname,arm_num,Replacementlist=NULL) {
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
  
  if(!is.null(Replacementlist)){
    for (todo_rp in names(Replacementlist)) {
      dty_dt[[todo_rp]]<-unlist(plyr::revalue(dty_dt[[todo_rp]],replace = Replacementlist[[todo_rp]],warn_missing = F))
    }
  }
  
  allcombodf<-do.call(rbind,lapply(protocol_name,function(evt){
    #print(evt)
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
  duplicatedentry<-which(duplicated(interaction(dty_combo[[ID_fieldname]],dty_combo$redcap_event_name)) | duplicated(interaction(dty_combo[[ID_fieldname]],dty_combo$redcap_event_name),fromLast = T))
  
  whichtoexclude<-unique( c(misscodenotallowed,noevent,eventnotincluded,duplicatedentry) )
  lsx<-new.env()
  if(length(whichtoexclude)>0) {
    lsx<-list2env(list(transfer=dty_combo[-whichtoexclude,], excluded=dty_combo[whichtoexclude,],excludecodes=list(misscodeallowed=misscodenotallowed,
                                                                                                                   noevent=noevent,duplicatedentry=duplicatedentry,
                                                                                                                   eventnotincluded=eventnotincluded))
                  ,envir = lsx)
  } else {
    lsx<-list2env(list(transfer=dty_combo, excluded=dty_combo[0,],excludecodes=list(misscodeallowed=misscodenotallowed,
                                                                                    noevent=noevent,duplicatedentry=duplicatedentry,
                                                                                    eventnotincluded=eventnotincluded))
                  ,envir = lsx)
  }                                                                                                     
  
  if(cleanup){
    
    cleanoutput<-trans_cleanup(dfx = lsx$transfer,metadata = metals$varimap,ID_fieldname = ID_fieldname)
    lsx$transfer<-cleanoutput$outputdf
    lsx$valuemismatch<-cleanoutput$valuemismatch
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

convert_exceldate<-function(x){
  return(as.Date(as.numeric(x),origin="1899-12-30"))
}