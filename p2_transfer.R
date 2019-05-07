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
    # ahha<-names(lsx$transfer)[which(metals$varimap$field_type[match(names(lsx$transfer),metals$varimap$field_name)] %in% c("radio","yesno","dropdown","checkbox"))]
    # 
    # templsx<-lapply(ahha,function(xj){
    #   whichones<-which(!lsx$transfer[[xj]] %in% bsrc.getchoicemapping(variablenames = xj,metadata = metals$varimap)$choice.code & !is.na(lsx$transfer[[xj]]))
    #   if(length(whichones)>0){
    #     #We Now Withheld all of their data instance; instead of just changing one of them 
    #     ogdata<-lsx$transfer[[xj]][whichones]
    #     ogdatainstance<-lsx$transfer[whichones,]
    #     IDs<-lsx$transfer[[ID_fieldname]][whichones]
    #     dfaz<-data.frame(ID=IDs,VariableName=xj,TriggeredOriginalData=ogdata,order=whichones)
    #   return(list(info=dfaz,ogdata=ogdatainstance))
    #   } else {NULL}
    # })
    # lsx$valuemismatch <- list(info=do.call(rbind,lapply(templsx,function(dx){dx$info})),
    #                           ogdata=do.call(rbind,lapply(templsx,function(dx){dx$ogdata})))
    # if(length(lsx$valuemismatch$info$order)>0){
    # lsx$transfer<-lsx$transfer[-lsx$valuemismatch$info$order,]
    # }
    # lsx$valuemismatch$info$order<-NULL
    #adapt function:
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
##########
convert_exceldate<-function(x){
  return(as.Date(as.numeric(x),origin="1899-12-30"))
}


rootdir = "~/Box/skinner/data/Redcap Transfer/redcap outputs"
idmap<-REDCapR::redcap_read(batch_size = 1000,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token,fields = c("registration_redcapid","registration_wpicid"))$data
stop("STOP HERE FUNCTIONS ABOVE")
#Local boxdir:


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
#REDCapR::redcap_write(DEMO_trans$data,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
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


###Update the demo and consent date for folks;
masterdemo<-bsrc.conredcap2(protocol = ptcs$masterdemo,online = T,batch_size = 1000)
conp_DEMO<-unpack(readxl::read_xlsx(path = file.path(rootdir,"ALL_SUBJECTS_DEMO.xlsx")))
conp_DEMO$data<-bsrc.findid(df = conp_DEMO$data,idmap = idmap)
NOTINMASTERYET<-conp_DEMO$data[is.na(conp_DEMO$data$registration_redcapid),]

allvaris<-bsrc.getchoicemapping(variablenames = "registration_ptcstat",metadata = masterdemo$metadata)

noids<-which(is.na(conp_DEMO$data$registration_redcapid))
conp_DEMO$data$registration_wpicid[noids]<-conp_DEMO$data$ID[noids]
if(any(conp_DEMO$data$ID[noids] > 450000)){stop("SOME OF THESE PEOPLE WITH MORE THAN 45---- ID!!!")
}else{
  conp_DEMO$data$registration_redcapid[noids]<-conp_DEMO$data$ID[noids]
  }

all_demog<-lapply(allvaris$choice.code,function(vgname){
    print(vgname)
    arz<-as.data.frame(lapply(conp_DEMO$data[names(conp_DEMO$data)[which(grepl(paste0(vgname,"$"),names(conp_DEMO$data)))]],convert_exceldate),stringsAsFactors = F)
    karz<-apply(arz, 1, function(x){any(!is.na(x))})
    ready_arz<-cbind(conp_DEMO$data["registration_redcapid"],arz)[karz,]
    ready_arz[paste("registration_ptcstat",vgname,sep="___")]<-1
    return(ready_arz)
  })
names(all_demog)<-allvaris$choice.code
all_demog$bsocial<-NULL
all_demog$ksocial<-NULL

lapply(all_demog,REDCapR::redcap_write,redcap_uri = ptcs$masterdemo$redcap_uri,token=ptcs$masterdemo$token)

#That's fine creating new subjects
if(any(as.numeric(NOTINMASTERYET$ID)>800000)){stop("HEY! THERE ARE SOME 88 PEOPLE!!!!!")}

conp_DEMO$data$registration_redcapid[is.na(conp_DEMO$data$registration_redcapid)]<-conp_DEMO$data$ID[is.na(conp_DEMO$data$registration_redcapid)]

conp_DEMO$data$NEWGROUP<-plyr::mapvalues(x = conp_DEMO$data$PATYPE_TEXT,from = c("ATTEMPTER","OTHER","IDEATOR","BPD","OTHER PATIENT","CONTROL","DEPRESSION","NON-ATTEMPTER"),
                to = c("ATT","88","IDE","NON","88","HC","DEP","NON")
                )
conp_DEMO$data$registration_groupchange<-NA
conp_DEMO$data$registration_groupchange[which(nchar(conp_DEMO$data$NEWGROUP)>4)]<-1

conp_DEMO$data$registration_oggroup<-NA
conp_DEMO$data$registration_oggroup[which(conp_DEMO$data$registration_groupchange==1)]<-sapply(strsplit(conp_DEMO$data$NEWGROUP[which(conp_DEMO$data$registration_groupchange==1)],split = "-"),`[[`,1)
conp_DEMO$data$NEWGROUP[which(conp_DEMO$data$registration_groupchange==1)]<-sapply(strsplit(conp_DEMO$data$NEWGROUP[which(conp_DEMO$data$registration_groupchange==1)],split = "-"),`[[`,2)
conp_DEMO$data$NEWGROUP<-plyr::mapvalues(x = conp_DEMO$data$NEWGROUP,from = c("ATTEMPTER","OTHER","IDEATOR","BPD","OTHER PATIENT","CONTROL","DEPRESSION","NON-ATTEMPTER"),
                                         to = c("ATT","88","IDE","NON","88","HC","DEP","NON"),warn_missing = F
)
conp_DEMO$data$registration_oggroup<-plyr::mapvalues(x = conp_DEMO$data$registration_oggroup,from = c("ATTEMPTER","OTHER","IDEATOR","BPD","OTHER PATIENT","CONTROL","DEPRESSION","NON-ATTEMPTER"),
                                         to = c("ATT","88","IDE","NON","88","HC","DEP","NON"),warn_missing = F
)
conp_DEMO$data$registration_group<-conp_DEMO$data$NEWGROUP

conp_2upload<-conp_DEMO$data[c("registration_redcapid","registration_initials","registration_edu","registration_marrs","registration_groupchange",
                               "registration_oggroup","registration_group")]


REDCapR::redcap_write(conp_2upload,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)


########Get BSocial People while we are here;
bsocial<-bsrc.checkdatabase2(protocol = ptcs$bsocial)
bsometa_reg<-bsocial$metadata[bsocial$metadata$form_name=="record_registration",]
subreg<-bsrc.getevent(eventname = "enrollment_arm_1",subreg = T,curdb = bsocial)
subreg<-subreg[!is.na(as.numeric(subreg$registration_redcapid)),]
subreg[subreg==""]<-NA
subreg$registration_id[is.na(subreg$registration_id)]<-subreg$registration_redcapid[is.na(subreg$registration_id)]

masterdemo$metadata[masterdemo$metadata==""]<-NA

tag<-masterdemo$metadata$field_note[which(masterdemo$metadata$field_note %in% bsometa_reg$field_note)]

itg_df<-data.frame(tag=tag,
           from = bsometa_reg$field_name[match(tag,bsometa_reg$field_note)],
           to = masterdemo$metadata$field_name[match(tag,masterdemo$metadata$field_note)],stringsAsFactors = F)



lapply(itg_df$tag,function(ms_name){
  if(!ms_name %in% c("dnplid")){
    #Our goal here is to get the conflict df
    subreg[c("registration_redcapid",ms_name)]
           
  } 
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


###Special Cases;

#Med List:
medlist_x <-readxl::read_xlsx(file.path(rootdir,"To be transferred","SP","MEDS.xlsx"))

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

rx_filter <- function(rxcui, propName, propValues = "IN"){
  prams <- list(propName = propName, propValues = propValues)
  r <- GET("https://rxnav.nlm.nih.gov/REST/", path = paste0("REST/rxcui/", rxcui,"/filter"),
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

if(!file.exists(file.path(rootdir,"drug_indx.rdata"))){
  allmednames<-unique(medlist_x$data$DRUGNAME)
  alldrug_xcui<-lapply(allmednames,get_drug)
  drug_indx<-do.call(rbind,alldrug_xcui)
  save(drug_indx,file = file.path(rootdir,"drug_indx.rdata"))
} else {load(file.path(rootdir,"drug_indx.rdata"))}

medlist_y<-merge(medlist_x,drug_indx,by.x = "medlist_name",by.y = "drug_name",all.x = T)
medlist_y$medlist_spname<-NA
#medlist_y$medlist_spname[which(is.na(medlist_y$score) | as.numeric(medlist_y$score)<100)]<-medlist_y$medlist_name[which(is.na(medlist_y$score) | as.numeric(medlist_y$score)<100)]
medlist_y$medlist_spname<-medlist_y$medlist_name
#medlist_y$medlist_name<-medlist_y$drug_rxcui
#medlist_y$medlist_name[which(is.na(medlist_y$score) | as.numeric(medlist_y$score)<100)]<-"1241571" #This is the code for Fish as we use 'Fish' for unspecified.
medlist_y$medlist_name<-""
medlist_y$medlist_dose<-round(medlist_y$medlist_dose,1)

medlist_y$medlist_freq<-gsub(" ","",medlist_y$medlist_freq)
medlist_y$medlist_freq<-gsub("DAYTOTPRN","PRN",medlist_y$medlist_freq)
variconmap<-bsrc.getchoicemapping(variablenames = "medlist_freq_1",protocol = ptcs$protect)
medlist_y$medlist_freq[which(tolower(medlist_y$medlist_freq)=="notinlist–seecomment")]<-"seecomment"
medlist_y$medlist_freq<-tolower(medlist_y$medlist_freq)

medlist_y$medlist_startdate<-as.Date(medlist_y$medlist_startdate,"YYYY-MM-DD")
medlist_y$medlist_enddate<-as.Date(medlist_y$medlist_enddate,"YYYY-MM-DD")

medlist_y<-bsrc.findid(medlist_y,idmap = idmap)

medlist_r<-medlist_y[,c("registration_redcapid","medlist_name","medlist_dose","medlist_units","medlist_freq","medlist_startdate","medlist_enddate","medlist_comment","medlist_spname")]
medlist_r<-medlist_r[order(medlist_r$registration_redcapid),]
medlist_r$seqx<-unlist(lapply(split(medlist_r$registration_redcapid,medlist_r$registration_redcapid),seq_along))

medlist_z<-reshape(data = medlist_r,timevar = "seqx",idvar = "registration_redcapid",direction = "wide",sep = "_")
nouploadid<-medx$registration_redcapid[which(medx$registration_redcapid %in% medlist_z$registration_redcapid)]
medlist_z2<-medlist_z[which(!medlist_z$registration_redcapid %in% nouploadid),]
medlist_z2$redcap_event_name<-"baseline_arm_2"
REDCapR::redcap_write(medlist_z2,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)


####This is the suicide histroy
##Get the existing folks, exclude them for now;
existsingSAHX<-bsrc.getform(protocol = ptcs$protect,formname = "ongoing_suicide_hx_lethality",mod = T,grabnewinfo = T,aggressivecog = F)
masterdemo<-bsrc.conredcap2(ptcs$masterdemo,output = T,batch_size = 500)
p2demo<-masterdemo$data[which(masterdemo$data$registration_ptcstat___protect2==1),]
#Let's do baseline first;
sahx_og<-unpack(readxl::read_xlsx(file.path(rootdir,"To be transferred","SP","SUICIDE HISTORY_BL_cv.xlsx")))
sahx_dfx<-sahx_og$data[sahx_og$map$OG_name[!is.na(sahx_og$map$RC_name)]]
names(sahx_dfx)<-sahx_og$map$RC_name[match(names(sahx_dfx),sahx_og$map$OG_name)]
if(any(sahx_dfx$ID %in% as.character(existsingSAHX$registration_redcapid))){stop("HEY!!!!!OVERLAPS!!!!!!")}

#For now only transfer P2 folks:
evtname<-"baseline_arm_2"
sahx_dfy<-sahx_dfx[which(sahx_dfx$ID %in% p2demo$registration_redcapid),]
sahx_dfz<-melt(sahx_dfy,id.vars=c("ID","sahx_attemptnum"))
sahx_dfz$type<-sapply(strsplit(as.character(sahx_dfz$variable),split = "-x-"),`[[`,1)
sahx_dfz$num<-sapply(strsplit(as.character(sahx_dfz$variable),split = "-x-"),`[[`,2)
#Recast
sahx_dfz$variable<-NULL;
sahx_dfr<-reshape(data = sahx_dfz,timevar = "type",idvar = c("ID","num","sahx_attemptnum"),direction = "wide",sep = "-x-")
names(sahx_dfr)<-gsub("value-x-","",names(sahx_dfr))
sahx_dfr$sahx_sadate_at<-convert_exceldate(sahx_dfr$sahx_sadate_at)

sp_sahx<-split(sahx_dfr,sahx_dfr$ID)

sp_sahx_proc<-cleanuplist(lapply(sp_sahx,function(grk){
  #print(unique(grk$ID))
  incorrectentry<-FALSE; notmatch<-FALSE; nodata<-FALSE
  if(!is.na(unique(grk$sahx_attemptnum))){
    grz<-grk[!duplicated(grk[which(!names(grk) %in% c("sahx_lr_at"))]),]
    if(as.numeric(unique(grz$sahx_attemptnum))>0){
      grz<-grz[which(!is.na(grz$sahx_sadate_at) | !is.na(grz$sahx_describe_at)),]
    }
    
    if(nrow(grz) != unique(grz$sahx_attemptnum)) {
      
      dupli_date<-which(duplicated(grz$sahx_sadate_at) | duplicated(grz$sahx_sadate_at,fromLast = T))
      if(length(dupli_date)>1){
        grz<-rbind(grz[-dupli_date,], do.call(rbind,lapply(unique(grz$sahx_sadate_at[dupli_date]),function(gd){
          if(any(grz[which(grz$sahx_sadate_at==gd),"sahx_describe_at"] %in% grz[which(grz$sahx_sadate_at!=gd),"sahx_describe_at"])){
            message("This person has incorrect entries: ",unique(grz$ID))
            incorrectentry<-TRUE
            return(grz[which(grz$sahx_sadate_at==gd),])
          } else {
            data.frame(t(apply(grz[which(grz$sahx_sadate_at==gd),],2,function(xr){
              xr<-xr[!is.na(xr)]
              if(length(xr)==0){xr<-NA}
              if(length(unique(xr))>1){
                paste(xr,collapse = " / ")
              } else {
                return(unique(xr))
              }
            })))
          }
         
        })
        ),stringsAsFactors = F)
      }
    }
    
    if(nrow(grz) != as.numeric(unique(grz$sahx_attemptnum)) & as.numeric(unique(grz$sahx_attemptnum))!=0){
      message("This person: ",unique(grz$ID)," doesn't match")
      notmatch<-TRUE
      }
  } else {nodata<-TRUE}
  
  statusls<-data.frame(incorrectentry,notmatch,nodata)
  
  if(any(unlist(statusls))){returndf<-grk} else {returndf<-grz}
  
  return(list(status=statusls,data=returndf))
  
}))

sahx_dfx_proc<-do.call(rbind,lapply(sp_sahx_proc,function(x){
  if(any(unlist(x$status))){NULL}else{
    xr<-x$data
    xr$ML<-NA
    xr$MR<-NA
    xr$num<-NA
    if(unique(xr$sahx_attemptnum)!=0){
      xr$ML[which.max(xr$sahx_lr_at)]<-TRUE
      xr$MR[which.max(xr$sahx_sadate_at)]<-TRUE
      xr$num<-1:nrow(xr)
    }
    return(xr)
    }
}))
for (xrz in names(sahx_dfx_proc)) {
  names(sahx_dfx_proc[[xrz]])<-NULL
}
rownames(sahx_dfx_proc)<-NULL
sahx_dfx_proc$sahx_lr_at[which(as.numeric(sahx_dfx_proc$sahx_lr_at)>8)]<-NA

wSA_dfx<-sahx_dfx_proc[which(sahx_dfx_proc$sahx_attemptnum>0),]




#####This is the termination form transfer:
term_og<-unpack(readxl::read_xlsx(file.path(rootdir,"To be transferred","SP","LONGITUDINAL_STUDIES_TERM.xlsx")))
term_og$data$TERMDATE<-convert_exceldate(term_og$data$TERMDATE)

term_dfx<-term_og$data[term_og$map$OG_name[!is.na(term_og$map$RC_name)]]
names(term_dfx)<-term_og$map$RC_name[match(names(term_dfx),term_og$map$OG_name)]
term_dfx$uniqueID<-paste(term_dfx$ID,term_dfx$ptcs,sep = "__")
term_notes<-reshape2::melt(term_dfx[c("uniqueID",names(term_dfx)[grep("notes_",names(term_dfx))])],id.vars=c("uniqueID"))
term_notes_sp<-split(term_notes,term_notes$uniqueID)

mastermeta<-REDCapR::redcap_metadata_read(redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)$data

term_notes_proc<-do.call(rbind,lapply(term_notes_sp,function(kgz){
  kgy<-kgz[!is.na(kgz$value),]
  if(nrow(kgy)>0){
      kgy$value<-paste(kgy$variable,kgy$value,sep = ": ")
      kgy<-kgy[c("uniqueID","value")]
      names(kgy)<-c("uniqueID","reg_term_reason_n")
      return(kgy)
  } else {
    return(data.frame(uniqueID=unique(kgz$uniqueID),reg_term_reason_n="NO NOTES FOUND/IMPORTED",stringsAsFactors = F) )d
  }
})
)
term_dfy<-merge(term_dfx,term_notes_proc,by = "uniqueID",all.x = T)
term_dfy$ptcs<-tolower(term_dfy$ptcs)
term_dfy$reg_term_reason_n[is.na(term_dfy$reg_term_reason_n)]<-""
term_dfy$reg_term_reason_n[!term_dfy$reg_term_who %in% bsrc.getchoicemapping(variablenames = "reg_term_who_protect2",
                                                                             metadata = mastermeta)$choice.code]<- paste("Rater: ",
   term_dfy$reg_term_who[!term_dfy$reg_term_who %in% bsrc.getchoicemapping(variablenames = "reg_term_who_protect2",metadata = mastermeta)$choice.code],
   term_dfy$reg_term_reason_n[!term_dfy$reg_term_who %in% bsrc.getchoicemapping(variablenames = "reg_term_who_protect2",metadata = mastermeta)$choice.code],
   sep = " ")
term_dfy$reg_term_who[!term_dfy$reg_term_who %in% bsrc.getchoicemapping(variablenames = "reg_term_who_protect2",metadata = mastermeta)$choice.code]<-NA
term_dfy$reg_term_reason_n[which(!is.na(term_dfy$reg_term_cod))]<-paste("Additional Info:",term_dfy$reg_term_cod[which(!is.na(term_dfy$reg_term_cod))])
term_dfy$reg_term_cod<-""
term_dfy<-term_dfy[c("ID","ptcs",names(term_dfy)[grep("reg_term",names(term_dfy))])]
term_dfy$reg_term_yesno<-1
term_dfz<-reshape(data = term_dfy,timevar = "ptcs",idvar = "ID",direction = "wide",sep = "_")
term_dfz<-bsrc.findid(df = term_dfz,idmap = idmap,id.var = "ID")
term_dfz$ID<-NULL;term_dfz$registration_wpicid<-NULL;term_dfz$ogid<-NULL;
if(any(!term_dfz$ifexist)){message("Adding additional ID, consider before re-uploading...")}
term_dfz$ifexist<-NULL
REDCapR::redcap_write(term_dfz,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)

proto<-"PROTECT2"

###Upload
ksr<-masterdemo$data[c(1,grep("reg_termdate",names(masterdemo$data)))]
grx<-melt(ksr,id.vars="registration_redcapid")
tx<-grx[!is.na(grx$value) & grx$value!="",]
tx$variable<-gsub("reg_termdate_","",tx$variable)
tx$reg_term_yesno<-1
tx$value<-NULL

txy<-reshape(data = tx,timevar = "variable",idvar = "registration_redcapid",direction = "wide",sep = "_")
REDCapR::redcap_write(txy,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)

####THIS IS CAUSE OF DEATH########
cod_og<-unpack(readxl::read_xlsx(file.path(rootdir,"To be transferred","SP","CAUSE OF DEATH_cv.xlsx")))

cod_dfx<-cod_og$data
names(cod_dfx)<-cod_og$map$RC_name[match(names(cod_dfx),cod_og$map$OG_name)]
cod_dfx<-bsrc.findid(df = cod_dfx,idmap = idmap,id.var = "ID")
cod_dfx$cod_dead<-1
cod_dfx$registration_wpicid<-NULL; cod_dfx$ID<-NULL; cod_dfx$ogid<-NULL; cod_dfx$ifexist<-NULL
REDCapR::redcap_write(cod_dfx,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)





