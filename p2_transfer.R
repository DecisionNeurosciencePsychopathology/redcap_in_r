source("./R/HELPER.R")

jiazhou.startup()
rootdir = "~/Box/skinner/data/Redcap Transfer/redcap outputs"
#Load ID Map:
idmap<-REDCapR::redcap_read(batch_size = 1000,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token,fields = c("registration_redcapid","registration_wpicid","registration_soloffid"))$data
names(idmap)<-c("masterdemoid","wpicid","soloffid")
#Load Look-up Table:
lookuptable<-readxl::read_xlsx(file.path(rootdir,"Subject Visits Table","S_CONTACTS.xlsx"))
lookuptable$CDATE<-as.Date(lookuptable$CDATE)
sp_lookup<-lapply(split(lookuptable,lookuptable$ID),function(krz){
  su<-which(tolower(krz$PROTECT2)=="adda")
  if(length(su)>0) {
    krz$PROTECT2[su] <- paste0("ADDA",seq(su))
  }
  return(krz)
})

metals<-list(
  evtmap=redcap.eventmapping(redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)$data,
  varimap=REDCapR::redcap_metadata_read(redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)$data
)

p2backuppath<-file.path(dirname(ptcs$protect$rdpath),"Backup")

stop("STOP HERE FUNCTIONS ABOVE")



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



#Transfer starts here:

#idmap<-REDCapR::redcap_read(batch_size = 1000,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token,fields = c("registration_redcapid","registration_wpicid"))$data
#Do look-up table:

###Update the demo and consent date for folks;#######
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



###Let's update the follow up dates and stuff;######
ptctoget<-c("PROTECT2")
sp_lookup$`23260`->dfa

gMAPx<-bsrc.getEVTDATEFIELD(protocol = ptcs$protect)
sp_rctogo<-lapply(sp_lookup,function(dfa){
  print(unique(dfa$ID))
  dfa<-bsrc.findid(dfa,idmap)
  dfa$ID<-dfa$masterdemoid
  dfa<-dfa[order(as.Date(dfa$CDATE)),]
  dfa_a<-dfa[c("ID","CDATE",ptctoget)]
  dfa_b<-change_evt(dty = dfa_a,protocol_name = ptctoget,arm_num = 2,evtvariname = ptctoget)
  dfa_c<-na.omit(dfa_b)
  dfa_c$date_variable<-gMAPx$date_variname[match(dfa_c$EVT,gMAPx$unique_event_name)]
  dfa_d<-dfa_c[c("ID","EVT","date_variable","CDATE")]
  if(nrow(dfa_d)<1){return(NULL)}
  if(any(duplicated(dfa_d$EVT))) {return(list(df=dfa_d,reason="duplicated evt"))}
  names(dfa_d)<-c("registration_redcapid","redcap_event_name","date_variable","CDATE")
  dfa_d$CDATE<-as.character(dfa_d$CDATE)
  dfa_e<-reshape2::dcast(dfa_d, registration_redcapid + redcap_event_name ~ date_variable, value.var="CDATE")
  return(as.data.frame(dfa_e))
})

sp_rctogo_erros<-sp_rctogo[which(sapply(sp_rctogo,inherits, "list"))]
sp_rctogo_ready<-sp_rctogo[which(sapply(sp_rctogo,inherits, "data.frame"))]
allnames<-unique(unlist(lapply(sp_rctogo_ready,names),use.names = F))
df_rctogo_ready<-do.call(rbind,lapply(sp_rctogo_ready,function(dfz){dfz[allnames[which(!allnames %in% names(dfz))]]<-NA;return(dfz)}))
protect<-bsrc.checkdatabase2(protocol = ptcs$protect,output = T)
save(protect,file = file.path(gsub("-","_",paste0("p2_backup",Sys.Date(),".rdata"))))









REDCapR::redcap_write(df_rctogo_ready,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)







#Do the self-reports cuz they are ezzzzzz########
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
                               ID_fieldname = "registration_redcapid",protocol_name = protocol_name,ifupload = F)
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

######Med List:######
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


####This is the suicide histroy########
##Get the existing folks, exclude them for now;
existsingSAHX<-bsrc.getform(protocol = ptcs$protect,formname = "ongoing_suicide_hx_lethality",mod = T,grabnewinfo = T,aggressivecog = F)
masterdemo<-bsrc.conredcap2(ptcs$masterdemo,output = T,batch_size = 500)
p2demo<-masterdemo$data[which(masterdemo$data$registration_ptcstat___protect2==1),]
#Let's do baseline first;
sahx_og<-unpack(readxl::read_xlsx(file.path(rootdir,"To be transferred","SP","SUICIDE HISTORY_cv.xlsx")))
sahx_dfx<-sahx_og$data[sahx_og$map$OG_name[!is.na(sahx_og$map$RC_name)]]
names(sahx_dfx)<-sahx_og$map$RC_name[match(names(sahx_dfx),sahx_og$map$OG_name)]
sahx_dfx<-bsrc.findid(sahx_dfx,idmap = idmap)
sahx_dfx$ID<-sahx_dfx$registration_redcapid
if(any(sahx_dfx$ID %in% as.character(existsingSAHX$registration_redcapid))){stop("HEY!!!!!OVERLAPS!!!!!!")}



sahx_dfx$registration_redcapid<-NULL;sahx_dfx$registration_wpicid<-NULL;sahx_dfx$ogid<-NULL;sahx_dfx$ifexist<-NULL
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
    } else {
      grz<-grz[1,]
    }
    
    if(nrow(grz) != unique(grz$sahx_attemptnum)) {
      
      dupli_date<-which(duplicated(grz$sahx_sadate_at) | duplicated(grz$sahx_sadate_at,fromLast = T))
      if(length(dupli_date)>1){
        grz<-rbind(grz[-dupli_date,], do.call(rbind,lapply(unique(grz$sahx_sadate_at[dupli_date]),function(gd){
          if(!is.na(gd)){
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
          } else {
            return(grz[is.na(grz$sahx_sadate_at) & !duplicated(grz$sahx_describe_at),])
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

#taking out ppl who are wrong;
sp_sahx_proc<-sp_sahx_proc[which(!sapply(sp_sahx_proc,function(arz){any(unlist(arz$status,use.names = F))}))]

sahx_dfx_proc<-do.call(rbind,lapply(sp_sahx_proc,function(x){
  if(any(unlist(x$status))){NULL}else{
    xr<-x$data
    #xr$ML<-NA
    #xr$MR<-NA
    xr$num<-NA
    if(unique(xr$sahx_attemptnum)!=0){
      #xr$ML[which.max(xr$sahx_lr_at)]<-TRUE
      #xr$MR[which.max(xr$sahx_sadate_at)]<-TRUE
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

wide_SAdfx<-reshape(wSA_dfx,idvar = c("ID","sahx_attemptnum"),timevar = "num",direction = "wide",sep = "")
wide_SAdfx$redcap_event_name<-evtname
wide_SAdfx$registration_redcapid<-wide_SAdfx$ID; wide_SAdfx$ID<-NULL

noSA_dfx<-sahx_dfx_proc[which(sahx_dfx_proc$sahx_attemptnum==0),]
noSA_dfx$redcap_event_name<-evtname
noSA_dfx$registration_redcapid<-noSA_dfx$ID; noSA_dfx$ID<-NULL
noSA_dfx<-noSA_dfx[c("registration_redcapid","redcap_event_name","sahx_attemptnum")]
noSA_dfx<-noSA_dfx[!noSA_dfx$registration_redcapid %in% c("114170"),]

REDCapR::redcap_write(wide_SAdfx,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)
REDCapR::redcap_write(noSA_dfx,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)

wSA_dfx$suiq_mlanum_bl<-NA
wSA_dfx$suiq_mranum_bl<-NA
wSA_dfx$suiq_mlanum_bl[which(wSA_dfx$ML)]<-wSA_dfx$num[which(wSA_dfx$ML)]
wSA_dfx$suiq_mranum_bl[which(wSA_dfx$MR)]<-wSA_dfx$num[which(wSA_dfx$MR)]
#wSA_dfx$ML<-NULL;wSA_dfx$MR<-NULL



#####This is the termination form transfer:##########
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
    return(data.frame(uniqueID=unique(kgz$uniqueID),reg_term_reason_n="NO NOTES FOUND/IMPORTED",stringsAsFactors = F) )
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

####Get reports:
protect<-bsrc.checkdatabase2(protocol = ptcs$protect)
list_of_forms<-unique(protect$metadata$form_name)

do.call(rbind,lapply(list_of_forms,function(xname){
  rm("dfax")
  tryCatch({
    dfax<-bsrc.getform(formname = xname,curdb = protect,mod = T,aggressivecog = T,nocalc = T)
  },error=function(e){message(e)}) 
  
  if(exists("dfax")){
    
    if(nrow(dfax)>0){
      infodf<-data.frame(formname=xname,num_observation=nrow(dfax),uniuqe_id_num=length(unique(dfax$registration_redcapid)),
                 avg_per_sub=mean(sapply(split(dfax,dfax$registration_redcapid),nrow,USE.NAMES = F,simplify = T)),
                 source_evt=paste(unique(dfax$redcap_event_name),collapse = "; "),stringsAsFactors = F)
    } else {
      infodf<-data.frame(num_observation=0,uniuqe_id_num=0,
                 avg_per_sub=NA,
                 source_evt=NA,formname=xname,stringsAsFactors = F)
    } 
  } else {
    infodf<-data.frame(num_observation=0,uniuqe_id_num=0,
               avg_per_sub=NA,
               source_evt=NA,formname=xname,stringsAsFactors = F)
  }
  return(infodf)
}))

#######SSI / SIS
list.files(dirname(txpath))



######re-do the function;#######


protocol_name <- c("PROTECT2","PROTECT","SUICIDE2","SUICIDE")


dtx_rp$data<-cbind(dtx_rp$data,do.call(rbind,lapply(1:nrow(dtx_rp$data),function(x){getevt(dtx_rp$data$ID[x],dtx_rp$data$CDATE[x],protocol_name,sp_lookup)})))

for(evt in protocol_name){
  dtx_rp$data<-cleanup(dtx_rp$data,EVTvari = evt)
}
output<-proc_transfer(dtx_rp = dtx_rp,idmap = idmap,upload = ifupload,metals = metals,misscodeallowed = misscodeallowed,arm_num=arm_num,
                      cleanup = clean,protocol_name=protocol_name,ID_fieldname=ID_fieldname)

df_a = dtx_rp$data




test_cirs<-readxl::read_xlsx(file.path(txpath,"test cirs","old_cirsg.xlsx"))
lol_a<-match_evt_clean_up(test_cirs,TimeDiffMax = 30,protocol_name = protocol_name,sp_lookup = sp_lookup)
lol_b<-bsrc.findid(df = lol_a,idmap = idmap,id.var = "ID")

protocol_name <- c("PROTECT2","EXPLORE","SNAKE","EYE_DECIDE")
dtx_r<-readxl::read_xlsx(file.path(dirname(txpath),"SSIcurr_cv.xlsx"))
dtx_rp<-unpack(dtx_r)
dtx_rp$data<-match_evt_clean_up(df_a = dtx_rp$data,TimeDiffMax = 30,protocol_name = protocol_name,sp_lookup = sp_lookup,cleanout = F)


Replacementlist<-lapply(as.character(na.omit(dtx_rp$map$RC_name)),function(a) {list("3"="dk","4"="refuse","5"="na")})
names(Replacementlist)<-as.character(na.omit(dtx_rp$map$RC_name))
ssi_cur<-proc_transfer(dtx_rp = dtx_rp,idmap = idmap,upload = F,metals = metals,misscodeallowed = 1:25,Replacementlist=Replacementlist,
              cleanup = T,protocol_name = c("PROTECT2","EXPLORE","SNAKE","EYE_DECIDE"),ID_fieldname = "masterdemoid",arm_num = 2)
ssi_cur$transfer$registration_redcapid<-ssi_cur$transfer$masterdemoid; ssi_cur$transfer$masterdemoid<-NULL
REDCapR::redcap_write(ssi_cur$transfer,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)


protocol_name <- c("PROTECT2","EXPLORE","SNAKE","EYE_DECIDE")
dtx_r<-readxl::read_xlsx(file.path(dirname(txpath),"SSIworst_cv.xlsx"))
dtx_rp<-unpack(dtx_r)
dtx_rp$data$WORSTSI<-convert_exceldate(dtx_rp$data$WORSTSI)
dtx_rp$data<-match_evt_clean_up(df_a = dtx_rp$data,TimeDiffMax = 30,protocol_name = protocol_name,sp_lookup = sp_lookup,cleanout = F)


Replacementlist<-lapply(as.character(na.omit(dtx_rp$map$RC_name)),function(a) {list("3"="dk","4"="refuse","5"="na")})
names(Replacementlist)<-as.character(na.omit(dtx_rp$map$RC_name))
Replacementlist$ssi_idead_worst<-NULL
ssi_cur<-proc_transfer(dtx_rp = dtx_rp,idmap = idmap,upload = F,metals = metals,misscodeallowed = 1:25,Replacementlist=Replacementlist,
                       cleanup = T,protocol_name = c("PROTECT2","EXPLORE","SNAKE","EYE_DECIDE"),ID_fieldname = "masterdemoid",arm_num = 2)
ssi_cur$transfer$registration_redcapid<-ssi_cur$transfer$masterdemoid; ssi_cur$transfer$masterdemoid<-NULL
REDCapR::redcap_write(ssi_cur$transfer,redcap_uri = ptcs$protect$redcap_uri,token = ptcs$protect$token)








proc_transfer<-function(dtx_rp,idmap,upload=T,metals,misscodeallowed=c(1),cleanup=T,protocol_name,ID_fieldname,arm_num,Replacementlist=NULL) {
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
##########other stuff#######
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















