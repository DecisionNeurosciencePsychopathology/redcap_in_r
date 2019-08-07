###
devtools::install_github("DecisionNeurosciencePsychopathology/redcap_in_r")

son1<-bsrc.switcher(name = "sonrisa1",redcap_uri = "https://www.ctsiredcap.pitt.edu/redcap/api/",token = "972159497F5F16A8F7DA29417F3C7D9B",protocol.cur = F)
son2<-bsrc.switcher(name = "sonrisa2",redcap_uri = "https://www.ctsiredcap.pitt.edu/redcap/api/",token = "015B64BD1703B792B7B448A4C13F3703",protocol.cur = F)

ptc.from<-son2
ptc.to<- son1

idmap<-son.getideventmap(ptc.from = ptc.from)
map<-read.csv("~/Desktop/depends & panas-poms.csv")


bakcup.son1<-bsrc.checkdatabase2(online = T,protocol = son1)

see<-dnpl.redcap2redcap.ssub(ptc.from=son2,ptc.to=son1,online=T,idmap=idmap,map=map,
                                  trigger.a=NULL,trigger.b=NULL,
                                  data.from=NULL,data.to=NULL,idvariable.from="record_id",
                                  idvariable.to="record_id",
                                  overwrite=F,bypass=T,functioncall=NULL,upload = T,output=T)


jiazhou.startup()
evt_name<-"bsocial"
IDfield<-"registration_redcapid"
motion_thres<-0.10
vox_thres<-0.20

curdb<-bsrc.checkdatabase2(protocol = ptcs$scandb,online = T,batch_size="1000")
dt_evt<-bsrc.getevent(curdb = curdb,eventname = paste0(evt_name,"_arm_1"))
dt_evt<-bsrc.getform(formname = evt_name,aggressivecog = F,curdb = scandb)
dt_allqc<-bsrc.getform(formname = paste0(evt_name,"_qc"),aggressivecog = F,curdb = curdb)

ssub_info<-dt_evt[c(IDfield,paste(evt_name,c("group","scandate","which","ps","generalcom","btcom"),sep="_"))]
ssub_info[[paste0(evt_name,"_group")]]<-bsrc.valuetostring(variname = paste0(evt_name,"_group"),valuein = ssub_info[[paste0(evt_name,"_group")]],metadata = curdb$metadata)
names(ssub_info)<-c(IDfield,"GROUP","ScanDate","SCANNER","PT STATUS","GENERAL COMMENT","TRAINING COMMENT")
message("Detected tasks: ",paste(gsub(paste0(evt_name,"_sta_"),"",names(dt_evt)[grepl(paste0(evt_name,"_sta_"),names(dt_evt))]),collapse = ", "),". If missing any, make sure variable names are correct.")

all_tsks<-gsub(paste0(evt_name,"_sta_"),"",names(dt_evt)[grepl(paste0(evt_name,"_sta_"),names(dt_evt))])
IDorder<-ssub_info[[IDfield]]
allqcout<-lapply(all_tsks,function(tsk_name){
  gx_qc<-dt_allqc[,c(which(curdb$metadata$field_note[match(names(dt_allqc),curdb$metadata$field_name)]==tsk_name))]
  if(ncol(gx_qc)>0){
    gx_qc[[IDfield]]<-dt_allqc[[IDfield]]
    gx_qc$moqc_com<-gx_qc$moqc_pass<-gx_qc$voxqc_com<-gx_qc$voxqc_pass<-"INCOMPLETE";
    gx_qc$moqc_com[which(apply(gx_qc[grepl("_moqc_",names(gx_qc))],1,function(x) {any(as.logical(x))} ))]<-"PART-COMPLETED"
    gx_qc$moqc_com[which(apply(gx_qc[grepl("_moqc_",names(gx_qc))],1,function(x) {!any(!as.logical(x))} ))]<-"COMPLETED"
    gx_qc$voxqc_com[which(apply(gx_qc[grepl("_voxqc_",names(gx_qc))],1,function(x) {any(as.logical(x))} ))]<-"PART-COMPLETED"
    gx_qc$voxqc_com[which(apply(gx_qc[grepl("_voxqc_",names(gx_qc))],1,function(x) {!any(!as.logical(x))} ))]<-"COMPLETED"
    gx_qc$moqc_pass[which(apply(gx_qc[grepl("_outper_",names(gx_qc))],1,function(x) {any(as.numeric(x) >=  motion_thres)}))]<-"PART-FAILED"
    gx_qc$moqc_pass[which(apply(gx_qc[grepl("_outper_",names(gx_qc))],1,function(x) {!any(as.numeric(x) >=  motion_thres)}))]<-"PASSED"
    gx_qc$voxqc_pass[which(apply(gx_qc[grepl("_voxsuv_",names(gx_qc))],1,function(x){any(as.numeric(x) < vox_thres)}))]<-"PART-FAILED"
    gx_qc$voxqc_pass[which(apply(gx_qc[grepl("_voxsuv_",names(gx_qc))],1,function(x){!any(as.numeric(x) < vox_thres)}))]<-"PASSED"
    gx_qc$passed_both<-as.logical(apply(gx_qc[c("moqc_pass","voxqc_pass")]=="PASSED",1,all))
   
    kr_qc<-dt_evt[c(IDfield,
      names(dt_evt)[grepl(paste0(evt_name,"_sta_"),names(dt_evt))][grepl(tsk_name,names(dt_evt)[grepl(paste0(evt_name,"_sta_"),names(dt_evt))])],
      names(dt_evt)[grepl(paste0(evt_name,"_mo_"),names(dt_evt))][grepl(tsk_name,names(dt_evt)[grepl(paste0(evt_name,"_mo_"),names(dt_evt))])]
    )]
    names(kr_qc)<-c(IDfield,"mark_com","mark_mo")
    zt_qc<-merge(kr_qc,gx_qc,by = IDfield,all = T)
    zt_qc<-zt_qc[match(zt_qc[[IDfield]],IDorder),]
    zr_qc<-zt_qc[c("mark_com","mark_mo","moqc_com","moqc_pass","voxqc_com","voxqc_pass","passed_both")]
    #names(gr_qc)<-paste(tsk_name,names(gr_qc),sep = "_")
    return(zr_qc)
  } else {return(NULL)}
})
names(allqcout)<-all_tsks
df_qcout<-do.call(cbind,cleanuplist(allqcout))
df_qcout[[IDfield]]<-IDorder
for (xname in all_tsks) {
  if(!is.null(allqcout[[xname]])){
    allqcout[[xname]]$TASKNAME<-xname
  }
}
#refine
sp_qcdfrx<-lapply(allqcout,function(dfrx){
  if(is.null(dfrx)){return(NULL)}else{
  dfrx[[IDfield]]<-IDorder
  dfrx<-dfrx[-which( (is.na(dfrx$mark_com) | dfrx$mark_com==0) & (is.na(dfrx$moqc_com) | dfrx$moqc_com=="INCOMPLETE")),]
  dfrx$QCstatus<-""
  dfrx$QCstatus[which(dfrx$passed_both)]<-"PASSED"
  dfrx$QCstatus[which(dfrx$moqc_pass!="PASSED" & dfrx$moqc_pass!="INCOMPLETE")]<-"FAILED MOTION"
  dfrx$QCstatus[which(dfrx$voxqc_pass!="PASSED" & dfrx$voxqc_pass!="INCOMPLETE")]<-"FAILED VOX"
  
  dfrx$Status<-""
  dfrx$Status[which(dfrx$mark_com=="1")]<-  paste(dfrx$Status[which(dfrx$mark_com=="1")],"MARKED COMPELTED",sep = ",")
  dfrx$Status[which(dfrx$moqc_com=="COMPLETED")]<-paste(dfrx$Status[which(dfrx$moqc_com=="COMPLETED")],"PREPROCed",sep = ",")
  dfrx$Status[which(dfrx$voxqc_com=="COMPLETED")]<-paste(dfrx$Status[which(dfrx$voxqc_com=="COMPLETED")],"QCed",sep = ",")  
  dfrx$Status[which(dfrx$QCstatus!="")]<-paste(dfrx$Status[which(dfrx$QCstatus!="")],
                                               dfrx$QCstatus[which(dfrx$QCstatus!="")],sep = ",")  
  dfry<-cbind(dfrx,ssub_info[match(dfrx[[IDfield]],ssub_info[[IDfield]]),])
  return(dfry)
  }
})

for(dftx in sp_qcdfrx){
  if(!is.null(dftx)){
    write.csv(dftx,paste("scanreport_outputtable_",unique(dftx$TASKNAME),".csv",sep = ""))
    print(unique(dftx$TASKNAME))
    print(table(dftx$GROUP,dftx$Status))
  }
}












