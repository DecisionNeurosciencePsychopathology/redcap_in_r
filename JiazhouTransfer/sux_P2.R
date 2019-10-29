



cdb <- bsrc.checkdatabase2(protocol = ptcs$protect,online = T)
essentialdatesvari<-cdb$metadata[which(cdb$metadata$field_note=="EVT_DATEFIELD"),c("field_name","form_name")]
essentialevtname<-cdb$eventmap[cdb$eventmap$form %in% cdb$metadata$form_name[cdb$metadata$field_name %in% essentialdatesvari$field_name],]
essentialevtname$field_name<-essentialdatesvari$field_name[match(essentialevtname$form,essentialdatesvari$form_name)]
missingcriticaldates<-do.call(rbind,lapply(essentialevtname$unique_event_name,function(evt){
  allevt<-bsrc.getevent(eventname = evt,curdb = cdb,mod = T)
  outdf<-allevt[is.na(allevt[essentialevtname$field_name[which(essentialevtname$unique_event_name == evt)]]),c("registration_redcapid","redcap_event_name")]
  if(nrow(outdf)>0){
  outdf$MissingDateField<-essentialevtname$field_name[which(essentialevtname$unique_event_name == evt)]
  return(outdf)
  } else {
    return(NULL)
  }
})
)

Txmap_path<-"~/Box/skinner/data/RedCap Data/redcap_synced_grid.xlsx"
Txmap<-readxl::read_excel(Txmap_path)
ptcs_variable<-"registration_ptcstat"
dest_ptcname<-"protect2"
bsrc.onewaysync<-function(){
  db_og<-bsrc.checkdatabase2(ptcs$masterdemo)
  db_target<-bsrc.checkdatabase2(ptcs$protect)  
  
  optMap<-Txmap[c("Names",db_og$name,db_target$name,"DirectTransfer")]
  optMap$DirectTransfer[which(grepl(".* - DIRECT",optMap[[3]]))]<-T
  message("Currently only support direct transfer ones.")
  optMap<-na.omit(optMap[which(as.logical(optMap$DirectTransfer)),])
  
  #og_dt<-
  
  grep(ptcs_variable,names(db_og$data))
  
   
    
}


index_df<-bsrc.getSUIHX_index(protocol = ptcs$protect,suicide_formname = "ongoing_suicide_hx_lethality")
sux_df<-bsrc.getform(protocol = ptcs$protect,grabnewinfo = T,formname = "ongoing_suicide_hx_lethality",batch_size=1000L)
melt_sux_df<-melt(sux_df,id.vars=c(index_df$names[index_df$SingleEntry]))
meltxa<-cbind(melt_sux_df,index_df[match(as.character(melt_sux_df$variable),index_df$names),])
meltxa<-meltxa[!meltxa$is_checkbox,]
reshape_sux<-reshape2::dcast(meltxa,value.var = "value",formula = registration_redcapid+rxsim1~root_names)



sux_df<-bsrc.getform(protocol = ptcs$bsocial,grabnewinfo = T,formname = "suicide_history",batch_size=1000L)



index_df<-bsrc.getSUIHX_index()
index_df$names[index_df$root_names=="sahx_lr"]

IDvars<-c("registration_redcapid","redcap_event_name")
simp<-sux_df[c(IDvars,index_df$names[index_df$SingleEntry])]
sub_ex<-sux_df[index_df$names[index_df$root_names=="sahx_lr"]]

evt_value<-apply(sub_ex,1,max,na.rm=T)
evt_value[evt_value==-Inf]<-NA
simp[["sahx_lr"]]<-evt_value

masterdemo<-bsrc.checkdatabase2(ptcs$masterdemo,forceskip = T)

xrmp<-bsrc.findid(simp,idmap = masterdemo$data[c("registration_redcapid","registration_group","registration_lethality")],id.var = "registration_redcapid")
NATTwithLeth<-xrmp[!is.na(xrmp$sahx_lr) & xrmp$registration_group!="ATT",]


