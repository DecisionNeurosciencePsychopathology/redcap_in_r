#Masterdemo 
startup()

library(bsrc)

bsocial<-bsrc.checkdatabase2(ptcs$bsocial)
masterdemo<-bsrc.conredcap2(protocol = ptcs$masterdemo,batch_size = 1000L,output = T)
protect<-bsrc.checkdatabase2(ptcs$protect)

idmap<-masterdemo$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
names(idmap)<-c("masterdemoid","wpicid","soloffid")

reg_demo<-bsrc.getform(formname = "record_registration",curdb = bsocial)

reg_demo_matchid<-bsrc.findid(df = reg_demo,idmap = idmap,id.var = "registration_redcapid")

#Get the existing folks:
existingBSDEMO<-reg_demo_matchid[which(reg_demo_matchid$ifexist),]
if(any(existingBSDEMO$registration_redcapid != existingBSDEMO$masterdemoid)){stop("YO! Mismatch IDs!!!")}

notEXDEMO<-reg_demo_matchid[which(!reg_demo_matchid$ifexist),]
notEXDEMO<-bsrc.findid(df = notEXDEMO,idmap = idmap,id.var = "registration_soloffid")
notEXDEMO<-notEXDEMO[which((!is.na(as.numeric(notEXDEMO$registration_redcapid)))),]
notEXDEMO<-bsrc.change_grp_ptcs(input = notEXDEMO,origin = "bsocial","masterdemo")

notEXDEMO_rcmatch<-bsrc.findid(df = notEXDEMO,idmap = idmap,id.var = "registration_redcapid")

if(any(bsrc.findid(df = notEXDEMO,idmap = idmap,id.var = "registration_id")$ifexist)) {stop("CHECK WPIC ID")}
directtransvarinames<-masterdemo$metadata$field_name[which(masterdemo$metadata$field_name %in% gsub("___.*","",names(notEXDEMO_rcmatch)) )]

print("list of untransferables")
gsub("___.*","",names(notEXDEMO_rcmatch))[which(!gsub("___.*","",names(notEXDEMO_rcmatch)) %in% directtransvarinames)]


targ_a<-notEXDEMO_rcmatch[grep(paste(directtransvarinames,collapse = "|"),names(notEXDEMO_rcmatch))]

REDCapR::redcap_write(targ_a,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)


bsrc.change_grp_ptcs<-function(input=NULL,origin=c("bsocial","protect","masterdemo"),destination=c("bsocial","protect","masterdemo")){
  if(is.null(input)){message("No input, supports data.frame (must specify group name) or vector string")}
  if((!origin %in% c("bsocial","protect","masterdemo")) | (!destination %in% c("bsocial","protect","masterdemo")) ) {
    message("Only supports the following:",c("bsocial","protect","masterdemo"))}
  grp_map<-data.frame(masterdemo=c("HC","NON","DEP","IDE","ATT","ATT","ATT","88","89"),
                  bsocial=c("1","4","4","4",NA,"2","3","88","89"),
                  protect=c("HC","NON","DEP","IDE","ATT","ATT","ATT","88","89"),stringsAsFactors = F
  )
  vari_map<-data.frame(masterdemo="registration_group",
                       bsocial="registration_group",
                       masterdemo="startup_group",stringsAsFactors = F
  )
  if(!is.data.frame(input)){input<-as.character(input)}
  
  switch (class(input),
          "character" = {
            noorder<-is.na(input)
            input<-plyr::mapvalues(x = input,from = grp_map[[origin]],to = grp_map[[destination]],warn_missing = F)
            input[noorder]<-NA
          },
          "data.frame" = { noorder<-is.na(input[[vari_map[[origin]]]])
            input[[vari_map[[destination]]]]<-plyr::mapvalues(x = input[[vari_map[[origin]]]],from = grp_map[[origin]],to = grp_map[[destination]],warn_missing = F)
          if (vari_map[[origin]] != vari_map[[destination]]){input[[vari_map[[origin]]]]<-NULL};
            input[[vari_map[[destination]]]][noorder]<-NA
          return(input)},
  )
}






