###
#Title: Migrator
#Author: Jiazhou Chen
#Version: 0.2

#Functions in this script will deal with migrating data between RedCap projects as well as migration from/to access database

#to migrate from scannig database:
dnpl.envir2df<-function(x.envir){
  if (class(x.envir)!="environment") stop("Argument is not an environment object!")
  if (any(eapply(x.envir,class)=="data.frame")) stop("Environment contains data.frame object already, filter please")
  as.list(x.envir)->x.list
  attr(x.list, 'row.names')<-names(x.list)
  x.df<-as.data.frame(x.list)
  return(x.df)
}

dnpl.access2redcap<-function(x,map,eventvariable="redcap_event_name"){
  if (!any(class(x) %in% "data.frame")) stop("x object has to be a data.frame object")
  as.environment(x)->y.envir
  map[map==""]<-NA
  accu<-new.env(parent = emptyenv())
  for (event in names(map)[-grep("original",names(map))]){
    map.cur<-na.omit(subset(map,select = c("original",event)))
    prc<-new.env(parent = emptyenv())
    nothing<-apply(map.cur,1,function(x) {
      assign(x[2],get(x[1],envir = y.envir),envir = prc)
    })
    x.df<-dnpl.envir2df(prc)
    strforeval<-paste("x.df$",eventvariable,"<-event",sep = "")
    eval(parse(text = strforeval))
    x.df$redcap_event_name<-event
    assign(event,x.df,envir = accu)
  }
  if(exists("z")) {rm(z)}
  invisible(eapply(accu,function(x){
    if (exists("z",envir =parent.env(environment()))){
      z<-get("z",envir = parent.env(environment()))
      assign("z",merge(x,z,all=T),envir = parent.env(environment()))
    }else{
      assign("z",x,envir = parent.env(environment()))
    }
  }))
  return(z)
}

son.getidmap<-function(ptc.from=son2,idfield.from="subject_id",idfield.to="otherid_1",data.from=NULL,sonfilter=T) {
  if (is.null(data.from)) {
    data.from<-bsrc.checkdatabase2(protocol = ptc.from,online = T)
  }
  from.data<-data.from$data
  data.idfields<-from.data[,c(idfield.from,idfield.to)]
  data.idfields[data.idfields==""]<-NA
  names(data.idfields)<-c("idfield.from","idfield.to")
  data.idfields<-na.omit(data.idfields)
  #Verify so that >900 or not even a number will not survive
  if (sonfilter) {
    data.idfields<-data.idfields[-which(suppressWarnings(unlist(lapply(sapply(strsplit(data.idfields$idfield.to,split = "_"),"[[",2), function(x) {
      as.numeric(x)->x
      if (!is.na(x)){if(x>900) {x<-NA}}
      is.na(x)
    })))),]
  }
  rownames(data.idfields)<-NULL
  return(data.idfields)
}

son.whichvisit<-function(ptc.from=son2,data.from=NULL){
  if (is.null(data.from)) {
    data.from<-bsrc.checkdatabase2(protocol = ptc.from,online = T)
  }
  idmatch<-data.from$data[,c("record_id","subject_id")]
  idmatch[idmatch==""]<-NA
  na.omit(idmatch)->idmatch
  data.from$data[,c("record_id","redcap_event_name","med_type")]->working
  working[grep("Plac",working$med_type),]->working.f
  working.f[working.f==""]<-NA
  merge(working.f,idmatch,all = T,by.x = "record_id", by.y = "record_id")->working.f
  return(working.f)
}

son.getideventmap<-function(ptc.from=NULL,data.from=NULL,naomit=T,...){
  if (is.null(data.from)) {
    data.from<-bsrc.checkdatabase2(protocol = ptc.from,online = T)
  }
  id<-son.getidmap(data.from = data.from,...)
  working.f<-son.whichvisit(data.from = data.from,...)
  merge(id,working.f,all = T,by.x = "idfield.from", by.y = "subject_id")->id.map
  if(naomit=T){
  f.id.map<-na.omit(id.map)} else {f.id.map<-id.map}
  return(f.id.map)
}


dnpl.redcap2redcap.ssub<-function(ptc.from=NULL,ptc.to=NULL,online=T,idmap=NULL,map=NULL,
                                  trigger.a=NULL,trigger.b=NULL, 
                                  data.from=NULL,data.to=NULL,idvariable.from="record_id",
                                  idvariable.to="record_id",
                                  overwrite=F,bypass=F,functioncall=NULL,upload=T,output=F,...) {
  #This function is used for transfer data 
  if (is.null(ptc.from) | is.null(ptc.to)) {stop("Need both protocol objects")}
  if (is.null(map)) {stop("Mapping is missing")}
  if (online) {print("Online mode is on, will alway retrive from RedCap server...")}
  if (is.null(data.from)) {
    data.from<-bsrc.checkdatabase2(protocol = ptc.from,online = T)
  }
  if (is.null(data.to)) {
    data.to<-bsrc.checkdatabase2(protocol = ptc.to,online = T)
  }
  
  #Generalizing function:
  colnames(data.from$data)[grep(idvariable.from,names(data.from$data))]<-"record_id"
  colnames(data.to$data)[grep(idvariable.to,names(data.to$data))]<-"record_id"
  #colnames(idmap)[grep(idvariable.from,names(idmap))]<-"record_id"
  if (is.null(idmap)) {print("No ID mapping provided, will carry over IDs from 'a' to 'b' and transfer everyone's data")
    #Need to generate ID Event Map Here:
    idmap<-data.frame(idfield.from=unique(data.from$data$record_id),
                      idfield.to=unique(data.from$data$record_id))
  }
  
  #We will use person loops to ensure it always checks for completion before uploading
  map->map.backup
  
  #for now, we will use single subject loop, not sure how efficient it would be but that's what's up:
  envir.ept<-as.environment(list())
  for (i in 1:length(idmap$record_id)) {
    map<-map.backup
    id<-idmap$record_id[i]
    from.id<-idmap$idfield.from[i]
    to.id<-idmap$idfield.to[i]
    subevetn<-idmap$redcap_event_name[i]
    record.id.to<-data.to$data$record_id[match(idmap$idfield.to[which(idmap$record_id==id)],data.to$data$subject_id)]
    print(paste("Processing from ID: ",from.id," to ID: ",to.id," now...",sep = ""))
    
    #Fix depends
    if (any(map$from.event %in% "depends")) {
      map$from.event[which(map$from.event %in% "depends")]<-idmap$redcap_event_name[idmap$record_id==id]
    }
    #Clean up personal map:
    if (length(map$fixed)>0){
      map<-map[which(map$from.event==idmap$redcap_event_name[idmap$record_id==id] | map$fixed),]
    }
    
    #Checking status of completion and decide if to overwrite data
    if (!overwrite) {
      checkstatuslist<-paste(as.character(map$to.form),"_complete",sep = "")
      tobeinvestigate<-data.to$data[c("record_id","redcap_event_name",checkstatuslist)]
      tobeinvestigate<-tobeinvestigate[which(tobeinvestigate$redcap_event_name %in% unique(map$to.event)),]
      tobeinvestigate[which(tobeinvestigate$record_id==record.id.to),]->singlesub
      missingforms<-as.character(map$to.form[as.numeric(apply(singlesub[-c(1,2)],1,function(x) {which(x==0)
      }))])
      } else {
        missingforms<-map$from.form
      }
    map[which(missingforms %in% map$from.form),]->map
    
    #Step Verification:
    #Even with just 0 length, the map will pass, allowing the loop to continue; 
    if (!bypass){
      if (!any(is.na(match(interaction(map$from.event,map$from.form),interaction(data.from$eventmap$unique_event_name,data.from$eventmap$form))))){
        print("Passed form varification")
      } else stop("Hey,some of these form names are not in the actual database, can't grab 'em if they ain't there!")
      vari.from<-data.from$metadata$field_name[which(data.from$metadata$form_name %in% as.character(map$from.form))]
      vari.to<-data.to$metadata$field_name[which(data.to$metadata$form_name %in% as.character(map$to.form))]
      if (!any(is.na(match(vari.from,vari.to)))) {
        print("Passed variable level varification")
      } else {stop("Hey,some of the variables in from/to forms don't match, go fix it, use bypass=T or just use variable level match!")
        print(vari.from[which(is.na(match(vari.from,vari.to)))])}
    }
    #Set up another loop to loop over to data event; i.e. only upload per person per event. 
    if (length(map$from.form)>0) {
      for (s.event in unique(map$from.event)) {
        print(s.event)
        map$from.form[which(map$from.event %in% s.event)]->from.form
        transfer.from.a<-bsrc.getform(curdb = data.from,res.event = s.event,formname = from.form)
        #transfer.from.a[which(transfer.from.a$redcap_event_name==s.event),]->transfer.from.b
        transfer.from.a[which(transfer.from.a$record_id==id),]->transfer.from.c
        transfer.from.c$record_id<-record.id.to
        transfer.from.c$redcap_event_name<-as.character(map$to.event[which(as.character(map$from.event) %in% s.event)[1]])
        
        #Call for additional function if needed:
        if (!is.null(functioncall)){do.call(functioncall,args = list(transfer.from.c))}
        #Change ID variable back to what they are supposed to be:
        colnames(data.from$data)[grep("record_id",names(data.from$data))]<-idvariable.to
        colnames(data.to$data)[grep("record_id",names(data.to$data))]<-idvariable.to
        colnames(idmap)[grep("record_id",names(idmap))]<-idvariable.to
        
        if (upload) {
          transfer.from.c$mini_d3summary<-NULL
          REDCapR::redcap_write(transfer.from.c,redcap_uri = ptc.to$redcap_uri,token = ptc.to$token)
        } 
        if (output) {
          assign(as.character(interaction(from.id,to.id),transfer.from.c,envir = envir.ept))
        }
      } #End Event Loop 
    }else {
      print("Nothing to upload...next person...")
    } # End if length check
  } # End subject loop
  
  if (output) {
    return(envir.ept)
  }
  
  #Step Checking to make sure:
  #if (length()>0 | bypass) {
  #  
  #  
  #} else if (length()==0) {print("nothing to upload at all...wasting my time...argh...") 
  # pass<-false} else if (bypass) {"Bypassed checking procedure...risk of data contemedation is possible."}

}


rm_noinfopt<-function(ptcs) {
  ptcs[which(sapply(lapply(ptcs,function(x) {x$rdpath}),is.null))]<-NULL
  return(ptcs)
}

dnpl.updatedemostatus<-function(ptc_demo=NULL){
  masterdemo<-bsrc.conredcap2(protocol = ptc_demo,updaterd = T,output = T,returnas="list")$data
  masterdemo<-bsrc.checkbox(masterdemo,variablename = "registration_ptcstat")
  lapply(masterdemo$registration_redcapid, function(id) {
      
  })
}

proc_iddf<-function(iddf=NULL,patterns=c("SON2_","SON1_","NOP3_"),preserve=c("record_id")){
  iddf<-iddf[apply(iddf,1,function(x) {length(which(is.empty(x)))}) < length(iddf)-1,]
  dfx<-as.data.frame(lapply(patterns,function(pd) {
    apply(iddf,1, function(d) {
      ld<-d[grep(x = d, pattern=pd)] 
      names(ld)<-NULL
      if (length(ld)>0) {return(ld)} else {return(NA)}
    })
  }))
  names(dfx)<-paste0(patterns,"ID")
  dfz<-cbind(iddf[preserve],dfx)
  return(dfz)  
}

dnpl.sync.ptcstatus<-function(...) {
  ptcx<-rm_noinfopt(list(...))
  mtt<-lapply(ptcx,function(ptc) {
    dtt<-bsrc.conredcap2(protocol = ptc,returnas = "envir",fullupdate = F, output = T, updaterd = F)
    dtx<-data.frame(ID=REDCapR::redcap_read(fields = dtt$metadata$field_name[1],redcap_uri = ptc$redcap_uri,token = ptc$token)$data,
                    name=ptc$name)
    return(dtx)
  })
  names(mtt)<-sapply(mtt,function(dtx){return(unique(dtx$name))})
  
  
}


dnpl.sync<-function(...,syncform=c("record_registration","registration_and_termination","progress_check")){ 
  #We clean up first, functionalized this in case this turn into something crazy
  ptcx<-rm_noinfopt(list(...))
  dt<-lapply(ptcx,function(ptc) {
    dtt<-bsrc.checkdatabase2(protocol = ptc,returnas = "envir",expiration = 4)
    dtt$name<-ptc$name
    return(dtt)
    })
  names(dt)<-sapply(dt,function(dtx){return(dtx$name)})
  allmeta<-do.call(rbind,lapply(dt,function(x1){
    x1x<-x1$metadata[which(x1$metadata$form_name %in% syncform),]
    x1x$study<-x1$name
    return(x1x)
    }))
  # allevent<-do.call(rbind,lapply(dt,function(x2){
  #   x2x<-x2$eventmap[which(x2$eventmap$form %in% syncform),]
  #   if (is.null(x2x)) {x2x<-data.frame(arm_num=NA,unique_event_name=NA,form=NA)}
  #   x2x$study<-x2$name
  #   return(x2x)
  # }))
  metamap<-do.call(rbind,lapply(allmeta$field_note[duplicated(allmeta$field_note) & nchar(allmeta$field_note)>0],function(x2) {
    return(allmeta[which(allmeta$field_note==x2),c("field_note","field_name","form_name","study")])
  }))
  
  dt_sub<-lapply(unique(metamap$form_name), function(x3) {
    x3r<-lapply(unique(metamap$study[metamap$form_name==x3]), function(x3a) {bsrc.getform(formname = x3,curdb = dt[[x3a]])})
    names(x3r)<-unique(metamap$study[metamap$form_name==x3])
    return(x3r)
    })
  names(dt_sub)<-unique(metamap$form_name)
  
  
  
  return(dt)
  }

dnpl.reworkdemo<-function(curdb,syncform=c("record_registration","registration_and_termination","progress_check")){
  metax<-curdb$metadata[which(curdb$metadata$form_name %in% syncform),]
  map<-bsrc.getchoicemapping(variablenames = metax[grep("group",metax$field_note),]$field_name,metadata = curdb$metadata)
  curdb$data[metax[grep("group",metax$field_note),]$field_name]<-plyr::mapvalues(x=curdb$data[[metax[grep("group",metax$field_note),]$field_name]],
                                                                                 from = map$choice.code,to = as.character(map$choice.string))
  curdb$data[[metax[grep("group",metax$field_note),]$field_name]]->groupvar
  groupvar[grep(paste("healthy control","hc",sep = "|"),groupvar,ignore.case = T)]<-"HC"
  groupvar[grep(paste("not sure yet","nsy","88",sep = "|"),groupvar,ignore.case = T)]<-"88"
  groupvar[grep(paste("Ineligible / Not Applicable","Ineligible","Not Applicable","89",sep = "|"),groupvar,ignore.case = T)]<-"89" 
  groupvar[grep(paste("Non-suicidal","Non-suicidal/Non-depressed patient","Non-depressed","non",sep = "|"),groupvar,ignore.case = T)]<-"NON"
  groupvar[grep(paste("High Lethality","HL",sep = "|"),groupvar,ignore.case = T)]<-"HL"
  groupvar[grep(paste("Low Lethality","LL",sep = "|"),groupvar,ignore.case = T)]<-"LL"
  groupvar[grep(paste("Attempter","att",sep = "|"),groupvar,ignore.case = T)]<-"ATT"
  groupvar[grep(paste("Ideator","ide",sep = "|"),groupvar,ignore.case = T)]<-"IDE"
  groupvar[grep(paste("Depressed Control","dep",sep = "|"),groupvar,ignore.case = T)]<-"IDE"
  
  curdb$data$lethality<-NA
  
  
  
  
  }










if (F) {

metareshape<-reshape(metamap,timevar = "study",idvar = "field_note",direction = "wide",sep = "/_|")
dt_sub$record_registration$bsocial
dt_xj<-dt_sub$record_registration$bsocial[which(!is.na(dt_sub$record_registration$bsocial[metamap$field_name[metamap$study=="bsocial" & metamap$field_note=="dnplid"]])),]
dt_xk<-dt_xj[grep(paste(toget,collapse = "|"),names(dt_xj))]
for (i in 1:length(metareshape[[1]])) {
  names(dt_xk)<-gsub(pattern = metareshape[i,paste("field_name","bsocial",sep = "/_|")],
       replacement = metareshape[i,paste("field_name","masterdemo",sep = "/_|")],
       names(dt_xk))
  
  }
dt_xk$registration_ptcstat___bsocial<-1

dt<-dnpl.sync(ptcs$masterdemo,ptcs$bsocial,ptcs$ksocial)
}