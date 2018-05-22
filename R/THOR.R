#Script use to get people who are proc'ed
#This will be intergrated with ARC and new RedCap scanning db
#So far getting pre-proc people done 
#thorndike.switcher()
#thorndike.updaterc(index.list = thorndike.getfuncproclist())


thorndike.getfuncproclist<-function(rootdir="/Volumes/bek",smartfind=T,dir.pattern="MR_Proc",proc.pattern="*nfsw*.nii.gz",id.pattern="[0-9]{4,6}",mprage.pattern="mprage",mapping=NULL,...) {
  #currently only want to use this 'smartfind' method...although not really smart...just codes...and logic...oh well
  if (smartfind){
  pro.dir<-system(paste0("find ",rootdir," -mindepth 1 -maxdepth 2 -iname \"", dir.pattern, "\" -type d"), intern=TRUE)
  dir<-unlist(pro.dir)
  attr(dir,which = "status")<-NULL
  finddir<-strsplit(dir,split = "/")
  tasklist<-c()  
  for (i in finddir){
    tasklist<-c(tasklist,i[which(i==dir.pattern)-1])
  }
  print(paste("Found following studies:",paste(tasklist,collapse = ","),sep = " "))
  index<-data.frame()
  for (z in 1:length(pro.dir)) {
    #func
    l<-system(paste0("find ",pro.dir[z]," -mindepth 1 -maxdepth 4 -iname ", proc.pattern), intern=TRUE)
    l.b<-strsplit(l,split="/")
    if (length(l.b)>0) {
    test.x<-l.b[[1]]
    id.pos<-which(grepl(id.pattern,test.x,perl = T))
    if (length(id.pos)>1){
    id.pos<-max(id.pos) 
    }

    study<-as.character(sapply(l.b,"[[",which(test.x %in% tasklist)))
    paran<-as.character(sapply(l.b, "[[",length(test.x)-1))
    para.name<-as.character(unlist(lapply(paran,function(x){
      substring(x,last=regexpr("[0-9]{1,3}",x)[1]-1,first = 0)
    })))
    para.count<-as.numeric(unlist(lapply(paran,function(x){
      substring(x,first=regexpr("[0-9]{1,3}",x)[1])
    })))
    id<-as.character(sapply(l.b, "[[",id.pos))
    y<-data.frame(id,study,para.name,para.count)
    y<-y[which(!duplicated(y)),]
    rm(list = c("id","para.count","para.name","study","paran"))
    yexist<-T
    } else {yexist<-F}
    #mprage

    k<-system(paste0("find ",pro.dir[z]," -mindepth 1 -maxdepth 2 -iname \"", mprage.pattern, "\" -type d"), intern=TRUE)
    k.b<-strsplit(k,split="/")
    if (length(k.b)>0) {
    test.x<-k.b[[1]]
    rageid.pos<-which(grepl(id.pattern,test.x,perl = T))
    if (length(rageid.pos)>1){
      rageid.pos<-max(rageid.pos) 
    }
    id<-sapply(k.b, "[[",rageid.pos)
    study<-sapply(k.b,"[[",which(test.x %in% tasklist))
    r<-data.frame(id,study)
    r$para.name<-"mprage"
    r$para.count<-1
    rexist<-T
    } else {rexist<-F}
    
    if (yexist & rexist) {yr<-merge(y,r,all=T)} else if (yexist) {yr<-y} else if (rexist) {yr<-r}
    
    if (z==1) {index<-yr
    }else{index<-merge(index,yr,all=T)}
    }#end loop

#I guess the mapping part will have to wait...
  } #end smartfind
  index.sub<-aggregate(para.count ~ id + study + para.name, data = index, max)
  index.stu<-aggregate(para.count ~ study + para.name, data = index, max)
  
  index.sub.comp<-index.sub[which(!is.na(match(interaction(index.sub$study,index.sub$para.name,index.sub$para.count),interaction(index.stu$study,index.stu$para.name,index.stu$para.count)))),]
  

  return(list(index.study=index.stu,index.proc=index.sub,index.fullproc=index.sub.comp))
}

thorndike.startup<-function(protocol="scandb",mode="offline"){
  source("/Volumes/bek/aux_scripts/thorndike_startup.R")
  thorndike.switcher(protocol.s=protocol,mode=mode)
}

thorndike.updaterc<-function(protocol=protocol.cur,index.list=NULL,preset=T,preproc.pattern="preproc", censor=c("120517.bsocial"),upload=T,output=F,...){
  #grab the metadata from RedCap:
  work<-NULL
  dbinfo<-bsrc.conredcap2(protocol = protocol,updaterd = F,output = T)
  event<-dbinfo$eventmap$unique_event_name
  pat<-paste(Reduce(intersect, strsplit(event,"_")),collapse = "_")
  
  nevent<-unlist(lapply(event, function(x){
    substr(x,start = 0,stop = regexpr(pat,x)-2)
  }))
  dbinfo$eventmap$event<-nevent
  uevent<-unique(nevent)
  stu<-as.character(unique(index.list$index.study$study))
  uform<-dbinfo$eventmap$form[match(uevent,dbinfo$eventmap$event)]
  #started to create mapping...
  map.s<-data.frame(rc=uevent,thorn=stu[match(uevent,stu)],form=uform)
  map.s$thorn<-as.character(map.s$thorn)
  map.s$rc<-as.character(map.s$rc)  
  #Future update to allow custom preset rules independent of the funtion so it's more generalizable
  if (preset){
  print("Will use preset name match")
  map.s$thorn[map.s$rc=="bsocial"]<-"bsocial_7341"
  map.s$thorn[map.s$rc=="bsocial_2016"]<-"bsocial"
  } else {
      for (i in which(is.na(map.s$thorn))){
      print(map.s$rc[i])
      opt<-stu[agrep(map.s$rc[i],stu)]
      if (length(opt)>0) {num<-readline(prompt=paste("options: ",paste(opt,collapse = ", ")," which one?<type number><-1 for none> :",sep = ""))} else {
        print("No match")
        num=-1}
      if (num=="") {num<- -1}
      as.numeric(num)->num
      if (num>0){
        map.s$thorn[i]<-opt[num]
      } else next 
      }
  }
  work<-index.list$index.proc
  #get the ones that's not even in mapping:
  work<-work[which(work$study %in% na.omit(map.s$thorn)),]
  work$study<-plyr::mapvalues(work$study,from = map.s$thorn, to = map.s$rc,warn_missing = F)
  work[]<-lapply(work,as.character)
  
  k<-strsplit(as.character(unique(interaction(work$study,work$para.name))),split = ".",fixed = T)
  map.g<-data.frame(study=sapply(k,"[[",1),para.t=sapply(k, "[[",2),stringsAsFactors = FALSE)
    
  rcmeta<-dbinfo$metadata$field_name[grep(preproc.pattern,dbinfo$metadata$field_name)]
  strsplit(rcmeta,split = "_")->rcmec
  rcmeca<-lapply(rcmec, function(x){
    z<-x[-which(x==preproc.pattern)]
    z[c(which(z %in% map.s$rc),which(!z %in% map.s$rc))]
  })
  map.x<-data.frame(form=sapply(rcmeca, "[[",1),para=sapply(rcmeca, "[[",2),stringsAsFactors = FALSE)
  #tbh...if I had just hard code everything....this should be done in like 4 lines.....instead creating mapping used up 130 lines....not efficient
  map.a<-merge(map.s,map.x,all=T)
  map.g$inter<-interaction(map.g$study,map.g$para.t)
  map.a$inter<-interaction(map.a$rc,map.a$para)
  map.a$para.t<-map.g$para.t[match(map.a$inter,map.g$inter)]
  map.a$para.t<-as.character(map.a$para.t)
  no.t<-map.a[which(is.na(map.a$para.t)),]
  no.t<-no.t[which(no.t$rc %in% map.g$study),]
  for (inv in unique(no.t$rc)){
    sle<-map.g[which(map.g$study==inv),]
    nole<-no.t[which(no.t$rc==inv),]
    c<-lapply(nole$para, function(x) {
      as.character(sle$para.t[grep(x,sle$para.t)])
      })
    d<-unlist(lapply(c, function(x){if(length(x)<1){x<-NA}else{x} }))
    nole$para.t<-d
    map.a<-merge(map.a,nole,all = T)
    }
  map.a<-map.a[-which(duplicated(map.a$inter) & is.na(map.a$para.t)),]
  work$para.name<-map.a$para[match(interaction(work$study,work$para.name),interaction(map.a$rc,map.a$para.t))]
  work$redcap_event_name<-paste(work$study,pat,sep = "_")
  work$form.name<-dbinfo$eventmap$form[match(work$redcap_event_name,dbinfo$eventmap$unique_event_name)]
  work$variable.name<-paste(work$form.name,preproc.pattern,work$para.name,sep = "_")
  #Clean-up:
  work<-work[-which(is.na(work$para.name)),]
  #Censoring out:
  work<-work[-which(interaction(work$id,work$study) %in% censor),]
  if (length(work[which(is.na(match(as.numeric(work$id),dbinfo$data$registration_redcapid))),]$id)>0){
  print(work[which(is.na(match(as.numeric(work$id),dbinfo$data$registration_redcapid))),])
  if (readline(prompt = "These following folks are NOT IN RedCap scandb, should new records be created for them? Answer 'Y' or 'N' :")=="N"){
    work<-work[-which(is.na(match(as.numeric(work$id),dbinfo$data$registration_redcapid))),]
  }
  }
  #reconstruct:
  work$value<-1
  nw<-subset(work,select = c("id","redcap_event_name","variable.name","value"))
  names(nw)[colnames(nw)=="id"]<-"registration_redcapid"
  nw.wide<-reshape(data = nw,v.names = "value",timevar = "variable.name",direction = "wide",idvar = c("registration_redcapid","redcap_event_name"))
  names(nw.wide)[grep(preproc.pattern,names(nw.wide))]<-sapply(strsplit(names(nw.wide)[grep(preproc.pattern,names(nw.wide))],split=".",fixed = T),"[[",2)
  
  
  
  #Preserve RedCap Data (if redcap variable has 1 will not remove)
  #original<-dbinfo$data[c(1,grep(preproc.pattern,names(dbinfo$data)))]
  #original[original==0]<-NA
  #colnames(original)
  #L<-apply(original,2,function(x) {
  #  if (length(which(x==1))>0){
  #    #colnames(original)[
  #      which(x==1)
  #     # ]
  #  }else {NA}
  #})
  #L<-L[!is.na(L)]
  #L<-as.data.frame(L)
  #L$id<-original$registration_redcapid[L$L]
  #L$variable.name<-rownames(L)
  #argh stupid...if not preproc here, it will not even get uploaded....
  
  
  
  #upload to redcap
  if (upload){
  result<-REDCapR::redcap_write(nw.wide,redcap_uri = protocol$redcap_uri,token = protocol$token)
  if (result$success){print("DONE")} else {print("SOMETHING WENT WRONG DURING UPLOADING...")}
  }
  
  if (output){
  return(nw.wide)    
  }
}














