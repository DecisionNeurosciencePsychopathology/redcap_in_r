###
#Title: Migrator
#Author: Jiazhou Chen
#Version: 0.1

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

son.getideventmap<-function(ptc.from=NULL,data.from=NULL,...){
  if (is.null(data.from)) {
    data.from<-bsrc.checkdatabase2(protocol = ptc.from,online = T)
  }
  id<-son.getidmap(data.from = data.from,...)
  working.f<-son.whichvisit(data.from = data.from,...)
  merge(id,working.f,all = T,by.x = "idfield.from", by.y = "subject_id")->id.map
  f.id.map<-na.omit(id.map)
  return(f.id.map)
}




