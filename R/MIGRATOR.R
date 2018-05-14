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

