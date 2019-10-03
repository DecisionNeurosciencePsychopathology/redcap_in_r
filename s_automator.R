##########THIS IS THE ACTUAL AUTOMATOR SCRIPT:
setwd("/Users/jiazhouchen/Documents/UPMC/RStation")

#Or any other general script; but since this is running on Jiazhou's machine...
source("Jiazhou.Startup.R")

message("AZ")
###########
# zz <- file("log.txt", open="wt")
# sink(zz, type=c("output","message"))

finish<-FALSE

whichtouse<-as.character(Sys.getenv("whichtouse"))

tryCatch({
  
  jiazhou.startup(load = F)
  
  switch(whichtouse,
         "three" = {
           message("Running Two Hour Version")
           ptcs[which(sapply(lapply(ptcs,function(x) {x$rdpath}),is.na))]<-NULL
           NXU<-lapply(ptcs,bsrc.conredcap2)
         },
         "midnight"={
           message("Running Midnight Version")
           protocol.cur<-bsrc.switcher(name="bsocial",redcap_uri=input.uri,
                                       token=ptcs$bsocial$token,
                                       rdpath=rdpaths$bsocial)
           ptcs[which(sapply(lapply(ptcs,function(x) {x$rdpath}),is.na))]<-NULL
           
           bsrc.refresh(protocol = ptcs$bsocial)
           NXZ<-lapply(ptcs,function(protocol.cur){
             curdb<-bsrc.checkdatabase2(protocol.cur,forceupdate = T)
             bsrc.backup(curdb = curdb, protocol = protocol.cur)
           })
           
         },
         "sunday"={
           #DONOTHING YET; REFRESH FOR SCANDB UPCOMING...
         }
  )
  
  print("Finished auto_run")
  finish<-TRUE
  
}, error=function(x){print(paste(x,"NOT SUCCESSFUL"))})
## reset message sink and close the file connection


status<-"_UNKNOWN"
if (finish) {status<-"_SUCCESSFUL"}

if (whichtouse=="midnight"){
  clean<-TRUE
  expiration<-14
  xpath<-"~/Box/skinner/projects_analyses/Project BPD Longitudinal/bsrc log"
  lfile<-list.files(path=xpath,pattern="*.txt")
  yur<-as.numeric(Sys.Date()-as.Date(sapply(strsplit(lfile,split = "_"), "[[",1)))
  delfile<-lfile[which(yur>expiration)] 
  if (clean & length(delfile)>0) {
    delfile<-paste(xpath,delfile,sep="/")
    print("Removing old log files")
    file.remove(delfile)
  }
  output<-paste(xpath,"/",Sys.Date(),status,"_log.txt",sep = "")
  write(readLines("log.txt"),file = output)
}

close(zz)



