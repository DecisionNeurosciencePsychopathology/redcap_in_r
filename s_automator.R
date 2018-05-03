##########THIS IS THE ACTUAL AUTOMATOR SCRIPT:
setwd("/Users/jiazhouchen/Documents/UPMC/RStation")


source("Jiazhou.Startup.R")


###########
zz <- file("log.txt", open="wt")
sink(zz, type=c("output","message"))

finish<-FALSE

whichtouse<-as.character(Sys.getenv("whichtouse"))

tryCatch({jiazhou.startup(load = F)
protocol.cur<-bsrc.switcher(preset = "bsocial",protocol.cur = T)
switch(whichtouse,"three" = {
bsrc.conredcap2(protocol = protocol.cur)
}
,"midnight"={
curdb<-bsrc.checkdatabase2(protocol.cur,forceupdate = T)
bsrc.refresh(protocol = protocol.cur,curdb = curdb)
bsrc.backup(curdb = curdb, protocol = protocol.cur)
})

print("DONE")
finish<-TRUE

}, error=function(x){print(paste(x,"NOT SUCCESSFUL"))})
## reset message sink and close the file connection


status<-"_UNKNOWN"
if (finish) {status<-"_SUCCESSFUL"}

if (whichtouse=="midnight"){
clean<-TRUE
expiration<-14
xpath<-"/Users/jiazhouchen/Box Sync/skinner/projects_analyses/Project BPD Longitudinal/bsrc log"
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



