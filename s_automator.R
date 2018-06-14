##########THIS IS THE ACTUAL AUTOMATOR SCRIPT:
setwd("/Users/jiazhouchen/Documents/UPMC/RStation")


source("Jiazhou.Startup.R")


###########
zz <- file("log.txt", open="wt")
sink(zz, type=c("output","message"))

finish<-FALSE

whichtouse<-as.character(Sys.getenv("whichtouse"))

tryCatch({jiazhou.startup(load = F)

switch(whichtouse,"three" = {
  protocol.b<-bsrc.switcher(name="bsocial",redcap_uri=input.uri,
               token='F4D36C656D822DF09832B5A4A8F323E6',
               rdpath=rdpaths$bsocial)
  protocol.k<-bsrc.switcher(name="ksocial",
               redcap_uri=input.uri,
               token='D542DAFD381E12682A7CFBB11286AC9B',
               rdpath=rdpaths$ksocial)
  protocol.s<-bsrc.switcher(name="scandb",
              redcap_uri=input.uri,
              token='0386803904E8E249F8D9500859528327',
              rdpath=rdpaths$scandb)
}
,"midnight"={
protocol.cur<-bsrc.switcher(name="bsocial",redcap_uri=input.uri,
                            token='F4D36C656D822DF09832B5A4A8F323E6',
                            rdpath=rdpaths$bsocial)
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



