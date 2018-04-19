##########THIS IS THE ACTUAL AUTOMATOR SCRIPT:
setwd("/Users/jiazhouchen/Documents/UPMC/RStation")

source("REDREW.R")
source("AUTOMATOR.R")
source("ECOLOGIST.R")
source("ADMINISTRATOR.R")
source("Jiazhou.Startup.R")


###########
zz <- file("log.txt", open="wt")
sink(zz, type=c("output","message"))

finish<-FALSE

tryCatch({jiazhou.startup(load = T)

bsrc.refresh(forcerun.e = T,forceupdate.e = F)

bsrc.backup()

print("DONE")

finish<-TRUE

}, error=function(x){print(paste(x,"NOT SUCCESSFUL"))})
## reset message sink and close the file connection
sink(type="message")
close(zz)

status<-"unknown"
if (finish) {status<-"_SUCCESSFUL"}

output<-paste(getwd(),"/TRACK/",Sys.Date(),status,"_log.txt",sep = "")
write(readLines("log.txt"),file = output)

clean<-TRUE
expiration<-14
xpath<-paste(getwd(),"/TRACK",sep = "")
lfile<-list.files(path=xpath,pattern="*.txt")
yur<-as.numeric(Sys.Date()-as.Date(as.character(as.data.table(strsplit(lfile,split = "_"))[1])))
delfile<-lfile[which(yur>expiration)] 
delfile<-paste(xpath,delfile,sep="/")
if (clean & length(delfile)>0) {
  print("Removing old log files")
  file.remove(delfile)
}

save.image("~/Documents/UPMC/RStation/curren.RData")