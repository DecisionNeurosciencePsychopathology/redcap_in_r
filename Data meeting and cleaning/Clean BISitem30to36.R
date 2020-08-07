setwd("~/Documents/github/UPMC/TRANSFER/PT")
source('~/Documents/github/UPMC/startup.R')
############startuptransfer#############
BIS36<-read.csv("~/Box/skinner/data/Redcap Transfer/All protect data/A_BIS_item31to36.csv")%>% bsrc.findid(idmap) %>% 
  mutate(CDATE = lubridate::mdy(CDATE)) %>% 
  filter(ifexist) %>%
  select(masterdemoid,CDATE:Q36,-MISSCODE)

names(BIS36)<-c("registration_redcapid","CDATE",paste0("bis36_",c(31:36)))

which(duplicated(BIS36$registration_redcapid))
BIS36<-BIS36[-59,] # remove 219658 
any(duplicated(BIS36$registration_redcapid))

BIS36 %>% write_csv(paste0("BISitem31to36_",Sys.Date(),".csv"))
