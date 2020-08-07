setwd("~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/")
source('~/Documents/github/UPMC/startup.R')
library(tidyverse);library(ggplot2)

PT <- bsrc.checkdatabase2(ptcs$protect,online = T)
BS <- bsrc.checkdatabase2(ptcs$bsocial,online = T)

PTEvtMap <- bsrc.idevtdatemap(ptcs$protect, online = T) %>%
  mutate(registration_redcapid = as.integer(registration_redcapid), variable = as.character(variable)) %>% 
  bsrc.findid(idmap,"registration_redcapid") %>%
  select(-registration_redcapid,-wpicid,-soloffid,-ogid,-ifexist)
BSEvtMap <- bsrc.idevtdatemap(ptcs$bsocial) %>% 
  mutate(registration_redcapid = as.integer(registration_redcapid), variable = as.character(variable)) %>% 
  bsrc.findid(idmap,"registration_redcapid") %>% 
  select(-registration_redcapid,-wpicid,-soloffid,-ogid,-ifexist)

# get the first consent date of every protect pts 
conDate<-md$data %>% select(registration_redcapid,paste0("reg_condate_",c("suicide","suicid2","protect","protect2","protect3"))) %>% 
  pivot_longer(-registration_redcapid,names_to = "ptc",names_prefix = "reg_condate_",values_to = "conDate") %>%
  filter(conDate!="") %>% 
  group_by(registration_redcapid) %>% slice(which.min(as.Date(conDate))) %>% ungroup() %>%
  transmute(masterdemoid = registration_redcapid, conDate = conDate)

# EXIT
EXIT<-read_csv("./form_dbo_A_EXIT_2020-06-23.csv") %>% select(-X1) %>% bsrc.findid(idmap,"registration_redcapid") %>% 
  select(masterdemoid,everything(),-registration_redcapid,-wpicid,-soloffid,-ogid,-ifexist)
any(duplicated(EXIT$IDDATE)) # FALSE: unique id+date
# MDRS
DRS<-read_csv("./form_A_MDRS_2020-06-23.csv") %>% select(-X1)%>% bsrc.findid(idmap,"registration_redcapid") %>% select(-registration_redcapid,-wpicid,-soloffid,-ogid,-ifexist)
any(duplicated(DRS$IDDATE)) # FALSE: unique id+date
# RBANS - baseline. map to the first baseline. must be later than the first baseline 
RBANS<-read_csv("./form_dbo_A_RBANS_2020-06-23.csv") %>% select(-X1)%>% bsrc.findid(idmap,"registration_redcapid") %>% select(-registration_redcapid,-wpicid,-soloffid,-ogid,-ifexist)
any(duplicated(RBANS$IDDATE)) # FALSE: unique id+date
# WTAR_PT
WTAR_PT<-read_csv("./form_WTAR_PT_2020-06-23.csv") %>% select(-X1)
any(duplicated(WTAR_PT$IDDATE)) # FALSE: unique id+date
# WTAR_BS
WTAR_BS<-read_csv("./form_WTAR_BS_2020-06-23.csv") %>% select(-X1)
any(duplicated(WTAR_BS$IDDATE)) # FALSE: unique id+date


# get the first bl of protect pts whose neuopysch data need transferring
baselines<-PTEvtMap %>% 
  filter(masterdemoid %in% c(RBANS$masterdemoid,WTAR_BS$masterdemoid,WTAR_PT$masterdemoid)) %>%
  left_join(conDate) %>% 
  # remove events earlier than the conDate
  filter(as.Date(date)>=as.Date(conDate)-283) %>% # a pt's bl can be 283 days earlier than the earliest consent date 
  #filter(!is.na(masterdemoid)) %>% 
  filter(!is.na(masterdemoid ) & !grepl("(ultimatum)|(trust)|(snake)|(learn)|(pie)",redcap_event_name)) %>% 
  group_by(masterdemoid) %>% filter(date == min(date,na.rm = T)) %>% ungroup() %>%
  #manually adjust 2 subjects' baselines 
  filter(masterdemoid != 220492)
baselines[which(baselines$masterdemoid==208554),"redcap_event_name"]<-"baseline_arm_1"
baselines[which(baselines$masterdemoid==208554),"conDate"]<-"2012-01-23"
  
unique(baselines$redcap_event_name)

any(duplicated(unique(baselines[c("masterdemoid","date")])$masterdemoid)) # false. One pt can have only one bl date
any(is.na(baselines$date)) # false 


# EXIT form under ptorocol Protect
  # map CDATE to redcap event   
EXIT_PTmapDate<-EXIT %>% select(masterdemoid,CDATE) %>%
  left_join(subset(PTEvtMap, redcap_event_name %in% subset(PT$eventmap,form=="exit")$unique_event_name)) %>% 
  mutate(daysDiff = as.Date(CDATE) - as.Date(date)) %>%
  group_by(masterdemoid,CDATE) %>% 
  # get the closest redcap event 
  mutate(absMin = min(abs(daysDiff),na.rm = T)) %>% #posMin = min(daysDiff[which(daysDiff>=0)]),valid = absMin ==posMin) %>% 
  filter(abs(daysDiff)==absMin) %>%
  left_join(conDate)
  #group_by(masterdemoid) %>% filter(absMin==min(absMin)) %>% ungroup() 

# check: the matched events allow form EXIT
which(!EXIT_PTmapDate$redcap_event_name %in% subset(PT$eventmap,form=="exit")$unique_event_name)

# check: no na in conDate
any(is.na(EXIT_PTmapDate$conDate)) # false

# allow -10 days to 30 days of differnece. can't be >7 days before first consent date   
newEXIT_PT0<-EXIT_PTmapDate %>% 
  filter(daysDiff>=-10 & daysDiff <=30 &
           as.Date(CDATE)-as.Date(conDate)>=-7)
  
#"Check after remapping ID event is unique "
newEXIT_PT0 %>% group_by(masterdemoid,redcap_event_name) %>% add_count(masterdemoid) %>% filter(n>1) %>% view()

newEXIT_PT<- newEXIT_PT0 %>%
  group_by(masterdemoid,redcap_event_name) %>% slice(which.min(absMin)) %>%
  left_join(EXIT) %>% 
  select(-variable,-date,-daysDiff,-absMin,-conDate)
# check: after remapping ID event is unique
newEXIT_PT %>% group_by(masterdemoid,redcap_event_name) %>% count(masterdemoid) %>% filter(n>1)
unique(newEXIT_PT$redcap_event_name)
# check: nothing is transferred into arm3
any(grepl("arm_3",newEXIT_PT$redcap_event_name)) # FALSE

# EXIT form under ptorocol Bsocial
"No exit for BS pts because I can't find dates of wtar_exit_score, the only event that allows form EXIT"

# aside 
"undone: aside for PT and BS "
aside_EXIT<-EXIT %>% anti_join(newEXIT_PT) %>% left_join(EXIT_PTmapDate) %>% filter(!is.na(date))  %>%view()


# MDRS form under ptorocol Protect
# map CDATE to redcap event   
DRS_PTmapDate<-DRS %>% select(masterdemoid,CDATE) %>%
  left_join(subset(PTEvtMap, redcap_event_name %in% subset(PT$eventmap,form=="drs")$unique_event_name)) %>% 
  mutate(daysDiff = as.Date(CDATE) - as.Date(date)) %>%
  group_by(masterdemoid,CDATE) %>% 
  # get the closest redcap event 
  mutate(absMin = min(abs(daysDiff),na.rm = T)) %>% #posMin = min(daysDiff[which(daysDiff>=0)]),valid = absMin ==posMin) %>% 
  filter(abs(daysDiff)==absMin) %>%
  left_join(conDate)
#group_by(masterdemoid) %>% filter(absMin==min(absMin)) %>% ungroup() 

# check: the matched events allow form DRS
which(!DRS_PTmapDate$redcap_event_name %in% subset(PT$eventmap,form=="exit")$unique_event_name)

# check: no na in conDate
any(is.na(DRS_PTmapDate$conDate)) # false

# allow -10 days to 30 days of differnece. can't be >7 days before first consent date   
newDRS_PT0<-DRS_PTmapDate %>% 
  filter(daysDiff>=-10 & daysDiff <=30 &
           as.Date(CDATE)-as.Date(conDate)>=-7)

#"Check after remapping ID event is unique "
newDRS_PT0 %>% group_by(masterdemoid,redcap_event_name) %>% add_count(masterdemoid) %>% filter(n>1) %>% view()

newDRS_PT<- newDRS_PT0 %>%
  group_by(masterdemoid,redcap_event_name) %>% slice(which.min(absMin)) %>%
  left_join(DRS)%>% 
  select(-variable,-date,-daysDiff,-absMin,-conDate)
# check: after remapping ID event is unique
newDRS_PT %>% group_by(masterdemoid,redcap_event_name) %>% count(masterdemoid) %>% filter(n>1)
unique(newDRS_PT$redcap_event_name)
# check: nothing is transferred into arm3
any(grepl("arm_3",newDRS_PT$redcap_event_name)) # FALSE

# check if the range of differnt days aer resonable 
barplot(table(DRS_PTmapDate$daysDiff[which(DRS_PTmapDate$daysDiff< -10)]))
barplot(table(DRS_PTmapDate$daysDiff[which(DRS_PTmapDate$daysDiff>30)]))


# aside 
"undone: aside for PT and BS "
aside_DRS<-DRS %>% anti_join(newDRS_PT) %>% left_join(DRS_PTmapDate) %>% filter(!is.na(date))  %>%view()



# RBANS form under ptorocol Protect
# everyone has a bldate. TRUE 
all(RBANS$masterdemoid %in% baselines$masterdemoid)

RBANS_PTmapDate<-RBANS %>% select(masterdemoid,CDATE) %>%
  #left_join(subset(PTEvtMap, redcap_event_name %in% subset(PT$eventmap,form=="rbansold_np")$unique_event_name)) %>% 
  left_join(baselines[c("masterdemoid","date")]) %>% left_join(conDate) %>% 
  filter(as.Date(CDATE)-as.Date(conDate)>= -10) %>%
  # only keep data of a cdate that is closest to the bldate 
  mutate(daysDiff = as.Date(CDATE)-as.Date(date)) %>% 
  group_by(masterdemoid) %>% slice(which.min(abs(daysDiff))) %>% ungroup() %>% 
  left_join(baselines)
#group_by(masterdemoid) %>% filter(absMin==min(absMin)) %>% ungroup() 

unique(RBANS_PTmapDate$redcap_event_name)
#"Check after remapping ID event is unique "
RBANS_PTmapDate %>% group_by(masterdemoid,redcap_event_name) %>% add_count(masterdemoid) %>% filter(n>1) 

newRBANS_PT<- RBANS_PTmapDate %>%
  group_by(masterdemoid,redcap_event_name) %>% 
  select(masterdemoid,CDATE,redcap_event_name) %>%
  left_join(RBANS)
# check: after remapping ID event is unique
newRBANS_PT %>% group_by(masterdemoid,redcap_event_name) %>% count(masterdemoid) %>% filter(n>1)


# RBANS form under ptorocol Bsocial

# aside 
"undone: aside for PT and BS "
aside_RBANS<-RBANS %>% anti_join(newRBANS_PT) %>% left_join(RBANS_PTmapDate) %>% filter(!is.na(date))  %>%view()



# WTAR_PT form under ptorocol Protect
# everyone has a bldate. TRUE 
which(!WTAR_PT$masterdemoid %in% baselines$masterdemoid) # row 223. this is okay 

WTAR_PTmapDate<-WTAR_PT %>% select(masterdemoid,CDATE) %>%
  #left_join(subset(PTEvtMap, redcap_event_name %in% subset(PT$eventmap,form=="rbansold_np")$unique_event_name)) %>% 
  left_join(baselines[c("masterdemoid","date")]) %>% left_join(conDate) %>% 
  filter(as.Date(CDATE)-as.Date(conDate)>= -10) %>%
  # only keep data of a cdate that is closest to the bldate 
  mutate(daysDiff = as.Date(CDATE)-as.Date(date)) %>% 
  group_by(masterdemoid) %>% slice(which.min(abs(daysDiff))) %>% ungroup() %>% 
  left_join(baselines)
#group_by(masterdemoid) %>% filter(absMin==min(absMin)) %>% ungroup() 

unique(WTAR_PTmapDate$redcap_event_name)
#"Check after remapping ID event is unique "
WTAR_PTmapDate %>% group_by(masterdemoid,redcap_event_name) %>% add_count(masterdemoid) %>% filter(n>1) 

newWTAR_PT<- WTAR_PTmapDate %>%
  group_by(masterdemoid,redcap_event_name) %>% 
  select(masterdemoid,CDATE,redcap_event_name) %>% 
  left_join(WTAR_PT)
# check: after remapping ID event is unique
newWTAR_PT %>% group_by(masterdemoid,redcap_event_name) %>% count(masterdemoid) %>% filter(n>1)


# WTAR_PT form under ptorocol Bsocial

# aside 
"undone: aside for PT and BS "
aside_WTAR_PT<-WTAR_PT %>% anti_join(newWTAR_PT) %>% left_join(WTAR_PTmapDate) %>% filter(!is.na(date))  %>%view()


# write csv
newEXIT_PT %>% rename(registration_redcapid = masterdemoid) %>% write.csv(paste0("./READY TO BE TRANSFERRED/EXIT_pt_",Sys.Date(),".csv"))
newDRS_PT %>% rename(registration_redcapid = masterdemoid) %>% write.csv(paste0("./READY TO BE TRANSFERRED/DRS_pt_",Sys.Date(),".csv"))
newRBANS_PT%>%rename(registration_redcapid = masterdemoid)  %>% write.csv(paste0("./READY TO BE TRANSFERRED/RBANS_pt_",Sys.Date(),".csv"))
newWTAR_PT %>% rename(registration_redcapid = masterdemoid) %>%write.csv(paste0("./READY TO BE TRANSFERRED/WTAR_pt_",Sys.Date(),".csv"))



BSbaselines <- BSEvtMap %>% 
  group_by(masterdemoid) %>% slice(which.min(as.Date(date))) %>% ungroup() %>% 
  filter(redcap_event_name == "baseline_arm_1")

newWTAR_BS<-WTAR_BS %>% left_join(BSbaselines) %>%
  select(registration_redcapid,wtar_date,wtar_s,wtar_s_adj,redcap_event_name)

newWTAR_BS %>%write.csv(paste0("./READY TO BE TRANSFERRED/WTAR_BS_",Sys.Date(),".csv"))


