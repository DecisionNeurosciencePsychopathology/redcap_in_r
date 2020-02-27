# setup 
setwd("~/Documents/github/UPMC/data meeting/")
source("~/Documents/github/UPMC/startup.R")

protocol.cur <- ptcs$masterdemo
md <- bsrc.checkdatabase2(online = T)
#plotpath="/Users/mogoverde/Desktop" #Morgan WFH
plotpath="C:/Users/buerkem/OneDrive - UPMC/Desktop" #Morgan Work
#plotpath="~/Documents/github/UPMC/data meeting" #Kexin

#get id 
MD<-bsrc.getform(formname = 'record_registration',curdb = md,protocol = ptcs$masterdemo)
#ptcname: ksocial, bsocial,protect3,protect2,protect,suicid2,suicide,EMA,EXPLORE
#Grabs the variable names based on protocol
varnames<-function(ptcname){
  id<-c('registration_redcapid', 'registration_wpicid', 'registration_demostatus','registration_group','registration_dob','registration_gender',
        paste0(c('registration_ptcstat___','reg_condate_','reg_term_yesno_','reg_term_reason_','reg_term_excl_'),ptcname),paste0("registration_race___",c(1:5,999)),"registration_edu")
  if(ptcname=='protect3'){return(c(id,'reg_status_protect3','reg_p3catchup'))
  }else if(ptcname=='protect2'){return(c(id,'reg_status_protect2'))
  }else if(ptcname=='protect'){return(c(id,'reg_status_protect'))
  }else if(ptcname=='suicid2'){return(c(id,'reg_status_suicid2'))
  }else if(ptcname=='suicide'){return(c(id,'reg_status_suicide'))
  }else if(ptcname=='ksocial'){return(c(id,'registration_ptcstat___bsocial'))
  }else if (ptcname=='bsocial'){return(c(id, 'registration_lethality'))}
  else{return(id)}
}


#PROTECT - ALL
var_pt<-unique(c(varnames('protect3'),varnames('protect2'),varnames('protect'),varnames('suicide'),varnames('suicid2')))
varnames("bsocial")->var_bsoc

#Checks1
all(var_pt %in% colnames(MD)) #should be TRUE
all(var_bsoc %in% colnames(MD)) #bsocial and ema

#Add master demo info
PT<-MD[,var_pt]
BS<-MD[,var_bsoc]


##More checks of each protocol
#Protect checks
PT<-PT[unique(which(PT[,grep('registration_ptcstat_',var_pt)]==1,arr.ind = T)[,1]),] # consented to at least one protocol 
allPT<-MD[,var_pt] #not removed ineligible people 
#temp<-PT[unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # ieligible people 
PT<-PT[-unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # remove all people terminated for at least one protocol due to ineligibility  
PT<-PT[-unique(which(PT[,grep('reg_term_excl_',var_pt)]==1,arr.ind = T)[,1]),] # remove all unusable data based on 'reg_term_excl_

#BSOCIAL checks
BS[which(BS$registration_ptcstat___bsocial==1 & !is.na(BS$registration_ptcstat___bsocial)),]->BS
BS[which(BS$reg_term_reason_bsocial!=3 | is.na(BS$reg_term_reason_bsocial)),]->BS
BS[which(BS$reg_term_excl_bsocial!=1 | is.na(BS$reg_term_excl_bsocial)),]->BS
idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
  names(idmap)<-c("masterdemoid","wpicid","soloffid")
BS<-bsrc.findid(BS,idmap = idmap,id.var = "registration_redcapid")


#####check cleaning##########
##Protect
sum(duplicated(PT$registration_redcapid)) #should be 0. check ID duplicates 
sum(PT$registration_group==89) #should be 0. No 89 in 'group'
sum(is.na(PT$registration_redcapid)) # should be 0. no NA in ID 
sum(is.na(PT$registration_group)) # should be 0. no NA in 'group'
sum(is.na(PT$registration_dob))# should be 0. no NA in 'dob'
#if all records are usable -- 'reg_term_excl_' should all be0 
sum(PT$reg_term_excl_protect3==1,na.rm = T)
sum(PT$reg_term_excl_protect2==1,na.rm = T)
sum(PT$reg_term_excl_protect==1,na.rm = T)
sum(PT$reg_term_excl_suicid2==1,na.rm = T)
sum(PT$reg_term_excl_suicide==1,na.rm = T)
#View(PT[which(PT$reg_term_excl_protect3==1 | PT$reg_term_excl_protect2==1 | PT$reg_term_excl_protect==1 | PT$reg_term_excl_suicid2==1 | PT$reg_term_excl_suicide==1),]) #18 people exluded from analysis 

##Bsocial
sum(duplicated(BS$registration_redcapid))
sum(BS$reg_term_excl_bsocial==1,na.rm = T)
sum(is.na(BS$registration_redcapid)) 
#IDs of the participants with no group status
sum(is.na(BS$registration_group)) # =163
BS[which(is.na(BS$registration_group)),"registration_redcapid"] 
#Remove ineligible
sum(BS$registration_group==89 & !is.na(BS$registration_group==89)) # =23
BS[-which(BS$registration_group==89 & !is.na(BS$registration_group==89)),]->BS
#Missing DOB
sum(is.na(BS$registration_dob)) 


root="~/Box/skinner/data/" #Kexin
#root="C:/Users/buerkem/Box/skinner/data/" #Morgan
list.files(path=paste0(root, "eprime/shark"))->Eshark
list.files(path=paste0(root, "eprime/clock_reversal"))->Eclock
#Remove the non-ID files (only a problem with clock)
as.numeric(Eclock)[-which(is.na(as.numeric(Eclock)))]->Eclock
as.numeric(Eshark)->Eshark
#ID mapping
idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
names(idmap)<-c("masterdemoid","wpicid","soloffid")
#Get list of shark OR clock IDs
unique(append(Eshark, Eclock))->Eallid
#Make df for ID matching
data.frame(ID=Eallid, extra=T)->Eallid
#Grab new ids as masterdemoids
bsrc.findid(Eallid,idmap = idmap,id.var = "ID")$masterdemoid->Eallmd
#Check
#Should be =0, all IDs should be in Pall
#Get EXPLORE pts from those in Protect
#Get people who have scanned from Box
list.files(path=paste0(root, "matlab task data/bpd_clock"))->Bclock
list.files(path=paste0(root, "matlab task data/bpd_spott"))->Bspott
list.files(path=paste0(root, "eprime/bpd_trust"))->Btrust
#Anyone who was scanned
unique(append(Bclock, Btrust))->BKtasks
unique(append(BKtasks,Bspott))->Bmriid
as.numeric(Bmriid)->Bmriid
as.numeric(Bspott)->Bspott
#Make df for ID matching (SPOTT seperately)
data.frame(ID=Bmriid, extra=T)->Bmriid
data.frame(ID=Bspott, extra=T)->Bspottid
#ID match
bsrc.findid(Bmriid,idmap = idmap,id.var = "ID")->Bmrimddf
#Bmrimddf[-which(Bmrimddf$ID=="120517"),]
Bmrimddf[which(!Bmrimddf$ifexist),] #Should be no one
bsrc.findid(Bspottid,idmap = idmap,id.var = "ID")->Bspottmddf
Bspottmddf[which(!Bspottmddf$ifexist),] #Should be no one
#Get people in EMA
bsrc.attachngrab(rdpaths$ema)->ema
ema$fulldata.ema$info->emainfo
emainfo$RedcapID<-as.character(emainfo$RedcapID)
emainfo<-bsrc.findid(emainfo,idmap=idmap,id.var = "RedcapID")
emainfo$RedcapID
table(emainfo$Status)

#Plot prep for KSOCIAL
#Get which participants completed each task (K Only)
list.files(path=paste0(root, "matlab task data/ksoc_clock"))->Kclock
list.files(path=paste0(root, "eprime/ksoc_trust"))->Ktrust
#B/Ksocial pts (gathered from BSOCIAL section)  
as.numeric(BKtasks)->BKtasks
#Make df for ID matching
data.frame(ID=BKtasks, extra=T)->BKtasksid
bsrc.findid(BKtasksid,idmap = idmap,id.var = "ID")->BKmddf
BKmddf[which(!BKmddf$ifexist),] #should be none
BKmddf$masterdemoid->BKid
unique(append(append(Kclock,Ktrust),BKid))->Kmri
#age>25

# the IDs
Eallmd #explore (pt)
Bspottmddf$masterdemoid #spott (bs)
Bmrimddf$masterdemoid #bmri (bs)
emainfo$masterdemoid #ema (bs)
#Kmri #kmri (k)

explore<-as_tibble(PT) %>%
  filter(registration_redcapid%in%Eallmd) %>%
  mutate(Group=plyr::mapvalues(registration_group,from = c("ATT","IDE","DEP","88","HC"), to = c("3ATT","2DNA","2DNA","2DNA","1HC"))) 
  
bmri<-as_tibble(BS) %>%
  filter(masterdemoid%in%Bmrimddf$masterdemoid) %>%
  mutate(Group=plyr::mapvalues(registration_group,from = c("ATT","NON","HC"), to = c("3ATT","2NON","1HC"))) %>%
  mutate(Group=replace(Group,registration_lethality=="hl","4ATT-HL")) %>%
  mutate(Group=replace(Group,registration_lethality=="ll","3ATT-LL")) 
bmri<-filter(bmri,registration_redcapid!=440132) # special case: 440132 is removed from the data because he's a bs att but does not have lethality status. Emailed Nate about this. 

emaa<-as_tibble(BS) %>%
  filter(masterdemoid%in%emainfo$masterdemoid) %>%          
  mutate(Group=plyr::mapvalues(registration_group,from = c("ATT","NON","HC"), to = c("3ATT","2NON","1HC"))) 
  

########################################
explore<-subset(PT,registration_redcapid%in%Eallmd)
explore$Group<-plyr::mapvalues(explore$registration_group,from = c("ATT","IDE","DEP","88","HC"), to = c("3ATT","IDE","DEP","Unsure","1HC"))
explore$Group[which(explore$registration_group %in% c("IDE","DEP","88"))]<-"2DNA"
#bspt<-subset(BS,registration_redcapid%in%Bspottmddf$masterdemoid)
bmri<-subset(BS,masterdemoid%in%Bmrimddf$masterdemoid) #143->136
bmri$Group<-plyr::mapvalues(bmri$registration_group,from = c("ATT","NON","HC"), to = c("3ATT","2NON","1HC"))
bmri$Group[which(bmri$registration_lethality=="hl")]<-"4ATT-HL"
bmri$Group[which(bmri$registration_lethality=="ll")]<-"3ATT-LL"
emaa<-subset(BS,masterdemoid%in%emainfo$masterdemoid) #193->192 440057 removed due to ineligibility
emaa$Group<-plyr::mapvalues(emaa$registration_group,from = c("ATT","NON","HC"), to = c("3ATT","2NON","1HC"))
#########################################
#kmri<-subset(K,registration_redcapid%in%Kmri) #152->60
setdiff(Bmrimddf$masterdemoid,bmri$masterdemoid) # id in folders but not consented to BS or ineligible for BS 
setdiff(Bmrimddf$masterdemoid,bmri$masterdemoid)%in%PT$registration_redcapid 
print("219475 no info; 220403 ineligible; 220691,220831 hc; 220976 220989 possibly did the wrong test - they posibly should have done explore clock")
#################
scandb<-bsrc.checkdatabase2(protocol = ptcs$scandb)
scanbs<-bsrc.getform(protocol = ptcs$scandb,formname = "bsocial",curdb = scandb)
scanbs<-bsrc.findid(scanbs,idmap = idmap,id.var = "registration_redcapid")

View(subset(scanbs,masterdemoid%in%c(220976,220989,221668)))

setdiff(Bmrimddf$masterdemoid,scanbs$masterdemoid) # id in folders but not in scandb: 220756 220976 220989 221548 221603 221668
setdiff(scanbs$masterdemoid,Bmrimddf$masterdemoid) # id in scandb but not in folders: 221256 221409     NA 440053


View(subset(BS,registration_redcapid%in%setdiff(Bmrimddf$masterdemoid,bmri$registration_redcapid)))

setdiff(emainfo$masterdemoid,emaa$registration_redcapid) 
###################
#check duplicates
any(duplicated(bmri$masterdemoid))
any(duplicated(emaa$masterdemoid))
any(duplicated(explore$registration_redcapid))
#prepare to get counts 
agemap<-data.frame(yrlable=c("18-25","26-30","31-40","41-50","51-60","61-70","70+"),yrstar=c(0,26,31,41,51,61,71))
#edumap<-data.frame(edulabel=c("1Below High School or GED: <12","2High School or GED: 12", "3RN Certificate: 13","4Associates / Technical: 14","5Bachelors: 16","6Masters: 18-19","7MD/PhD/MD-PhD: >19"),yrstar=c(0,12,13,14,16,18,20))
edumap<-data.frame(edulabel=c("1Below Bachelors","2Bachelors or Above: 16"),yrstar=c(0,16))
category<-c("registration_gender","Age","Age_Label","registration_edu","Education_Reference")
races<-c("1American Indian or Alaska Native","2Asian","3Black or African American","4Native Hawaiian or Other Pacific Islander","5White")

#get counts by group, gender, age, race, edu, race
for (dfname in c("explore","bmri","emaa")){
  df<-get(dfname)
  library(lubridate)
  df$Age<-as.integer((Sys.Date()-as.Date(df$registration_dob))/365)
  df$Age_Label<-as.character(agemap$yrlable[findInterval(df$Age,agemap$yrstar)])
  print(unique(df$registration_edu))
  df$Education_Reference<-as.character(edumap$edulabel[findInterval(df$registration_edu,edumap$yrstar)])
  
  library(tidyr)
  result<-data.frame(matrix(c(NA,as.vector(table(df$Group,useNA = "always")),NA),nrow = 1))
  colnames(result)<-c("attributes",sort(unique(df$Group)),"NA","Total")
  result$Total[1]<-sum(result[1,],na.rm = T)
  for (cate in category){
    newtable<-as.data.frame(table(df[c(cate,"Group")],useNA="always"))%>%
      pivot_wider(id_cols = cate,names_from = "Group",values_from = "Freq")
    newtable$Total<-rowSums(newtable[-1],na.rm = T)
    subtotal<-c("Total",as.vector(colSums(newtable[-1],na.rm = T)))
    newtable<-rbind(colnames(newtable)[1],newtable)
    colnames(newtable)<-c("attributes",colnames(newtable)[-1])
    newtable<-as.data.frame(newtable);newtable[1]<-as.character(newtable[[1]])
    result<-rbind(result,newtable,subtotal)
  }
  result<-rbind(result,c(NA,rep("Race",length(result)-1)))
  for (racei in 1:5){
    cate2<-paste0("registration_race___",racei)
    racetable<-table(df[c(cate2,"Group")],useNA = "always")
    racecounts<-racetable[which(rownames(racetable)=="1"),]
    result<-rbind(result,c(races[racei],racecounts,sum(racecounts,na.rm = T)))
  }
  # for race 999
  racetable<-table(df[c("registration_race___999","Group")],useNA = "always")
  racecounts<-racetable[which(rownames(racetable)=="1"),]
  result<-rbind(result,c("6No answer",racecounts,sum(racecounts,na.rm = T)))
  
  result[which(is.na(result),arr.ind = T)]<-""
  assign(paste0("stats_",dfname),result)
  write.csv(result,paste0("~/Documents/github/UPMC/data meeting/Counts_Scanning_for_Mandy_",dfname,"_",Sys.Date(),"_",".csv"))
}

#get mean and sd of age 
for (dfname in c("explore","bmri","emaa")){
  df<-get(dfname)
  library(lubridate)
  df$Age<-as.integer((Sys.Date()-as.Date(df$registration_dob))/365)
  print(dfname)
  print(class(df$Age))
  print(df%>% group_by(Group) %>% summarise(mean=mean(Age,na.rm = T),sd=(sd(Age,na.rm = T))))
  print(paste("age mean",mean(df$Age)))
  print(paste("age sd",sd(df$Age)))}

####2/27/2020 additions to determine if group differences on gender
bsgrp<-md$data[which(md$data$registration_redcapid %in% Bmrimddf$masterdemoid),c("registration_redcapid","registration_group",
                                                              "registration_lethality","registration_gender","registration_ptcstat___bsocial")]
if(any(bsgrp$registration_ptcstat___bsocial!=1)){
  message("These people aren't in BSOCIAL but have BSOCIAL scanning data plz fix")
  message(list(bsgrp[which(bsgrp$registration_ptcstat___bsocial!=1),"registration_redcapid"]))
  bsgrp[-which(bsgrp$registration_ptcstat___bsocial!=1),]->bsgrp}

if(any(bsgrp$registration_lethality=="" & bsgrp$registration_group=="ATT")){
  message("Attempter w/o lethality, please fix!")
  message(list(bsgrp[which(bsgrp$registration_lethality=="" & bsgrp$registration_group=="ATT"),"registration_redcapid"]))}


if(any(bsgrp$registration_group=="89")){
  message("Group listed as 89")
  message(list(bsgrp[which(bsgrp$registration_group=="89"),"registration_redcapid"]))
  bsgrp[-which(bsgrp$registration_group=="89"),]->bsgrp
}

library(dplyr)
bsgrp %>% mutate(group=ifelse(registration_group=="ATT",toupper(registration_lethality), registration_group))->bsgrp
bsgrp[c(4,6)]->genderxgroup
library(tableone)
c2<-
  compareGroups::compareGroups(
    genderxgroup,
    y=genderxgroup$group, 
    include.miss=F
  )
t2<-compareGroups::createTable(
    c2,
    hide.no = 0,
    digits = 0,
    show.n = TRUE,
    show.p.mul = TRUE)


compareGroups::export2html(t2, "bsocialbygrp.html")
      
