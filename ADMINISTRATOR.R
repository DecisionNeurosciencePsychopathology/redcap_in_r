###---
Title: "Administrator"
Author: "Jiazhou Chen"
Version: 0.5
###---
#Version 0.5:
  #bsrc.admin.rppr() calculates number for rppr report.
  #bsrc.emaonly() for ema number update
  #bsrc.reg.group() to map group name to text

#Version 0.4 Changelog:
  #bsrc.admin.biweekly() now produce also follow-ups from current and next month

#Version 0.3 Changelog:
  #Refined function bsrc.admin.biweekly()

#Version 0.2 Changelog:
  #New function: bsrc.admin.biweekly()
  #For B-Social biweekly meetings

####
#To do list:
# Functionalize the graphing function since it's almost identical in most process
# Functionalize Follow-up histagram
# Break down by group; use interaction() better
# NEW, EMA, MRI
library("ggplot2")

#####
save.image("~/Documents/UPMC/RStation/admin.RData")

###########################Bi-Weekly Meeting Sheet:
bsrc.admin.biweekly<-function(days=14,monthz=2){
  
  #Find Max Follow-Up Dates: 
  funbsrc$fudemo_visitdate[which(funbsrc$fudemo_visitdate=="")]<-NA
  maxfudate<-aggregate(na.exclude(as.Date(funbsrc$fudemo_visitdate)),by=list(funbsrc$registration_redcapid[!is.na(funbsrc$fudemo_visitdate)]),max)
  names(maxfudate)<-c("registration_redcapid","Follow-up")
  
  #Get progress:
  futurefolks<-subreg[,c("registration_redcapid","registration_consentmonth","prog_diff","prog_lastfollow","prog_endor_y")]

  
  #Get EMA Dates:
  emapg<-bsrc.getform(formname = "ema_progress_check")
  emapgonly<-subset(emapg,select = c("registration_redcapid","emapg_date_7days","emapg_date_14days","emapg_date_21days"))
  emapgonly$ema_maxdate<-apply(emapgonly[-grep("registration_redcapid",names(emapgonly))],1,max,na.rm=T)
  emapgonly.a<-subset(emapgonly,select = c("registration_redcapid","ema_maxdate"))
  names(emapgonly.a)<-c("registration_redcapid","EMA")
  
  #Get MRI Dates:
  mripgonly<-bsrc.getform(formname = c("fmri_screening_form","fmri_session_checklist"))
  mripgonly.a<-subset(mripgonly,select = c("registration_redcapid","mricheck_scheudleddate","mricheck_scanneddate")) 
  mripgonly.b<-mripgonly.a[which(!is.na(mripgonly.a$mricheck_scheudleddate) | !is.na(mripgonly.a$mricheck_scanneddate)),]
  mripgonly.b$MRI<-apply(mripgonly.b[-grep("registration_redcapid",names(mripgonly.b))],1,max,na.rm=T)
  mripgonly.c<-subset(mripgonly.b, select = c("registration_redcapid","MRI"))

  #Get Baseline:
  baseline<-na.omit(subset(bsrc.getform(formname = "bldemo"),select = c('registration_redcapid',"demo_visitdate")))
  baseline$demo_visitdate<-as.Date(baseline$demo_visitdate)
  names(baseline)<-c("registration_redcapid","Baseline")
  
  #Get Consented Dates:
  consented<-subset(subreg,select = c("registration_redcapid","registration_consentdate"))
  consented$registration_consentdate[consented$registration_consentdate==""]<-NA
  consented<-na.omit(consented)
  consented$registration_consentdate<-as.Date(consented$registration_consentdate)
  names(consented)<-c("registration_redcapid","Consented")
  
  #Add additional component here
  #Merged:
  merged.a<-merge(merge(emapgonly.a,maxfudate,all=T),merge(baseline,consented,all=T),all=T)
  merged<-merge(merged.a,mripgonly.c,all=T)
  merged$`Event`<-colnames(merged[-grep("registration_redcapid",names(merged))])[apply(merged[-grep("registration_redcapid",names(merged))],1,function(x) {which(x==max(x,na.rm=T))}[1])]
  merged$`Event Date`<-apply(merged[-grep("registration_redcapid|Event",names(merged))],1,max,na.rm=T)
  merged.simp<-merged
  merged.simp<-subset(merged,select = c("registration_redcapid","Event","Event Date"))
  #Add Status
  merged.simp$`MRI Status`<-subreg$prog_fmristatus[which(subreg$registration_redcapid %in% merged.simp$registration_redcapid)]
  merged.simp$`EMA Status`<-subreg$prog_emastatus[which(subreg$registration_redcapid %in% merged.simp$registration_redcapid)]
  #Add Initials & Age:
  merged.simp$`Age`<-subreg$prog_cage[which(subreg$registration_redcapid %in% merged.simp$registration_redcapid)]
  merged.simp$`Initials`<-subreg$registration_initials[which(subreg$registration_redcapid %in% merged.simp$registration_redcapid)]
  merged.simp$`Group`<-subreg$registration_group[which(subreg$registration_redcapid %in% merged.simp$registration_redcapid)]
  merged.simp$`Latest IPDE Date`<-subreg$prog_latestipdedate[which(subreg$registration_redcapid %in% merged.simp$registration_redcapid)]
  
  #Refine Status: 
  ord<-c("Consented","Baseline","Follow-up","MRI","EMA")
  merged.simp<-merged.simp[order(match(merged.simp$`Event`,ord),merged.simp$`Event Date`),]
  merged.simp$Group<-mapvalues(merged.simp$Group,from = c("1","2","3","4","88"), to=c("HC","LL","HL","NON-ATT","UNCLEAR"))
  colnames(merged.simp)[grep("registration_redcapid",names(merged.simp))]<-"RedCap ID"
  merged.simp$Event[merged.simp$Event=="Follow-up"]<-paste(subreg$prog_lastfollow[match(merged.simp[merged.simp$Event=="Follow-up",]$`RedCap ID`,subreg$registration_redcapid)],"Years Follow-up")

  #FU Month:
  merged.simp$`Follow-up Month`<-month.name[futurefolks$registration_consentmonth[match(merged.simp$`RedCap ID`,futurefolks$registration_redcapid)]]
  merged.simp<-merged.simp[,c("RedCap ID","Initials","Age","Group","Follow-up Month","Event","Event Date","Latest IPDE Date","MRI Status","EMA Status")]

  
  #########Future Folks
  tarmon<-c(month(Sys.Date()),month(Sys.Date())+1)
  
  which(subreg$prog_diff>0 & subreg$prog_diff< monthz+0.1)
  
  futureid<-futurefolks[which(futurefolks$registration_consentmonth %in% tarmon & subreg$prog_diff>0 & subreg$prog_diff< monthz+0.1),]$registration_redcapid
  future<-merged.simp[which(merged.simp$`RedCap ID` %in% futureid),]
  #future$`Follow-up Month`<-month.name[futurefolks$registration_consentmonth[match(future$`RedCap ID`,futurefolks$registration_redcapid)]]
  future<-future[order(match(future$`Follow-up Month`,month.name)),]
  future<-future[,c("RedCap ID","Initials","Age","Group","Follow-up Month","Event","Event Date","Latest IPDE Date","MRI Status","EMA Status")]
  names(future)<-c("RedCap ID","Initials","Age","Group","Follow-up Month","Last Event","Last Event Date","Latest IPDE Date","MRI Status","EMA Status")
  rownames(future)<-NULL
  
  ########Current Folks
  merged.recent<-merged.simp[which(merged.simp$`Event Date` >= Sys.Date()-days),]
  rownames(merged.recent)<-NULL

  
  
  return(list(Past_Two_Weeks=merged.recent,Next_Two_Month = future))
   
}
###########################RPPR Report:
bsrc.admin.rppr<-function(){

newconsent<-subreg[which(as.Date(subreg$registration_consentdate)>startdate & subreg$registration_status!=88),]
totaln<-length(newconsent$registration_redcapid)
}

#################
bsrc.emaonly<-function(x) {
  if (missing(x)){x<-funbsrc}
  else (x->funbsrc)
  
  funbsrc$ema_setuptime[which(funbsrc$ema_setuptime=="")]<-NA
  emaonly.f<-bsrc.getform(formname = "ema_session_checklist", forcerun.e = T)
  emaonly.f<-emaonly.f[which(!is.na(emaonly.f$ema_setuptime)),]
  emaonly.s<-subset(emaonly,select = c("registration_redcapid","redcap_event_name","ema_setuptime","ema_completed___2"))
  emaonly.s$ema_setuptime<-as.Date(emaonly.s$ema_setuptime)
  emaonly.s$prog_emadued<-emaonly.s$ema_setuptime+3
  emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptimeema.+22]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+22]+22
  emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptime+15]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+15]+15
  emaonly.s$prog_emadued[Sys.Date() <= emaonly.s$ema_setuptime+8]<-emaonly.s$ema_setuptime[Sys.Date() <= emaonly.s$ema_setuptime+8]+8
  emaonly.s$prog_emadued[Sys.Date() >= emaonly.s$ema_setuptime+21]<-emaonly.s$ema_setuptime[Sys.Date() >= emaonly.s$ema_setuptime+21]+21
  
  emaonly.s$prog_emastatus<-paste("IP3d:",emaonly.s$ema_setuptime+3)
  emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+22)]<-paste("IP21d:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+22)]+22)
  emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+15)]<-paste("IP14d:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+15)]+15)
  emaonly.s$prog_emastatus[which(Sys.Date() <= emaonly.s$ema_setuptime+8)]<-paste("IP7d:",emaonly.s$ema_setuptime[which(Sys.Date() <= emaonly.s$ema_setuptime+8)]+8)
  emaonly.s$prog_emastatus[which(Sys.Date() >= emaonly.s$ema_setuptime+21)]<-paste("DONE:",emaonly.s$ema_setuptime[which(Sys.Date() >= emaonly.s$ema_setuptime+21)]+21)
  emaonly.s$prog_emastatus[which(emaonly.s$ema_completed___2==1)]<-paste("Completed:",emaonly.s$ema_setuptime[which(emaonly.s$ema_completed___2==1)]+21)
  emaonly.s$prog_emastatus_di<-emaonly.s$prog_emastatus
  emaonly.s$prog_emastatus_di[emaonly.s$ema_completed___2==1]<-NA
  emaonly.x<-subset(emaonly.s,select = c("registration_redcapid","prog_emastatus","prog_emastatus_di","prog_emadued"))
  #In Progress: 
  emaonly.j<-bsrc.getform(formname = "ema_screening_form", forcerun.e = T)
  emaonly.j<-subset(emaonly.j[!(emaonly.j$registration_redcapid %in% emaonly.x$registration_redcapid) & emaonly.j$ema_yesno==1,],select = c("registration_redcapid"))
  emaonly.j$prog_emastatus<-"Screened&Ready"
  #Merge:
  emaonly.r<-merge(emaonly.x,emaonly.j,all=T)
  
  return(emaonly.r)
}

bsrc.reg.group<-function(x){
x$registration_group_txt<-mapvalues(x$registration_group, from = c(1:4,88), 
          to = c("HC","LL ATT","HL ATT","Non-ATT BPD","NOTSURE"),warn_missing = F)
}


###########################Data Meeting:
bsrc.datameeting<-function(protocol="bsocial"){
  if(missing(protocol)) {
    protocol=readline(prompt = "Please input the protocl: ")}
  switch (protocol,
          bsocial = {enddate<-as.Date("2020-08-01") 
          startdate<-as.Date("2017-07-30")},
          ksocial = {enddate<-as.Date("2021-08-01") 
          startdate<-as.Date("2017-08-01")
          })
  
  totaln<-200
  
  if(protcol=="bsocial") {
    #New Consent since Auguest 2017
    newconsent<-as.data.frame(funbsrc$registration_consentdate[which(as.Date(funbsrc$registration_consentdate)>startdate)])
    names(newconsent)<-c("date")
    newconsent$date<-as.Date(sort(newconsent$date))
    newconsent$Actual<-1:length(newconsent$date)
    totaln.new<-100 #Different
    
    #IDENTICAL
    seqdate<-seq.Date(from=startdate,to=enddate,by="days")
    totalseq<-seq(from=0,to=totaln.new,length.out = length(seqdate))
    total<-as.data.frame(seqdate)
    total$accu<-totalseq
    
    new.start<-newconsent$date[1]
    new.end<-newconsent$date[length(newconsent$date)]
    new.plan<-total[which(total$seqdate %in% new.start):which(total$seqdate %in% new.end),]
    names(new.plan)<-c("date","Projection")
    merged.new<-merge(newconsent,new.plan,by=1,all = T)
    merged.new.melt <- na.omit(melt(merged.new, id.var='date'))
    names(merged.new.melt)<-c("date","Number Type","count")
    merged.new.melt$label<-merged.new.melt$count
    merged.new.melt$label[merged.new.melt$`Number Type`!="Actual"]
    
    new.plot<- ggplot(merged.new.melt, aes(x=date, y=count, label=count, color=`Number Type`)) +
      ggtitle(paste("New BPD Participant, current total:", length(newconsent$date)))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line() +
      geom_point(data = merged.new.melt[which(merged.new.melt$`Number Type` == "Actual"),])+
      scale_color_manual(values=c('red', 'gray')) +
      xlab(paste("Current Difference: ",round(merged.new$Actual[length(merged.new$date)]-merged.new$Projection[length(merged.new$date)]))) + 
      ylab("Number of Participants") +
      scale_x_date(date_labels = "%B", date_breaks = "1 month") +
      geom_label(data = merged.new.melt[which(merged.new.melt$`Number Type` == "Actual"),], aes(label=count))+
      geom_label(data = merged.new.melt[max(which(merged.new.melt$`Number Type` == "Projection")),], aes(label=round(count)))
    
    ###################
    #EMA
    funbsrc$ema_setuptime[funbsrc$ema_setuptime==""]<-NA
    emaconsent<-as.data.frame(funbsrc$ema_setuptime[which(!is.na(funbsrc$ema_setuptime))])
    names(emaconsent)<-c("date")
    emaconsent$date<-as.Date(sort(emaconsent$date))
    emaconsent$Actual<-1:length(emaconsent$date)
    emaconsent$ip<-funbsrc$ema_completed___ip[which(!is.na(funbsrc$ema_setuptime))]
    emaconsent$v2<-funbsrc$ema_completed___2[which(!is.na(funbsrc$ema_setuptime))]
    emaconsent$noappli<-funbsrc$ema_completed___999[which(!is.na(funbsrc$ema_setuptime))]
    emaconsent.f<-emaconsent
    emaconsent<-emaconsent[1:2]
    
    #Identical
    totaln.ema<-200
    seqdate<-seq.Date(from=startdate,to=enddate,by="days")
    total<-as.data.frame(seqdate)
    total$accu<-seq(from=0,to=totaln.ema,length.out = length(seqdate))
    
    ema.start<-emaconsent$date[1]
    ema.end<-emaconsent$date[length(emaconsent$date)]
    ema.plan<-total[which(total$seqdate %in% ema.start):which(total$seqdate %in% ema.end),]
    names(ema.plan)<-c("date","Projection")
    
    seqdate.new<-seq.Date(from=ema.start,to=enddate,by="days")
    ema.newprojection<-as.data.frame(seqdate.new)
    ema.newprojection$accu<-seq(from=0,to=totaln.ema,length.out = length(seqdate.new))
    ema.newplan<-ema.newprojection[which(ema.newprojection$seqdate %in% ema.start):which(ema.newprojection$seqdate %in% ema.end),]
    names(ema.newplan)<-c("date","New Projection")
    
    
    merged.ema<-merge(emaconsent,ema.plan,by=1,all = T)
    merged.ema<-merge(merged.ema,ema.newplan,by=1,all = T)
    merged.ema.melt <- na.omit(melt(merged.ema, id.var='date'))
    names(merged.ema.melt)<-c("date","Number Type","count")
    
    
    
    
    ema.plot<-ggplot(merged.ema.melt, aes(x=date, y=count, color=`Number Type`)) +
      ggtitle(paste("EMA Participants, total:", length(emaconsent$date), ", in progress: ", length(emaconsent.f$date[emaconsent.f$ip==1]), ", completed: ", length(emaconsent.f$date[emaconsent.f$v2==1])))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line() +
      geom_point()+
      scale_color_manual(values=c('red', 'gray','blue')) +
      xlab(paste("Current Difference: ",round(merged.ema$Actual[length(merged.ema$date)]-merged.ema$Projection[length(merged.ema$date)]))) + 
      ylab("Number of Participants") +
      geom_label(data = merged.ema.melt[which(merged.ema.melt$`Number Type` == "Actual"),], aes(label=count))+
      geom_label(data = merged.ema.melt[max(which(merged.ema.melt$`Number Type` == "Projection")),], aes(label=round(count)))+
      geom_label(data = merged.ema.melt[max(which(merged.ema.melt$`Number Type` == "New Projection")),], aes(label=round(count)))
    
  }
  
}
##########################IRB NUMBER:
bsrc.irb.numsum<-function() {
  ID_SUPREME <- read_excel("Box Sync/skinner/projects_analyses/Project BPD Longitudinal/BPD Database/JC/RE/ID_ SUPREME.xlsx")
  ID_SUPREME[,5:8]<-NULL
  tkj<-bsrc.getidmatchdb(ID_SUPREME)
  tkj<-as.data.frame(tkj)
  newid<-as.data.frame(subreg$registration_redcapid[! subreg$registration_redcapid %in% tkj$registration_redcapid])
  names(newid)<-c("registration_redcapid")
  jrk<-merge(tkj,newid,all = T)
  nui<-subset(subreg,select = c("registration_redcapid","registration_status","registration_soloffid"))
  nui<-merge(jrk,nui,all = T)
  if (length(nui$Status[which(!nui$Status==nui$registration_status)])>0) {
    #Info user the conflict:
    return(as.data.frame(nui$registration_redcapid[which(!nui$Status==nui$registration_status)],nui$Status[which(!nui$Status==nui$registration_status)],nui$registration_status[which(!nui$Status==nui$registration_status)]))
    #which direction:
    direct.r<-readline(prompt = "Please type 'RC' for picking RedCap Status, or 'OG' for picking legacy status: ")
    direct.r<-as.numeric(direct.r)
    switch (direct.r,RC = nui$Status[which(!nui$Status==nui$registration_status)]<-nui$registration_status[which(!nui$Status==nui$registration_status)],
            OG = nui$Status[which(!nui$Status==nui$registration_status)]->nui$registration_status[which(!nui$Status==nui$registration_status)])}
  nui$Status[which(is.na(nui$Status))]<-nui$registration_status[which(is.na(nui$Status))]
  nui$iftranx<-NULL
  nui$StatusWord[nui$Status==88]<-"Ineligible Drop"
  nui$StatusWord[nui$Status==7]<-"IRB Admin Drop"
  nui$StatusWord[nui$Status==6]<-"Lost Contact/Drop"
  nui$StatusWord[nui$Status==5]<-"Deceased"
  nui$StatusWord[nui$Status==4]<-"Do Not Contact"
  nui$StatusWord[nui$Status==3]<-"In Jail"
  nui$StatusWord[nui$Status==2]<-"Missing"
  nui$StatusWord[nui$Status==1]<-"Active"
  
}







if (FALSE) {
opu$registration_initials <- paste(toupper(substr(opu$`First Name`,0,1)),toupper(substr(opu$`Last Name`,0,3)))
opu$registration_initials[which(opu$registration_initials=="NA NA")]<-"NA"
odz$registration_redcapid<-idmatch$redcapid[match(odz$ID,idmatch$soloffid)]
odz$iftranx<-is.na( match(odz$ID,idmatch$soloffid))

#######################
#Follow-up has unfinished graphic function: 

###Follow-Up
fuconsent<-as.data.frame(funbsrc$fudemo_visitdate[which(as.Date(funbsrc$fudemo_visitdate)>startdate)])
###Follow-Up
funbsrc$fudemo_visitdate[which(funbsrc$fudemo_visitdate=='')]<-NA
fuconsent<-as.data.frame(funbsrc$fudemo_visitdate[which(as.Date(funbsrc$fudemo_visitdate)>startdate)])
names(fuconsent)<-c("date")
fuconsent$date<-as.Date(sort(fuconsent$date))
fuconsent$Actual<-1:length(fuconsent$date)
totaln.fu<-100 #Different
fuconsent
fuconsent<-data.frame(funbsrc$registration_redcapid[which(as.Date(funbsrc$fudemo_visitdate)>startdate)],funbsrc$fudemo_visitdate[which(as.Date(funbsrc$fudemo_visitdate)>startdate)])
names(fuconsent)<-c('id',"date")
fuconsent$date<-as.Date(sort(fuconsent$date))
fuconsent$Actual<-1:length(fuconsent$date)
fuconsent<-data.frame(funbsrc$registration_redcapid[which(as.Date(funbsrc$fudemo_visitdate)>startdate)],funbsrc$fudemo_visitdate[which(as.Date(funbsrc$fudemo_visitdate)>startdate)])
names(fuconsent)<-c('id',"date")
fuconsent$inter<-interaction(fuconsent$id,fuconsent$date)

fuconsent<-fuconsent[which(!duplicated(fuconsent$inter)),]
fuconsent$inter<-NULL

fuconsent$month<-month(fuconsent$date)

test<-as.data.table(fuconsent)[,count:=seq_len(.N), by=month]

test<-test[order(-rank(month)),]
qplot (test$month,geom="histogram")
ggplot(data=test, aes(month)) +
geom_histogram(breaks=seq(12, 1, by = 1),
col="green",
fill="green",
alpha = .2) +
labs(title="Histogram by Month") +
labs(x="Age", y="Count") +
xlim(c(18,52)) +
ylim(c(1,12))
ggplot(data=test, aes(month)) +
geom_histogram(breaks=seq(1, 12, by = 1),
col="green",
fill="green",
alpha = .2) +
labs(title="Histogram by Month") +
labs(x="Age", y="Count") +
xlim(c(18,52)) +
ylim(c(1,12))
ggplot(data=test, aes(x=month, y=count)) +
geom_histogram(breaks=seq(12, 1, by = 1),
col="green",
fill="green",
alpha = .2) +
labs(title="Histogram by Month") +
labs(x="Age", y="Count") +
xlim(c(18,52)) +
ylim(c(1,12))
ggplot(data=test, aes(x=month, y=count)) +
geom_histogram(
col="green",
fill="green",
alpha = .2) +
labs(title="Histogram by Month") +
labs(x="Age", y="Count") +
xlim(c(18,52)) +
ylim(c(1,12))
qplot (test$month,geom="histogram")
qplot (test$month,geom="histogram", binwidth=20)
qplot (test$month,geom="histogram", binwidth=1)
qplot (test$month,geom="histogram", binwidth=30)
qplot (test$month,geom="histogram")

###Gender


###Edu###
funbsrc$demo_eduyears
which(!is.na(funbsrc$demo_eduyears))



#END OF NOT RUN CHUCK
}



