###---
Title: "Administrator"
Author: "Jiazhou Chen"
Version: 0.1
###---

#Version 0.2 Changelog:
#New function: bsrc.admin.biweekly()
  #For B-Social biweekly meetings

####
#To do list:
# Functionalize the graphing function since it's almost identical in most process
# Functionalize Follow-up histagram
# Break down by group; use interaction() better
# NEW, EMA, MRI

#####

###########################Bi-Weekly Meeting Sheet:
bsrc.admin.biweekly<-function(type="Past 7 Days"){
  
  #Find Max Follow-Up Dates:
  funbsrc$fudemo_visitdate[which(funbsrc$fudemo_visitdate=="")]<-NA
  maxfudate<-aggregate(na.exclude(as.Date(funbsrc$fudemo_visitdate)),by=list(funbsrc$registration_redcapid[!is.na(funbsrc$fudemo_visitdate)]),max)
  names(maxfudate)<-c("registration_redcapid","fudemo_visitdate")
  
  #Get EMA Dates:
  emapg<-bsrc.getform(formname = "ema_progress_check")
  emapgonly<-subset(emapg,select = c("registration_redcapid","emapg_date_7days","emapg_date_14days","emapg_date_21days"))
  emapgonly$ema_maxdate<-apply(emapgonly[-grep("registration_redcapid",names(emapgonly))],1,max,na.rm=T)
  emapgonly.a<-subset(emapgonly,select = c("registration_redcapid","ema_maxdate"))
  #Get Baseline:
  baseline<-na.omit(subset(bsrc.getform(formname = "bldemo"),select = c('registration_redcapid',"demo_visitdate")))
  baseline$demo_visitdate<-as.Date(baseline$demo_visitdate)
  
  #Get Consented Dates:
  consented<-subset(subreg,select = c("registration_redcapid","registration_consentdate"))
  consented$registration_consentdate[consented$registration_consentdate==""]<-NA
  consented<-na.omit(consented)
  consented$registration_consentdate<-as.Date(consented$registration_consentdate)
  
  #Add additional component here
  #Merged:
  merged<-merge(merge(emapgonly.a,maxfudate,all=T),merge(baseline,consented,all=T),all=T)
  merged$maxwhich<-colnames(merged[-grep("registration_redcapid",names(merged))])[apply(merged[-grep("registration_redcapid",names(merged))],1,function(x) {which(x==max(x,na.rm=T))}[1])]
  merged$max<-apply(merged[-grep("registration_redcapid|maxwhich",names(merged))],1,max,na.rm=T)
  
  merged.recent<-merged[which(merged$max >= Sys.Date()-7),]
  
  
  return(merged.recent)
  
}










library("ggplot2")
  
  
opu$registration_initials <- paste(toupper(substr(opu$`First Name`,0,1)),toupper(substr(opu$`Last Name`,0,3)))
opu$registration_initials[which(opu$registration_initials=="NA NA")]<-"NA"
odz$registration_redcapid<-idmatch$redcapid[match(odz$ID,idmatch$soloffid)]
odz$iftranx<-is.na( match(odz$ID,idmatch$soloffid))

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

bsrc.datameeting<-function(protocol){
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
View(fuconsent)
unique(fuconsent$id)
length(unique(fuconsent$id))
length(fuconsent)
length(fuconsent$date)
duplicated(fuconsent$id)
which(duplicated(fuconsent$id))
fuconsent[which(duplicated(fuconsent$id)),]
fuconsent[which(duplicated(fuconsent$id))-1,]
fuconsent$date[which(duplicated(fuconsent$id))-1,] == fuconsent$date[which(duplicated(fuconsent$id)),]
fuconsent[which(duplicated(fuconsent$id))-1,]
fuconsent$date[which(duplicated(fuconsent$id))-1,]
fuconsent$date[which(duplicated(fuconsent$id))-1] == fuconsent$date[which(duplicated(fuconsent$id))]
which(fuconsent$date[which(duplicated(fuconsent$id))-1] == fuconsent$date[which(duplicated(fuconsent$id))])
fuconsent$date[which(duplicated(fuconsent$id)),][which(fuconsent$date[which(duplicated(fuconsent$id))-1] == fuconsent$date[which(duplicated(fuconsent$id))])]
fuconsent[which(duplicated(fuconsent$id)),][which(fuconsent$date[which(duplicated(fuconsent$id))-1] == fuconsent$date[which(duplicated(fuconsent$id))])]
fuconsent[which(duplicated(fuconsent$id)),][which(fuconsent$date[which(duplicated(fuconsent$id))-1] == fuconsent$date[which(duplicated(fuconsent$id))]),]
fuconsent[which(duplicated(fuconsent$id))-1,][which(fuconsent$date[which(duplicated(fuconsent$id))-1] == fuconsent$date[which(duplicated(fuconsent$id))]),]
fuconsent$inter<-interaction(fuconsent$id,fuconsent$date)
unique(fuconsent$inter)
length(unique(fuconsent$inter))
length(unique(fuconsent$id))
which(unique(fuconsent$inter))
unique(fuconsent$id) %in% fuconsent$inter
unique(fuconsent$inter) %in% fuconsent$inter
fuconsent$inter %in% unique(fuconsent$inter)
which(duplicated(fuconsent$inter))
which(!duplicated(fuconsent$inter))
fuconsent[which(!duplicated(fuconsent$inter)),]
fuconsent<-fuconsent[which(!duplicated(fuconsent$inter)),]
fuconsent$inter<-NULL
fuconsent$Actual<-1:length(fuconsent$date)
View(fuconsent)
(interval(start = as.Date(subreg$registration_dob), end = Sys.Date()))
month(fuconsent$date)
fuconsent$month<-month(fuconsent$date)
as.data.table(fuconsent)
as.data.table(fuconsent)[,count:=seq_len(.N), by=month]
test<-as.data.table(fuconsent)[,count:=seq_len(.N), by=month]
test$count
test[order(-rank(month)),]
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





