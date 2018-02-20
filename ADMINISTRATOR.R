---
Title: "Administrator"
Author: "Jiazhou Chen"
Version: 0.1
---
  
  
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
    startdate<-as.Date("2017-08-01")},
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

    
    new.plot<-ggplot(merged.new.melt, aes(x=date, y=count, color=`Number Type`)) +
    ggtitle(paste("New BPD Particiapnt, current total:", length(newconsent$date)))+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_line() +
    geom_point()+
    scale_color_manual(values=c('red', 'gray')) +
    xlab(paste("Current Difference: ",round(merged.new$Actual[length(merged.new$date)]-merged.new$Projection[length(merged.new$date)]))) + 
    ylab("Number of Participants") +
    scale_x_date(date_labels = "%B", date_breaks = "1 month")
    
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
      ggtitle(paste("EMA Particiapnts, total:", length(emaconsent$date), ", in progress: ", length(emaconsent.f$date[emaconsent.f$ip==1]), ", completed: ", length(emaconsent.f$date[emaconsent.f$v2==1])))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line() +
      geom_point()+
      scale_color_manual(values=c('red', 'gray','blue')) +
      xlab(paste("Current Difference: ",round(merged.ema$Actual[length(merged.ema$date)]-merged.ema$Projection[length(merged.ema$date)]))) + 
      ylab("Number of Participants") +
      scale_x_date(date_labels = "%B", date_breaks = "1 week")
    
    
  }
  
}


###Follow-Up


###Gender



###Edu





