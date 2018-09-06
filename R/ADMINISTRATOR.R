###---
#Title: "Administrator"
#Author: "Jiazhou Chen"
#Version: 0.7.1
###---
###Version 0.7 & 0.7.1:
  #Added new function bsrc.emastats() for ema stats

##Version 0.6
  #Reworked bsrc.admin.biweekly()

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

######Percent function
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
###########################Bi-Weekly Meeting Sheet:
bsrc.admin.biweekly<-function(protocol=protocol.cur,days=14,monthz=2,exportpath=NA, curdb=NULL,...){
  if (is.null(curdb)){
  curdb<-bsrc.checkdatabase2(protocol = protocol,... = ...)}
  funbsrc<-curdb$data
  ifrun<-curdb$success
  subreg<-bsrc.getevent(eventname = "enrollment_arm_1",subreg = T,curdb = curdb,... = ...)
  if (ifrun) {
  #Find Max Follow-Up Dates: 
  funbsrc$fudemo_visitdate[which(funbsrc$fudemo_visitdate=="")]<-NA
  maxfudate<-aggregate(na.exclude(as.Date(funbsrc$fudemo_visitdate)),by=list(funbsrc$registration_redcapid[!is.na(funbsrc$fudemo_visitdate)]),max)
  names(maxfudate)<-c("registration_redcapid","Follow-up")
  
  #Get progress:
  futurefolks<-subreg[,c("registration_redcapid","registration_consentmonth","prog_diff","prog_lastfollow","prog_endor_y")]

  
  #Get EMA Dates:
  emapg<-bsrc.getform(formname = "ema_progress_check",curdb = curdb)
  emapgonly<-subset(emapg,select = c("registration_redcapid","emapg_date_7days","emapg_date_14days","emapg_date_21days"))
  emapgonly$ema_maxdate<-apply(emapgonly[-grep("registration_redcapid",names(emapgonly))],1,max,na.rm=T)
  emapgonly.a<-subset(emapgonly,select = c("registration_redcapid","ema_maxdate"))
  names(emapgonly.a)<-c("registration_redcapid","EMA")
  
  #Get MRI Dates:
  mripgonly<-bsrc.getform(formname = c("fmri_screening_form","fmri_session_checklist"),curdb = curdb)
  mripgonly.a<-subset(mripgonly,select = c("registration_redcapid","mricheck_scheudleddate","mricheck_scanneddate")) 
  mripgonly.b<-mripgonly.a[which(!is.na(mripgonly.a$mricheck_scheudleddate) | !is.na(mripgonly.a$mricheck_scanneddate)),]
  mripgonly.b$MRI<-apply(mripgonly.b[-grep("registration_redcapid",names(mripgonly.b))],1,max,na.rm=T)
  mripgonly.c<-subset(mripgonly.b, select = c("registration_redcapid","MRI"))
  mrisc<-mripgonly.c[which(mripgonly.c$MRI>Sys.Date()),]
  colnames(mrisc)[2]<-"MRI Scheduled"
  mripgonly.d<-mripgonly.c[-which(mripgonly.c$registration_redcapid %in% mrisc$registration_redcapid),]

  #Get Baseline:
  baseline<-na.omit(subset(bsrc.getform(formname = "bldemo",curdb = curdb),select = c('registration_redcapid',"demo_visitdate")))
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
  merged<-merge(merged.a,mripgonly.d,all=T)
  if (any(!mrisc$registration_redcapid %in% merged$registration_redcapid)){
    nmrisc<-mrisc[which(!mrisc$registration_redcapid %in% merged$registration_redcapid),]
    merged<-merge(merged,nmrisc,all = T)}
  merged$`Event`<-colnames(merged[-grep("registration_redcapid",names(merged))])[apply(merged[-grep("registration_redcapid",names(merged))],1,function(x) {which(x==max(x,na.rm=T))}[1])]
  merged$`Event Date`<-apply(merged[-grep("registration_redcapid|Event",names(merged))],1,max,na.rm=T)
  merged.simp<-subset(merged,select = c("registration_redcapid","Event","Event Date"))
  #Add Status
  merged.simp$`MRI Status`<-subreg$prog_fmristatus[match(merged.simp$registration_redcapid,subreg$registration_redcapid)]
  merged.simp$`EMA Status`<-subreg$prog_emastatus[match(merged.simp$registration_redcapid,subreg$registration_redcapid)]
  #Add Initials & Age:
  merged.simp$`Age`<-subreg$prog_cage[match(merged.simp$registration_redcapid,subreg$registration_redcapid)]
  merged.simp$`Initials`<-subreg$registration_initials[match(merged.simp$registration_redcapid,subreg$registration_redcapid)]
  merged.simp$`Group`<-subreg$registration_group[match(merged.simp$registration_redcapid,subreg$registration_redcapid)]
  merged.simp$`Latest IPDE Date`<-subreg$prog_latestipdedate[match(merged.simp$registration_redcapid,subreg$registration_redcapid)]
  merged.simp$`IPDE Dx`<-subreg$prog_latestipdes_dx[match(merged.simp$registration_redcapid,subreg$registration_redcapid)]
  merged.simp$`IPDE Dx`<-plyr::mapvalues(merged.simp$`IPDE Dx`, from = c(1:3), to = c("Negative","Probable","Definite"),warn_missing = F)
  
  #Refine Status: 
  ord<-c("Consented","Baseline","Follow-up","MRI","EMA")
  merged.simp<-merged.simp[order(match(merged.simp$`Event`,ord),merged.simp$`Event Date`),]
  merged.simp$Group<-plyr::mapvalues(merged.simp$Group,from = c("1","2","3","4","88","89"), to=c("HC","LL","HL","NON-ATT","UNCLEAR","INELIGIBLE"),warn_missing = F)
  colnames(merged.simp)[grep("registration_redcapid",names(merged.simp))]<-"RedCap ID"
  merged.simp$Event[which(merged.simp$Event=="Follow-up")]<-paste(subreg$prog_lastfollow[match(merged.simp[merged.simp$Event=="Follow-up",]$`RedCap ID`,subreg$registration_redcapid)],"Yrs Follow-up")
  merged.simp$Event[which(merged.simp$Event=="0.5 Yrs Follow-up")]<-"6 Mons Follow-Up"
  merged.simp$Event[which(merged.simp$Event=="0.25 Yrs Follow-up")]<-"3 Mons Follow-Up"
  #merged.simp$Group[which(merged.simp$`RedCap ID` %in% subreg$registration_redcapid[which(subreg$registration_status=="88")])]<-"INELIGIBLE"
  #merged.simp$Event[which(merged.simp$`RedCap ID` %in% subreg$registration_redcapid[which(subreg$registration_status=="88")])]<-"RULED OUT"
  #FU Month:
  
  if (lubridate::month(Sys.Date())<6){
  merged.simp$`Follow-up Month`<-month.name[futurefolks$registration_consentmonth[match(merged.simp$`RedCap ID`,futurefolks$registration_redcapid)]]
  }
  else {
    merged.simp$`Follow-up Month`<-month.name[futurefolks$registration_consentmonth[match(merged.simp$`RedCap ID`,futurefolks$registration_redcapid)]+6]
  }
  merged.simp<-merged.simp[,c("RedCap ID","Initials","Age","Group","Follow-up Month","Event","Event Date","Latest IPDE Date","IPDE Dx","MRI Status","EMA Status")]

  
  #########Future Folks
  #curmon<-month(Sys.Date())
  #getmon<-curmon+monthz-1
  #getmon<-ifelse(curmon>=12,getmon-12,getmon)
  #threemon<-ifelse(month(Sys.Date())<=3,15-month(Sys.Date()),month(Sys.Date())-3)
  #getthreemon<-threemon+monthz-1
  #getthreemon<-ifelse(threemon>=12,getthreemon-12,getthreemon)
  #tarmon<-seq(curmon,getmon)
  #tarmonthree<-seq(threemon,getthreemon)
  
  ####

  curdate<-Sys.Date()
  plusmon<-Sys.Date()
  lubridate::month(plusmon)<-lubridate::month(curdate)+monthz-1
  tarmon<-lubridate::month(seq.Date(from = curdate, to = plusmon, by="mon"))
  tarmon[which(tarmon>6)]<-tarmon[which(tarmon>6)]-6
  
  #To prevent the Febuary non-sense
  usedate<-Sys.Date()
   if (lubridate::day(Sys.Date())>28) {lubridate::day(usedate)<-28}
  
  threemon<-usedate
  plusthreemon<-usedate
  lubridate::month(threemon)<-lubridate::month(usedate)-3
  lubridate::month(plusthreemon)<-lubridate::month(threemon)+monthz-1
  tarmonthree<-lubridate::month(seq.Date(from = threemon, to = plusthreemon, by="mon"))
  tarmonthree[which(tarmonthree>6)]<-tarmon[which(tarmonthree>6)]-6
  
  futureid.x<-subreg$registration_redcapid[which(subreg$registration_consentmonth %in% tarmon & subreg$prog_diff>0 & subreg$registration_status!="89"& subreg$prog_diff< monthz+0.1)]
  threemonid<-subreg$registration_redcapid[which(subreg$prog_endor_y==0.25 & subreg$prog_diff>0 & subreg$registration_status!="89" & subreg$registration_consentmonth %in% tarmonthree)]
  futureid<-append(futureid.x,threemonid)

  future<-merged.simp[match(futureid,merged.simp$`RedCap ID`),]
  # Change Fu Month on 3 months folks
  future$`Follow-up Month`[match(threemonid,future$`RedCap ID`)]<-month.name[match(future$`Follow-up Month`[match(threemonid,future$`RedCap ID`)],month.name)+3]
  #future$`Follow-up Month`<-month.name[futurefolks$registration_consentmonth[match(future$`RedCap ID`,futurefolks$registration_redcapid)]]
  future<-future[order(match(future$`Follow-up Month`,month.name)),]
  future$`Follow-up Due`<-paste(subreg$prog_endor_y[match(future$`RedCap ID`,subreg$registration_redcapid)],"Yrs Follow-up")
  future$`Follow-up Due`[which(future$`Follow-up Due`=="0.5 Yrs Follow-up")]<-"6 Mons Follow-Up"
  future$`Follow-up Due`[which(future$`Follow-up Due`=="0.25 Yrs Follow-up")]<-"3 Mons Follow-Up"
  future$`Follow-up Due`[which(future$`Follow-up Due`=="0 Yrs Follow-up")]<-"Baseline"
  future<-future[,c("RedCap ID","Initials","Age","Group","Follow-up Month","Event","Event Date","Follow-up Due","Latest IPDE Date","IPDE Dx","MRI Status","EMA Status")]
  names(future)<-c("RedCap ID","Initials","Age","Group","Follow-up Month","Last Event","Last Event Date","Follow-up Due","Latest IPDE Date","IPDE Dx","MRI Status","EMA Status")
  rownames(future)<-NULL
  
  ########Current Folks
  merged.recent<-merged.simp[which(merged.simp$`Event Date` >= Sys.Date()-days),]
  rownames(merged.recent)<-NULL

  #######
  if (is.na(exportpath)){
  
  return(list(Past_Two_Weeks=merged.recent,Next_Two_Month = future))
  }
  else {
    writexl::write_xlsx(list(Past_Two_Weeks=merged.recent,Next_Two_Month = future),path = "BS_BIWEEK_REPORT.xlsx")
  }
}}

################# Future update to include automatic sync
bsrc.emastats<-function(protocol=protocol.cur,shortlist=T,...) {
  curdb<-bsrc.checkdatabase2(protocol = protocol,... = ...)
  subreg<-bsrc.getevent(eventname = "enrollment_arm_1",subreg = T,curdb = curdb)
  #Get funema:
  funema<-bsrc.getform(formname = "ema_session_checklist",grabnewinfo = T)
  
  emastate<-funema[c(1,grep("ema_completed___",names(funema)))]
  emastate$status<-names(emastate)[c(-1)][apply(emastate[c(-1)], 1, function(x) {which(x==1)}[1])]
  emastate$status[which(is.na(emastate$status))]<-"UNKNOWN"
  emastate$`EMA Status FULL`<-plyr::mapvalues(emastate$status,from = c("ema_completed___ip","ema_completed___2","ema_completed___3","ema_completed___999","ema_completed___et"), 
                                              to = c("IN PROGRESS","COMPELETED VERSION 2","COMPELETED VERSION 3","DID NOT COMPELETE","Early Termination"), warn_missing = F)
  emastate$`EMA Status`<-emastate$`EMA Status FULL`
  emastate$`EMA Status`[agrep("COMPLETED",emastate$`EMA Status`)]<-"COMPLETED"
  emastate$group.num<-subreg$registration_group[match(emastate$registration_redcapid,subreg$registration_redcapid)]
  emastate$`GROUP`<-plyr::mapvalues(emastate$group,from = c("1","2","3","4","88","89"), 
                                    to = c("HEALTHY CONTROL","LOW LETHALITY","HIGH LETHALITY","NON-SUICIDAL","NOT SURE YET","INELIGIBLE (WHY???)"), warn_missing = F)
  emacount<-xtabs(~GROUP+`EMA Status`,emastate)
  emacount<-addmargins(emacount)
  
  if (shortlist){
    emastate.s<-emastate
    emastate.s$group.num<-NULL
    emastate.s$status<-NULL
    emastate.s<-emastate.s[-grep("ema_completed__",names(emastate.s))]
    return(list(emastatus=emastate.s,emacount=emacount))
  } else {return(list(emastatus=emastate,emacount=emacount))}
  
  if (length(which(!emastate$status.w %in% c("IN PROGRESS","COMPELETED VERSION 2","COMPELETED VERSION 3")))>0){
    print("REMARKABLE PT:")
    for (i in 1:length(emastate$registration_redcapid[which(!emastate$status.w %in% c("IN PROGRESS","COMPELETED VERSION 2","COMPELETED VERSION 3"))])) {
      print("####################")
      idinv<-emastate$registration_redcapid[which(!emastate$status.w %in% c("IN PROGRESS","COMPELETED VERSION 2","COMPELETED VERSION 3"))][i]
      print(paste("RedCap ID: ",idinv))
      print(paste("NOTES: ",funema$ema_masnote[match(idinv,funema$registration_redcapid)]))
    }
  }
  
}
##################
bsrc.admin.rcstats<-function(dbenvir=NULL) {
  if (!exists("data",envir = dbenvir) | !exists("metadata",envir = dbenvir) ) {stop("Not enough info from dbenvir")}
  group.vari.name<-names(dbenvir$data)[grep("group",names(dbenvir$data))[1]]
  reg.form<-bsrc.getform(curdb = dbenvir, forceskip = T, 
               formname = dbenvir$metadata$form_name[which(dbenvir$metadata$field_name==group.vari.name)])
  if (any(as.numeric(substring(reg.form$redcap_event_name,regexpr("_arm_",reg.form$redcap_event_name)[1]+nchar("_arm_")))>1)) {
    multiarm<-TRUE}
  #Total Numeber:
  
  bsrc.getchoicemapping(variablenames = group.vari.name,metadata = dbenvir$metadata)
  
  
}
###########################RPPR Report:
bsrc.admin.rppr<-function(){
  
  newconsent<-subreg[which(as.Date(subreg$registration_consentdate)>startdate & subreg$registration_status!=89),]
  totaln<-length(newconsent$registration_redcapid)
}

#####Utility function:
do_for_asub<-function(xdata=NULL,lxyz=c("gender","race","edu","age"),tit="B-Social New Consent",plotpath=NULL,filename=NULL){
  for (xyz in lxyz) {
    datalist=get(paste0(xyz,"group"))
    print(datalist$x2)
    assign(paste0(xyz,"plot"),graph_data_meet(datalist = datalist, xdata = xdata, title = tit))
  }
  
  eval(parse(text=paste0("x<-gridExtra::arrangeGrob(",paste(paste0(lxyz,"plot"),collapse = ","),")")))
  ggsave(plot = x,
         filename = filename,device = "jpeg",dpi = 300,path = plotpath,width = 11.69,height = 8.27)
}

graph_data_meet<-function(datalist=NULL,xdata=NULL,title=NULL,save=F,savepath=NULL,filename=NULL){
  if (is.null(datalist$x2)) {
    eval(parse(text = paste0("test<-as.data.frame(xtabs(formula = ","~",datalist$x1,",data = xdata))")))
    datalist$x2<-""
    xd<-aes(x = test[[1]], y=Freq)
  } else {
    eval(parse(text = paste0("test<-as.data.frame(xtabs(formula = ","~",datalist$x1,"+",datalist$x2,",data = xdata))")))
    xd<-aes(fill = test[[1]],x = test[[2]], y=Freq)
  }
  plotx<-ggplot(data=test, xd)+
    geom_bar(position = "dodge",stat="identity",color="black")+
    geom_text(stat = "identity",aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.5)+
    ggtitle(paste(title,"by",datalist$x1,"and",datalist$x2)) +
    xlab(datalist$x2) + ylab("Frequency Count")+
    scale_fill_brewer(palette="OrRd")
  #+scale_fill_discrete(name = datalist$x1)
  plotx$labels$fill <- datalist$x1
  return(plotx)
}
###########################Data Meeting:
bsrc.datameeting<-function(curdb=NULL,protocol=protocol.cur,plotpath=NULL){
  if (is.null(curdb)){
  curdb<-bsrc.checkdatabase2(protocol = protocol,... = ...)}
  funbsrc<-curdb$data
  subreg<-bsrc.getevent(eventname = "enrollment_arm_1",subreg = T,curdb = curdb,... = ...)
  
  switch (protocol$name,
          bsocial = {
            enddate<-as.Date("2020-08-01") 
            startdate<-as.Date("2017-07-30")
            projection<-list(totaln=100,eman=200)
          },
          ksocial = {
            enddate<-as.Date("2021-08-01") 
            startdate<-as.Date("2017-08-01")
          })

  #ADD GROUP TO IT
  bsrc.getchoicemapping("registration_group",metadata = curdb$metadata)->groupmap
  subreg$`Group`<- plyr::mapvalues(x = subreg$registration_group, from = groupmap$choice.code, to = as.character(groupmap$choice.string),warn_missing = F)
  #Gender
  subreg$Gender<-subreg$registration_gender
  gendergroup<-list(x1="Group",x2="Gender")
  #Race
  subreg<-bsrc.checkbox(subreg)
  subreg$registration_race_string->subreg$race
  subreg$race[subreg$registration_race_ifmultiple]<-"Mixed"
  bsrc.getchoicemapping("registration_race",metadata = curdb$metadata)->racemap
  racemap$choice.string<-c("AmerIndi","Asian","AfriAmer","PaciIslander","White","Refused")
  subreg$Race<- plyr::mapvalues(x = subreg$race, from = racemap$choice.code, to = as.character(racemap$choice.string),warn_missing = F)
  racegroup<-list(x1="Group",x2="Race")
  #Educ
  bldemo<-bsrc.getform(curdb = curdb,formname = "bldemo")
  bldemo$demo_eduyears[match(subreg$registration_redcapid,bldemo$registration_redcapid)]->subreg$eduyrs
  edumap<-data.frame(yrlable=c("0-12","13-14","15-16","17-18","18+"),yrstar=c(0,13,15,17,19))
  subreg$Edu<-as.character(edumap$yrlable[findInterval(subreg$eduyrs,edumap$yrstar)])
  edugroup<-list(x1="Group",x2="Edu")
  
  #age
  subreg$ageyrs<-lubridate::as.period(lubridate::interval(as.Date(subreg$registration_dob),as.Date(subreg$registration_consentdate)))$year
  agemap<-data.frame(yrlable=c("18-25","26-30","31-40","41-50","51-60","61-70","70+"),yrstar=c(0,26,31,41,51,61,71))
  subreg$`Age`<-as.character(agemap$yrlable[findInterval(subreg$ageyrs,agemap$yrstar)])
  agegroup<-list(x1="Group",x2="Age")
  
  onlygroup<-list(x1="Group",x2=NULL)

  if(protcol=="bsocial") {
    #New Consent since Auguest 2017

    newconsent<-subreg[which(as.Date(subreg$registration_consentdate)>startdate & !subreg$registration_group %in% c(88,89)),]
    do_for_asub(newconsent,tit = "B-Social New Consent",plotpath=plotpath,filename="by_newconsent.jpeg")
    nplot<-graph_data_meet(datalist = onlygroup,xdata = newconsent,title = "New Consent")
    
    subreg[grep("Completed",subreg$prog_emastatus),]->emacompleted
    do_for_asub(emacompleted,tit = "B-Social EMA Completed",plotpath=plotpath,filename="by_ema.jpeg")
    eplot<-graph_data_meet(datalist = onlygroup,xdata = emacompleted,title = "EMA")
    
    fmricompleted<-subreg[grep(paste("Scanned","2016 Pilot",sep = "|"), subreg$prog_fmristatus),]
    do_for_asub(fmricompleted, tit = "B-Social fMRI Completed (include 2016 pilot)",plotpath=plotpath,filename="by_fMRI.jpeg")
    fplot<-graph_data_meet(datalist = onlygroup,xdata = fmricompleted,title = "fMRI")
    
    lastyrfu<-subreg[which(subreg$prog_endorfu<365),]
    do_for_asub(lastyrfu, tit = "B-Social Follow-Up within last year",plotpath=plotpath,filename="by_fu.jpeg")
    fuplot<-graph_data_meet(datalist = onlygroup,xdata = lastyrfu,title = "Follow-Up")
    
    
    zx<-gridExtra::arrangeGrob(nplot,eplot,fplot,fuplot)
    ggsave(plot = zx,
           filename = "grp.jpeg",device = "jpeg",dpi = 300,path = plotpath,width = 11.69,height = 8.27)
    
    
    
    newconsent$date<-as.Date(newconsent$registration_consentdate)
    
    sortedpos<-sort.int(newconsent$date,index.return=T)$ix
    newconsent<-newconsent[sortedpos,]
    newconsent$Actual<-1:length(newconsent$date)

    
    #IDENTICAL
    seqdate<-seq.Date(from=startdate,to=enddate,by="days")
    totalseq<-seq(from=0,to=projection$totaln,length.out = length(seqdate))
    total<-as.data.frame(seqdate)
    total$accu<-totalseq
    
    new.start<-newconsent$date[1]
    new.end<-newconsent$date[length(newconsent$date)]
    new.plan<-total[which(total$seqdate %in% new.start):which(total$seqdate %in% new.end),]
    names(new.plan)<-c("date","Projection")
    subconsen<-data.frame(date=newconsent$date,Actual=newconsent$Actual)
    merged.new<-merge(subconsen,new.plan,by=1,all = T)
    merged.new.melt <- na.omit(reshape2::melt(merged.new, id.var='date'))
    names(merged.new.melt)<-c("date","Number Type","count")
    merged.new.melt$label<-merged.new.melt$count
    #merged.new.melt$label[merged.new.melt$`Number Type`!="Actual"]
    
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
    emaconsent$v3<-funbsrc$ema_completed___3[which(!is.na(funbsrc$ema_setuptime))]
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
      ggtitle(paste("EMA Participants, total:", length(emaconsent$date), ", in progress: ", length(emaconsent.f$date[emaconsent.f$ip==1]), ", completed: ", length(emaconsent.f$date[emaconsent.f$v2==1])+length(emaconsent.f$date[emaconsent.f$v3==1])))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line() +
      geom_point()+
      scale_color_manual(values=c('red', 'gray','blue')) +
      xlab(paste("Current Difference: ",round(merged.ema$Actual[length(merged.ema$date)]-merged.ema$Projection[length(merged.ema$date)]))) + 
      ylab("Number of Participants") +
      geom_label(data = merged.ema.melt[which(merged.ema.melt$`Number Type` == "Actual"),], aes(label=count))+
      geom_label(data = merged.ema.melt[max(which(merged.ema.melt$`Number Type` == "Projection")),], aes(label=round(count)))+
      geom_label(data = merged.ema.melt[max(which(merged.ema.melt$`Number Type` == "New Projection")),], aes(label=round(count)))
    
  tz<-gridExtra::arrangeGrob(new.plot,ema.plot)
  ggsave(plot = tz,filename = "recurt.jpeg",device = "jpeg",dpi = 300,path = plotpath,width = 11.69,height = 8.27)  
  
    
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
  nui$StatusWord[nui$Status==89]<-"Ineligible Drop"
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


#######Death data pie chart code:



deathdata$DeathCat.words<-plyr::mapvalues(deathdata$`Death Category`,from = 1:7, 
                                          to = c("Cardiovascular/Respiratory Related",
                                                 "Cancer",
                                                 "Dementia",
                                                 "Other",
                                                 "Suicide",
                                                 "Suicide",
                                                 "Accidental Death"))

as.data.frame(table(deathdata$DeathCat.words))->deathcat.df
names(deathcat.df)<-c("Death Category","Proportions")
deathcat.df$`Death Category`<-paste(deathcat.df$`Death Category`," n=[",deathcat.df$Proportions,"]",sep = "")


ggplot(deathcat.df, aes(x="", y=Proportions, fill=`Death Category`))+
  ggtitle("Proportions of Death by Reported Cause")+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")+
  xlab("")+
  #scale_fill_brewer(palette="Set2")+
  scale_fill_grey()+
  theme(axis.text.x=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))+
  geom_text(aes(label = percent(deathcat.df$Proportions/sum(deathcat.df$Proportions),digits = 0)), size=5, position = position_stack(vjust = 0.5))+
  labs(caption = "Other includes all other deaths by natural causes, such as sepsis, multiorgan failure, DM, etc.
       Suicides include 5 confirmed deaths by suicide and 4 strongly suspected suicides (i.e. drug overdose but not explicitly listed as suicide on death certificate).
       Accidental death includes car accident.")






#END OF NOT RUN CHUCK
}

