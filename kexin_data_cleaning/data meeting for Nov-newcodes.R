# setup 
setwd("~/Documents/github/UPMC/data meeting/")
source("~/Documents/github/UPMC/startup.R")

protocol.cur <- ptcs$masterdemo
md <- bsrc.checkdatabase2(online = F)
#plotpath="/Users/mogoverde/Desktop" #Morgan WFH
#plotpath="C:/Users/buerkem/OneDrive - UPMC/Desktop" #Morgan Work
plotpath="/Documents/github/UPMC/data meeting" #Kexin

#PT3 recruitment milestone
recr_pt3<-read.csv("~/Box/skinner/administrative/Data meeting/Recruitment milestones for MH085651 - Decision Process of Late-Life Suicide.csv",stringsAsFactors = F)

#get id 
MD<-bsrc.getform(formname = 'record_registration',curdb = md)
#ptcname: ksocial, bsocial,protect3,protect2,protect,suicid2,suicide,EMA,EXPLORE
#Grabs the variable names based on protocol
varnames<-function(ptcname){
  id<-c('registration_redcapid', 'registration_wpicid', 'registration_demostatus','registration_group','registration_dob','registration_gender',
        paste0(c('registration_ptcstat___','reg_condate_','reg_term_yesno_','reg_term_reason_','reg_term_excl_'),ptcname))
  if(ptcname=='protect3'){return(c(id,'reg_status_protect3','reg_p3catchup'))
  }else if(ptcname=='protect2'){return(c(id,'reg_status_protect2'))
  }else if(ptcname=='protect'){return(c(id,'reg_status_protect'))
  }else if(ptcname=='suicid2'){return(c(id,'reg_status_suicid2'))
  }else if(ptcname=='suicide'){return(c(id,'reg_status_suicide'))
  }else if(ptcname=='ksocial'){return(c(id,'registration_ptcstat___bsocial'))
  }else{return(id)}
}

#PROTECT - ALL
var_pt<-unique(c(varnames('protect3'),varnames('protect2'),varnames('protect'),varnames('suicide'),varnames('suicid2')))
varnames("ksocial")->var_k
varnames("bsocial")->var_ema
varnames("bsocial")->var_bsoc

#Checks1
all(var_pt %in% colnames(MD)) #should be TRUE
all(var_k %in% colnames(MD)) #Ksocial
all(var_bsoc %in% colnames(MD)) #bsocial and ema

#Add master demo info
PT<-MD[,var_pt]
K<-MD[,var_k]
BS<-MD[,var_bsoc]


##More checks of each protocol
#Protect checks
PT<-PT[unique(which(PT[,grep('registration_ptcstat_',var_pt)]==1,arr.ind = T)[,1]),] # consented to at least one protocol 
#temp<-PT[unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # ieligible people 
PT<-PT[-unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # remove all ineligible people terminated for at least one protocol due to ineligibility  
PT<-PT[-unique(which(PT[,grep('reg_term_excl_',var_pt)]==1,arr.ind = T)[,1]),] # remove all unusable data based on 'reg_term_excl_

#KSOCIAL checks (must be in KSOCIAL, can also be in BSOCIAL)
K[which(K$registration_ptcstat___ksocial==1 & !is.na(K$registration_ptcstat___ksocial==1)),]->K
K[which(K$reg_term_reason_ksocial!=3 | is.na(K$reg_term_reason_ksocial)),]->K
K[which(K$reg_term_excl_ksocial!=1 | is.na(K$reg_term_excl_ksocial)),]->K

#BSOCIAL checks
BS[which(BS$registration_ptcstat___bsocial==1 & !is.na(BS$registration_ptcstat___bsocial==1)),]->BS
BS[which(BS$reg_term_reason_bsocial!=3 | is.na(BS$reg_term_reason_bsocial)),]->BS
BS[which(BS$reg_term_excl_bsocial!=1 | is.na(BS$reg_term_excl_bsocial)),]->BS


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

##Ksocial
sum(duplicated(K$registration_redcapid)) 
sum(K$registration_group==89) 
sum(is.na(K$registration_redcapid)) 
sum(is.na(K$registration_group))
sum(is.na(K$registration_dob))
sum(K$reg_term_excl_ksocial==1,na.rm = T)

##Bsocial
sum(duplicated(BS$registration_redcapid))
sum(BS$reg_term_excl_bsocial==1,na.rm = T)
sum(is.na(BS$registration_redcapid)) 
#IDs of the participants with no group status
sum(is.na(BS$registration_group))
BS[which(is.na(BS$registration_group)),"registration_redcapid"]
#Remove ineligible
sum(BS$registration_group==89 & !is.na(BS$registration_group==89))
BS[-which(BS$registration_group==89 & !is.na(BS$registration_group==89)),]->BS
#One person with missing dob, new person, remove (no initials or ANYTHING)
sum(is.na(BS$registration_dob))
BS[-which(is.na(BS$registration_dob)),]->BS



##################
#Protect first consent date
PT$'mincondate'<-zoo::as.Date(apply(PT[,grep('reg_condate_',var_pt)],1,function(x){min(as.Date(x),na.rm=T)}))
sum(is.na(PT$mincondate))# should be 0. no NA in 'mincondate'

#MAKE GRAPHS. subreg is the clean data that contains only participants that we want 
                      #(and no duplicated rows). earliest consentdate is called: mincondate
#subreg<-PT
#curdb<-md
#PREPARE LISTS FOR PLOTTING 
plot_prep<-function(subreg, curdb){#add Group to df 
    as.Date(subreg$mincondate)->subreg$mincondate
    groupmap<-bsrc.getchoicemapping("registration_group",metadata = curdb$metadata)
    subreg$`Group`<- plyr::mapvalues(x = subreg$registration_group, from = groupmap$choice.code, to = as.character(groupmap$choice.string),warn_missing = T)
    onlygroup<-list(x1="Group",x2=NULL)
    #Gender
    subreg$Gender<-subreg$registration_gender
    gendergroup<-list(x1="Gender",x2="Group")
    #Age
    subreg$ageyrs<-lubridate::as.period(lubridate::interval(as.Date(subreg$registration_dob),as.Date(subreg$mincondate)))$year
    agemap<-data.frame(yrlable=c("18-25","26-30","31-40","41-50","51-60","61-70","70+"),yrstar=c(0,26,31,41,51,61,71))
    subreg$`Age`<-as.character(agemap$yrlable[findInterval(subreg$ageyrs,agemap$yrstar)])
    agegroup<-list(x1="Age",x2="Group")
    return(subreg)
    return(onlygroup)
    return(gendergroup)
    return(agegroup)}


#Protect data frame use
  #Protect all
    #Plots
    plot_prep(PT,md)->Pall
    do_for_asub(Pall,tit = 'Protect (all)',plotpath = plotpath,filename = 'Protect(all)byGroup_age_gender.jpeg')
  #Get EXPLORE scanned
    #Get files from box
    root="/Users/mogoverde/Box/skinner/data/"
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
      sum(!Eallmd %in% Pall$registration_redcapid) #Should be =0, all IDs should be in Pall
    #Get EXPLORE pts from those in Protect
    Pall[which(Pall$registration_redcapid %in% Eallmd),]->Eall
    do_for_asub(Eall,tit = 'EXPLORE',plotpath = plotpath,
                filename = 'ExplorebyGroup_age_gender.jpeg')
  

#BSOCIAL data frame use
names(BS)[grepl("condate",names(BS))]<-"mincondate"
plot_prep(BS,md)->Ball

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
  Bmrimddf$masterdemoid
  bsrc.findid(Bspottid,idmap = idmap,id.var = "ID")->Bspottmddf
  Bspottmddf[which(!Bspottmddf$ifexist),] #Should be no one
  #Plotting and final df
  Ball[which(Ball$registration_redcapid %in% Bmrimddf$masterdemoid),]->Bmriall
    do_for_asub(Bmriall,tit = 'BSOCIAL MRI',plotpath = plotpath,
                filename = 'BsocialmribyGroup_age_gender.jpeg')
  Ball[which(Ball$registration_redcapid %in% Bspottmddf$masterdemoid),]->Bspottall
    do_for_asub(Bspottall,tit = 'BSOCIAL SPOTT',plotpath = plotpath,
                filename = 'BsocialspottbyGroup_age_gender.jpeg')
  #Get people in EMA
    bsrc.attachngrab(rdpaths$ema)->ema
    ema$fulldata.ema$info->emainfo
    table(emainfo$Status)
    #Check IDs
    emainfo[which(!emainfo$RedcapID %in% Ball$registration_redcapid),"RedcapID"]
    #Remove one person for being ineligible
    emainfo[-which(emainfo$RedcapID=="440057"),]->emainfo
    #Plotting and df
    Ball[which(Ball$registration_redcapid %in% emainfo$RedcapID),]->Bemaall
    do_for_asub(Bemaall,tit = 'BSOCIAL EMA',plotpath = plotpath,
                filename = 'BsocialemabyGroup_age_gender.jpeg')
   
#Plot prep for KSOCIAL
  names(K)[grepl("condate",names(K))]<-"mincondate"
  plot_prep(K,md)->Kall
  #Get which participants completed each task (K Only)
  list.files(path=paste0(root, "matlab task data/ksoc_clock"))->Kclock
  list.files(path=paste0(root, "eprime/ksoc_trust"))->Ktrust
  #Any ids not in Kall
  Kclock[which(!Kclock %in% Kall$registration_redcapid)] #should be 0
  Ktrust[which(!Ktrust %in% Kall$registration_redcapid)] #should be 0
  #B/Ksocial pts (gathered from BSOCIAL section)  
    as.numeric(BKtasks)->BKtasks
    #Make df for ID matching
    data.frame(ID=BKtasks, extra=T)->BKtasksid
    bsrc.findid(BKtasksid,idmap = idmap,id.var = "ID")->BKmddf
    BKmddf[which(!BKmddf$ifexist),] #should be none
    BKmddf$masterdemoid->BKid
    unique(append(append(Kclock,Ktrust),BKid))->Kmri
    #Plotting and df
    Kall[which(Kall$registration_redcapid %in% Kmri),]->Kmriall
    do_for_asub(Kmriall,tit = 'KSOCIAL',plotpath = plotpath,
                filename = 'KsocialbyGroup_age_gender.jpeg')
    

# recruitment graph for pt3 and bsocial
  #Only P3
  Pall[which(!is.na(Pall$registration_ptcstat___protect3) & Pall$registration_ptcstat___protect3==1),]->P3 #get P3 only
  #Number of P3 not carried over from P2
  sum(P3$reg_p3catchup=="00")
  P3[which(P3$reg_p3catchup=="P2"),c("registration_redcapid","ageyrs")]
  sum(P3$ageyrs>=55)
  #Plots
  do_for_asub(P3,tit = 'Protect 3',plotpath = plotpath,filename = 'Protect3byGroup_age_gender.jpeg') #group by 
  
####################Jiazhou's plotting#################
  newconsent<-P3
  newconsent$date<-as.Date(newconsent$reg_condate_protect3)
  sortedpos<-sort.int(newconsent$date,index.return=T)$ix #############HERE!!!
  newconsent<-newconsent[sortedpos,]
  newconsent$Actual<-1:length(newconsent$date)
  newconsent$month<-month(newconsent$date)
  #newconsent$year<-year(newconsent$date)
  newconsent<-newconsent[which(lubridate::as.period(lubridate::interval(as.Date(Sys.Date()),as.Date(newconsent$date)))$year >= 0),]
  newconsent<-newconsent[which(!duplicated(newconsent$month,fromLast=T)),]
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
  #merged.new.melt$label[merged.new.melt$`Number Type`!="Actual"]
  
  
  new.plot<- ggplot(merged.new.melt, aes(x=date, y=count, label=count, color=`Number Type`)) +
    ggtitle("New BPD Participant")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_line() +
    geom_point(data = merged.new.melt[which(merged.new.melt$`Number Type` == "Actual"),])+
    scale_color_manual(values=c('red', 'gray')) +
    xlab(paste("Current Difference: ",round(merged.new$Actual[length(merged.new$date)]-merged.new$Projection[length(merged.new$date)]))) + 
    ylab("Number of Participants") +
    scale_x_date(date_labels = "%B", date_breaks = "1 month") +
    geom_label(data = merged.new.melt[which(merged.new.melt$`Number Type` == "Actual"),], aes(label=count))+
    geom_label(data = merged.new.melt[max(which(merged.new.melt$`Number Type` == "Projection")),], aes(label=round(count)))

#######################################################




#JIAZHOU'S UTILITY FUNCTION
do_for_asub<-function(xdata=NULL,lxyz=c("only","gender","age"),tit="B-Social New Consent",plotpath=NULL,filename=NULL){
  for (xyz in lxyz) {
    datalist=get(paste0(xyz,"group"))
    print(datalist$x1)
    assign(paste0(xyz,"plot"),graph_data_meet(datalist = datalist, xdata = xdata, title = tit))
  }
  
  eval(parse(text=paste0("x<-gridExtra::arrangeGrob(",paste(paste0(lxyz,"plot"),collapse = ","),")")))
  ggsave(plot = x,
         filename = filename,device = "jpeg",dpi = 300,path = plotpath,width = 8,height = 18)
}
#save_seperate_plot<-function(xdata=NULL,lxyz=c("only","gender","age"),tit="B-Social New Consent",plotpath=NULL,filename=NULL){
#  for (xyz in lxyz) {
#    datalist=get(paste0(xyz,"group"))
#    print(datalist$x1)
#    assign(paste0(xyz,"plot"),graph_data_meet(datalist = datalist, xdata = xdata, title = tit))
#    ggsave(plot = paste0(xyz,"plot"),filename = paste0(tit,xyz,"plot"),device = "jpeg",dpi = 300,path = plotpath,width = 8,height = 8)
#  }}
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

#plotting 

#save_seperate_plot(subreg,tit = 'Protect (all)',plotpath = "~/Documents/github/UPMC/data meeting/",filename = 'Protect(all)byGroup_age_gender.jpeg')

#newconsent<-subreg[which(as.Date(subreg$registration_consentdate)>startdate & !subreg$registration_group %in% c(88,89)),]
do_for_asub(newconsent,tit = "B-Social New Consent",plotpath=plotpath,filename="by_newconsent.jpeg")
nplot<-graph_data_meet(datalist = onlygroup,xdata = subreg,title = "New Consent")



###########end of the usable codes############


#for protect 
## data: all protect protocol, remove all termination because of ineligible (no need to consider how many data collected) 


## by arm: protect3 new consents; protect 3 followup consents; all others; all 
  # for each arm -- group by age50 or not: above age 50; all 
    # 1. realization of recruitment targets 
    #    line chart (x=month, y=# of enrollments, line1 = targets, line2 = actual) 
    # 2. enrollment by group 
      # for each age55? group -- 
         # group by group (ATT, HC. IDA, ...) in each graph
         # group by group, gender, age, race 

