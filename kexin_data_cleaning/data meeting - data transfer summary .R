# setup 
setwd("~/Documents/github/UPMC/data meeting/")
source("~/Documents/github/UPMC/startup.R")

protocol.cur <- ptcs$masterdemo
md <- bsrc.checkdatabase2(online = T)
#plotpath="/Users/mogoverde/Desktop" #Morgan WFH
#plotpath="C:/Users/buerkem/OneDrive - UPMC/Desktop" #Morgan Work
plotpath="~/Documents/github/UPMC/data meeting" #Kexin

#PT3 recruitment milestone
recr_pt3<-read.csv("~/Box/skinner/administrative/Data meeting/Recruitment milestones for MH085651 - Decision Process of Late-Life Suicide_new.csv",stringsAsFactors = F)

#get id 
MD<-bsrc.getform(formname = 'record_registration',curdb = md)

#ptcname: ksocial, bsocial,protect3,protect2,protect,suicid2,suicide,EMA,EXPLORE
#Grabs the variable names based on protocol
varnames<-function(ptcname){
  id<-c('registration_redcapid', 'registration_wpicid', 'registration_demostatus','registration_group','registration_dob','registration_gender','registration_recruisource',
        paste0(c('registration_ptcstat___','reg_condate_','reg_term_yesno_','reg_term_reason_','reg_term_excl_'),ptcname))
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

#Checks1
all(var_pt %in% colnames(MD)) #should be TRUE

#Add master demo info
PT<-MD[,var_pt]
codcol<-bsrc.getform(formname = "cause_of_death",curdb = md)[c("raw_a",'cod_dead','cod_primcause','cod_pi_mod')];colnames(codcol)[1]<-"registration_redcapid"
PT<-dplyr::full_join(PT,codcol,by="registration_redcapid")
##More checks of each protocol
#Protect checks
PT<-PT[unique(which(PT[,grep('registration_ptcstat_',var_pt)]==1,arr.ind = T)[,1]),] # consented to at least one protocol 
any(is.na(PT$registration_redcapid)) #FALSE
any(duplicated(PT$registration_redcapid)) #FALSE
#temp<-PT[unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # ieligible people 

# Get a table of the number of terminated people in each protocol 
term_table<-data.frame(Protocal=c("Completed study","Lost to follow-up","Ineligible after signing consent","Withdrawn at own/family request","Withdrawn by PI d/t other reasons (death, unreliability, etc.)"),stringsAsFactors = F)
for (ptversion in c("suicide","suicid2","protect","protect2")){
  print(any(duplicated(PT$registration_redcapid))); print(table(PT[paste0("reg_term_reason_",ptversion)]));print(sum(PT[[paste0("reg_term_excl_",ptversion)]],na.rm = T))
  term_table<-data.frame(term_table,newcol=as.vector(table(PT[paste0("reg_term_reason_",ptversion)])),stringsAsFactors = F)
  colnames(term_table)[ncol(term_table)]<-ptversion
  term_table
}
#speical protect3
ptversion="protect3"
print(any(duplicated(PT$registration_redcapid))); print(table(PT[paste0("reg_term_reason_",ptversion)])); print(sum(PT[[paste0("reg_term_excl_",ptversion)]],na.rm = T))
term_table<-data.frame(term_table,newcol=c(0,0,as.vector(table(PT[paste0("reg_term_reason_",ptversion)]))),stringsAsFactors = F)
colnames(term_table)[ncol(term_table)]<-ptversion
term_table<-rbind(term_table,c("Total",colSums(term_table[-1])))
term_table

# cause of death 
PT<-PT[-unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),]  # Remove people terminated for at least one protocol due to ineligibility 
deadPT<-



PT<-PT[-unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # remove all ineligible people terminated for at least one protocol due to ineligibility  
PT<-PT[-unique(which(PT[,grep('reg_term_excl_',var_pt)]==1,arr.ind = T)[,1]),] # remove all unusable data based on 'reg_term_excl_

#KSOCIAL checks (must be in KSOCIAL, can also be in BSOCIAL)
K[which(K$registration_ptcstat___ksocial==1 & !is.na(K$registration_ptcstat___ksocial)),]->K
K[which(K$reg_term_reason_ksocial!=3 | is.na(K$reg_term_reason_ksocial)),]->K
K[which(K$reg_term_excl_ksocial!=1 | is.na(K$reg_term_excl_ksocial)),]->K

#BSOCIAL checks
BS[which(BS$registration_ptcstat___bsocial==1 & !is.na(BS$registration_ptcstat___bsocial)),]->BS
BS[which(BS$reg_term_reason_bsocial!=3 | is.na(BS$reg_term_reason_bsocial)),]->BS
BS[which(BS$reg_term_excl_bsocial!=1 | is.na(BS$reg_term_excl_bsocial)),]->BS
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

##P3 all
sum(duplicated(allP3$registration_redcapid)) #should be 0. check ID duplicates 
sum(allP3$registration_group==89) #should be 0. No 89 in 'group'
sum(is.na(allP3$registration_redcapid)) # should be 0. no NA in ID 
sum(is.na(allP3$registration_group)) # should be 0. no NA in 'group'
sum(is.na(allP3$registration_dob))# should be 0. no NA in 'dob'

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
sum(is.na(BS$registration_group)) # =163
BS[which(is.na(BS$registration_group)),"registration_redcapid"] 
#Remove ineligible
sum(BS$registration_group==89 & !is.na(BS$registration_group==89)) # =23
BS[-which(BS$registration_group==89 & !is.na(BS$registration_group==89)),]->BS
#One person with missing dob, new person, remove (no initials or ANYTHING)
sum(is.na(BS$registration_dob)) 
#BS[-which(is.na(BS$registration_dob)),]->BS

################## plot ################
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
  groupmap$choice.string<-gsub("Healthy control","1Healthy control",groupmap$choice.string)
  groupmap$choice.string<-gsub("Depressed Control","2Depressed Control",groupmap$choice.string)
  groupmap$choice.string<-gsub("Ideator","3Ideator",groupmap$choice.string)
  groupmap$choice.string<-gsub("Non-Suicidal Patient (BPD NON-ATT)","5Non-Suicidal Patient (BPD NON-ATT)",groupmap$choice.string)
  subreg$`Group`<- plyr::mapvalues(x = subreg$registration_group, from = groupmap$choice.code, to = as.character(groupmap$choice.string),warn_missing = T)
  if(any(subreg$'Group'=="Non-Suicidal Patient (BPD NON-ATT)")){
    subreg[which(subreg$'Group'=="Attempter"),"Group"]<-subreg[which(subreg$'Group'=="Attempter"),"registration_lethality"]}
  onlygroup<-list(x1="Group",x2=NULL)
  #Gender
  subreg$Gender<-subreg$registration_gender
  gendergroup<-list(x1="Gender",x2="Group")
  #Age
  subreg$ageyrs<-lubridate::as.period(lubridate::interval(as.Date(subreg$registration_dob),as.Date(subreg$mincondate)))$year
  agemap<-data.frame(yrlable=c("18-25","26-30","31-40","41-50","51-60","61-70","70+"),yrstar=c(0,26,31,41,51,61,71))
  subreg$`Age`<-as.character(agemap$yrlable[findInterval(subreg$ageyrs,agemap$yrstar)])
  agegroup<-list(x1="Age",x2="Group")
  
  onlygroup<<-onlygroup
  #return(onlygroup)
  gendergroup<<-gendergroup
  #return(gendergroup)
  agegroup<<-agegroup
  #return(agegroup)
  return(subreg)
}

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
    ggtitle(paste(title,"by",datalist$x1,datalist$x2)) +
    xlab(datalist$x2) + ylab("Frequency Count")+
    scale_fill_brewer(palette="OrRd")
  #+scale_fill_discrete(name = datalist$x1)
  plotx$labels$fill <- datalist$x1
  return(plotx)
}

#Protect data frame use
#Protect all
#Plots
plot_prep(PT,md)->Pall
#do_for_asub(Pall,tit = 'Protect (all)',plotpath = plotpath,filename = 'Protect(all)byGroup_age_gender.jpeg')
Pall[-which(Pall$ageyrs<50),]->Pall2
do_for_asub(Pall2,tit = 'Protect (all)',plotpath = plotpath,filename = 'Protect(all)byGroup_age_gender.jpeg')
#Get EXPLORE scanned
#Get files from box
#root="/Users/mogoverde/Box/skinner/data/" #Morgan 
root="~/Box/skinner/data/" #Kexin
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
bsrc.findid(Bspottid,idmap = idmap,id.var = "ID")->Bspottmddf
Bspottmddf[which(!Bspottmddf$ifexist),] #Should be no one
#Plotting and final df
Ball[which(Ball$registration_redcapid %in% Bmrimddf$masterdemoid),]->Bmriall
sum(is.na(Bmriall$group))
do_for_asub(Bmriall,tit = 'BSOCIAL MRI',plotpath = plotpath,
            filename = 'BsocialmribyGroup_age_gender.jpeg')
Ball[which(Ball$registration_redcapid %in% Bspottmddf$masterdemoid),]->Bspottall
sum(is.na(Bspottall$group))
do_for_asub(Bspottall,tit = 'BSOCIAL SPOTT',plotpath = plotpath,
            filename = 'BsocialspottbyGroup_age_gender.jpeg')
#Get people in EMA
bsrc.attachngrab(rdpaths$ema)->ema
ema$fulldata.ema$info->emainfo
emainfo$RedcapID<-as.character(emainfo$RedcapID)
emainfo<-bsrc.findid(emainfo,idmap=idmap,id.var = "RedcapID")
emainfo$RedcapID
table(emainfo$Status)
#Check IDs
emainfo[which(!emainfo$RedcapID %in% Ball$registration_redcapid),"RedcapID"]
#Remove one person for being ineligible
emainfo[-which(emainfo$RedcapID=="440057"),]->emainfo
#Plotting and df
Ball[which(Ball$registration_redcapid %in% emainfo$RedcapID),]->Bemaall
sum(is.na(Bemaall$group))
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
Kmriall2<-subset(Kmriall,ageyrs>=25)
do_for_asub(Kmriall2,tit = 'KSOCIAL(over 25 yrs old)',plotpath = plotpath,
            filename = 'KsocialbyGroup_age_gender2.jpeg')

#Numbers output functions:
#Calculate ratio of K to B pts
sum(table(unique(Kmriall2$registration_redcapid)))==nrow(Kmriall2) #Should be T, checks if duplicated IDs
message("Ratio of K participants to BSOCIAL:",sum(is.na(Kmriall2$registration_ptcstat___bsocial)),"/",nrow(Kmriall2))

# recruitment graph for pt3 
#Only P3
#P3<-Pall[which(!is.na(Pall$registration_ptcstat___protect3) & Pall$registration_ptcstat___protect3==1),] #get P3 only (eligible people)
plot_prep(allP3,md)->P3all
newP3<-subset(P3all,reg_p3catchup==0) # get new P3
#Number of P3 not carried over from P2
sum(P3all$reg_p3catchup=="0",na.rm = T)
P3all[which(P3all$reg_p3catchup=="P2"),c("registration_redcapid","ageyrs")]
sum(P3all$ageyrs>=55)

recr_pt3[1]<-as.Date(recr_pt3[[1]])

newconsent<-newP3
newconsent$date<-as.Date(newconsent$mincondate)
sortedpos<-sort.int(newconsent$date,index.return=T)$ix 
newconsent<-newconsent[sortedpos,] #order newP3 by consent date 
newconsent$Actual<-1:length(newconsent$date) # get the acual number of participants by date 
newconsent$month<-month(newconsent$date)
newconsent$year<-year(newconsent$date)
newconsentdate<-newconsent[which(!duplicated(newconsent$date,fromLast=T)),] # the "actual" of the last row of each day is the actual number of new participants in each date so only keep the last row of each date
newconsentmonth<-newconsent[which(!duplicated(newconsent$month,fromLast=T)),] #the "actual" of the last row of each month is the actual number of new participants in each month so only keep the last row of each month
subconsendate<-data.frame(date=newconsentdate$date,Actual=newconsentdate$Actual) #get only the date and actual number 
subconsenmonth<- data.frame(date=newconsentmonth$date,Actual=newconsentmonth$Actual) #get only the year, month and actual number 

targetrecrui1yr<-recr_pt3[1:4,c(1,3)] # the projection until Apr 2020 
targetrecruiall<-recr_pt3[,c(1,3)] # the projection - all 
names(targetrecrui1yr)<-c("date","Projection")
names(targetrecruiall)<-c("date","Projection")

#8 months until Apr 2020 
result1yr<-dplyr::full_join(subconsen,targetrecrui1yr,by="date") #merge actual and projection
result1yr<-na.omit(reshape2::melt(result1yr,id.vars="date",variable.name="Number Type",value.name="count"))

#recrui_1yr<-
ggplot(result1yr, aes(x=date, y=count, label=count, color=`Number Type`)) +
  ggtitle("New Protect3 Participant and Projections: 08/2019 - 08/2020")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, vjust=.5))+
  geom_line() +
  geom_point(data = result1yr[which(result1yr$`Number Type` == "Actual"),])+
  scale_color_manual(values=c('red', 'gray')) +
  xlab("Month") + 
  ylab("Number of Participants") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limits = c(as.Date("2019-07-01"),targetrecrui1yr$date[nrow(targetrecrui1yr)])) +
  geom_text(data = result1yr[which(result1yr$`Number Type` == "Actual"),], aes(label=count),vjust=-1)+
  geom_label(data = result1yr[which(result1yr$`Number Type` == "Projection"),], aes(label=count))
# projection until 2024 
resultall<-dplyr::full_join(subconsenmonth,targetrecruiall,by="date") #merge actual and projection
resultall<-na.omit(reshape2::melt(resultall,id.vars="date",variable.name="Number Type",value.name="count"))
recrui_all<- ggplot(resultall, aes(x=date, y=count, label=count, color=`Number Type`)) +
  ggtitle("New Protect3 Participant and Projections")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, vjust=.5))+
  geom_line() +
  geom_point(data = resultall[which(resultall$`Number Type` == "Actual"),])+
  scale_color_manual(values=c('red', 'gray')) +
  xlab("Month") + 
  ylab("Number of Participants") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limits = c(as.Date("2019-07-01"),targetrecruiall$date[nrow(targetrecruiall)])) +
  geom_text(data = resultall[which(resultall$`Number Type` == "Actual"),], aes(label=count),vjust=-1)+
  geom_label(data = resultall[which(resultall$`Number Type` == "Projection"),], aes(label=count))

#testplot<-recr_pt3[c(1,3)]
#names(testplot)<-c("pjdate","Projection")
#ggplot(testplot,aes(x=pj_date,y=Projection,label=Projection))

######################################################################

root="~/Box/skinner/data/" #Kexin
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
#Plotting and final df

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
Kmri #kmri (k)

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
  write.csv(result,paste0("~/Documents/github/UPMC/data meeting/Counts_Scanning_for_Mandy_",dfname,".csv"))
}

#get mean and sd of age 
for (dfname in c("explore","bmri","emaa")){
  df<-get(dfname)
  library(lubridate)
  df$Age<-as.integer((Sys.Date()-as.Date(df$registration_dob))/365)
  print(dfname)
  print(class(df$Age))
  print(df%>% group_by(Group) %>% summarise(mean=mean(Age,na.rm = T),sd=(sd(Age,na.rm = T))))
  print(mean(df$Age))
  print(sd(df$Age))}


