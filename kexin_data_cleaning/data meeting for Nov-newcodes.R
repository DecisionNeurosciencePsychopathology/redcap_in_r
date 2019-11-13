# setup 
setwd("~/Documents/github/UPMC/data meeting/")
source("~/Documents/github/UPMC/startup.R")

protocol.cur <- ptcs$masterdemo
md <- bsrc.checkdatabase2(online = T)

#get id 
MD<-bsrc.getform(formname = 'record_registration',curdb = md)
#ptcname: ksocial, bsocial,protect3,protect2,protect,suicid2,suicide,EMA,EXPLORE
varnames<-function(ptcname){
  id<-c('registration_redcapid', 'registration_demostatus','registration_group','registration_dob','registration_gender',
        paste0(c('registration_ptcstat___','reg_condate_','reg_term_yesno_','reg_term_reason_','reg_term_excl_'),ptcname))
  if(ptcname=='protect3'){return(c(id,'reg_status_protect3','reg_p3catchup'))
  }else if(ptcname=='protect2'){return(c(id,'reg_status_protect2'))
  }else if(ptcname=='protect'){return(c(id,'reg_status_protect'))
  }else if(ptcname=='suicid2'){return(c(id,'reg_status_suicid2'))
  }else if(ptcname=='suicide'){return(c(id,'reg_status_suicide'))
  }else if(ptcname=='EMA'){ptcname='bsocial';return(id)}
  else {return(id)}
}

#varnames<-list(#recuirtment = c('registration_recruisource','registration_recruitother'),
 #              ksocial=c('reg_condate_ksocial','reg_term_yesno_ksocial','reg_term_reason_ksocial'),
  #             bsocil=c('reg_condate_bsocial','reg_term_yesno_bsocial','reg_term_reason_bsocial'),
    #           protect3=c('reg_condate_protect3','reg_status_protect3','reg_p3catchup','reg_term_yesno_protect3','reg_term_reason_protect3'),
     #          protect2=c('reg_condate_protect2','reg_status_protect2','reg_term_yesno_protect2','reg_term_reason_protect2'),
      #         protect1=c('reg_condate_protect','reg_status_protect','reg_term_yesno_protect','reg_term_reason_protect'),
       #        suicid2=c('reg_condate_suicid2','reg_status_suicid2','reg_term_yesno_suicid2','reg_term_reason_suicid2'),
        #       suicid1=c('reg_condate_suicide','reg_status_suicide','reg_term_yesno_suicide','reg_term_reason_suicide'),
         #      EMA=c('reg_condate_bsocial','reg_term_yesno_bsocial','reg_term_reason_bsocial'),
          #     EXPLORE=c('reg_condate_explore','reg_term_yesno_explore','reg_term_reason_explore'))
#PROTECT - ALL
var_pt<-unique(c(varnames('protect3'),varnames('protect2'),varnames('protect'),varnames('suicide'),varnames('suicid2')))
all(var_pt %in% colnames(MD)) #should be TRUE
PT<-MD[,var_pt] 
PT<-PT[unique(which(PT[,grep('registration_ptcstat_',var_pt)]==1,arr.ind = T)[,1]),] # consented to at least one protocol 
#temp<-PT[unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # ieligible people 
PT<-PT[-unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # remove all ineligible people terminated for at least one protocol due to ineligibility  
PT<-PT[-unique(which(PT[,grep('reg_term_excl_',var_pt)]==1,arr.ind = T)[,1]),] # remove all unusable data based on 'reg_term_excl_

#####check#####
sum(duplicated(PT$registration_redcapid)) #should be 0. check ID duplicatets 
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
View(PT[which(PT$reg_term_excl_protect3==1 | PT$reg_term_excl_protect2==1 | PT$reg_term_excl_protect==1 | PT$reg_term_excl_suicid2==1 | PT$reg_term_excl_suicide==1),]) #18 people exluded from analysis 
##################
PT$'mincondate'<-zoo::as.Date(apply(PT[,grep('reg_condate_',var_pt)],1,function(x){min(as.Date(x),na.rm=T)}))
sum(is.na(PT$mincondate))# should be 0. no NA in 'mincondate'

#MAKE GRAPHS. subreg is the clean data that contains only participants that we want (and no duplicated rows). earliest consentdate is called: mincondate
subreg<-PT
curdb<-md
#PREPARE LISTS FOR PLOTTING 
#add Group to df 
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
do_for_asub(subreg,tit = 'Protect (all)',plotpath = "~/Documents/github/UPMC/data meeting/",filename = 'Protect(all)byGroup_age_gender.jpeg')
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

