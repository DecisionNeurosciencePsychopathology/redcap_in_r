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
PT$group<-plyr::mapvalues(PT$registration_group,from = c("ATT","DEP","HC","IDE",88,89), to = c("4ATT","2DEP","1HC","3IDE","5Not sure yet","6Ineligible"))
any(is.na(PT$registration_redcapid)) #FALSE
any(duplicated(PT$registration_redcapid)) #FALSE
#temp<-PT[unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),] # ieligible people 

library(tidyr)
# Get a table of the number of terminated people in each protocol 
gettable<-function(field,variable,valuemapfrom,valuemapto,ptversion){
  df0<-subset(PT,eval(parse(text = (paste0("registration_ptcstat___",ptversion))))==1)
  if(all(is.na(df0[variable]))){warning(paste(ptversion,field,variable,"has no values."))
  }else{
    df<-as.data.frame(table(df0[c(variable,'group')]))%>%
      pivot_wider(names_from = group,values_from = Freq)
    df<-as.data.frame(df)
    df[1]<-plyr::mapvalues(df[[1]],from = valuemapfrom,to = valuemapto,warn_missing = F)
    colnames(df)[1]<-field
    return(df)}
}
for (ptversion in c("suicide","suicid2","protect","protect2","protect3")){
  print(any(duplicated(PT$registration_redcapid)))
  field<-c("term","mod","recurisour")
  variables<-c(paste0("reg_term_reason_",ptversion),"cod_pi_mod","registration_recruisource")
  valuemapfrom<-list(c(1:5),c(1:7),c(1:12,998,999))
  valuemapto<-list(c("Completed study","Lost to follow-up","Ineligible after signing consent","Withdrawn at own/family request","Withdrawn by PI d/t other reasons (death, unreliability, etc.)"),
                   c("Natural","Suicide","Accident","Homicide","Pending","Could not determine","Suspected Suicide"),
                   c("Involuntary Inpatient","Voluntary Inpatient","Outpatient","Bus Ad","Radio Ad","Magazine / Newspaper","Pitt+Me / CTSI database","Clinic/PI/Affiliation Referral","Flyers / Posters","Word of mouth","Fairs / Conventions","Private Practice Referral","Other","N/A"))
  for (i in 1:3){
    assign(paste0(field[i],"_",ptversion),
           gettable(field = field[i],variable = variables[i],valuemapfrom = valuemapfrom[[i]],valuemapto = valuemapto[[i]], ptversion = ptversion))}
}

# cause of death 
PT<-PT[-unique(which(PT[,grep('reg_term_reason_',var_pt)]==3,arr.ind = T)[,1]),]  # Remove people terminated for at least one protocol due to ineligibility 
deadPT<-subset(PT,cod_dead==1)
table(PT[c("cod_pi_mod","registration_group")])




