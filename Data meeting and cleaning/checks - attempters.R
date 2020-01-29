# setup 
setwd("~/Documents/github/UPMC/TRANSFER/checks/for data meeting")
source("~/Documents/github/UPMC/startup.R")
# # of atttemps missing lethality scores 
suihis<-bsrc.getform(formname = "suicide_history",protocol = ptcs$protect,curdb=md); colnames(suihis)[1]<-"registration_redcapid"
regis<-bsrc.getform(formname = "record_registration",curdb = md)

any(duplicated(suihis$registration_redcapid))|any(is.na(suihis$registration_redcapid)) #false
any(duplicated(regis$registration_redcapid)) #false
#regis$mincondate<-sapply(1:nrow(regis),function(x){min(lubridate::ymd(subset(regis,select=grep("reg_condate",colnames(regis)))[x,]),na.rm = T)}) #13 has no condate at all 
#all(paste0("registration_ptcstat___",c("suicide","suicid2","protect","protect2","protect2")) %in% colnames(regis))
regis_pt<-regis[which(rowSums(regis[paste0("registration_ptcstat___",c("suicide","suicid2","protect","protect2","protect2"))],na.rm = T)>0),]
regis_pt$mincondate<-sapply(1:nrow(regis_pt),function(x){min(lubridate::ymd(subset(regis_pt,select=grep("reg_condate",colnames(regis)))[x,]),na.rm = T)}) #13 has no condate at all 
suihis<-dplyr::left_join(regis_pt[c("registration_redcapid","registration_group","mincondate")],suihis,by="registration_redcapid")
sum(is.na(suihis$registration_group)) #0 no group 

# group ATT 226rows
att<-subset(suihis,registration_group=="ATT") 
# people who answered yes: Have you ever attempted suicide? 308 
answeryes<-subset(suihis,sahx_sa==1) #4
sum(answeryes$registration_group=="ATT",na.rm = T) # 4 in group ATT

# people who have positive number of attempts 189
attplus0<-subset(suihis,sahx_attemptnum>0) 
sum(attplus0$sahx_sa==1,na.rm = T) #4 answered yes 
sum(is.na(attplus0$sahx_sa)) # 185 no answer, 1 "88"
#View(attplus0[which(attplus0$sahx_sa==0),]) #220756 ansered no 

sum(attplus0$registration_group=="ATT",na.rm = T) # 188 in group ATT
sum(is.na(attplus0$registration_group)) # 0 has no group 

# here I use poeple who have more than 0 attempts to count attempters 
ind_col<-sapply(c("sahx_sadate","sahx_lr_"),function(x){grep(x,colnames(attplus0))}) # rownumber: attempts, col1:date, col2: leth scale
attempters<-subset(attplus0,select = c(unlist(lapply(c("registration_redcapid","registration_group","mincondate"),function(x){grep(x,colnames(attplus0))})),as.vector(t(ind_col)))) 
col_date<-seq(2,ncol(attempters)-1,2)[-1] # col# of dates
col_ls<-seq(1,ncol(attempters),2)[-c(1,2)] # col# of ls

#check if dates are all reasonable. convert to string first because the last five coldate data types are logical
newcolname<-c(colnames(attempters),paste0("DaysToToday_at",1:30),paste0("DaysFromConsent_at",1:30))
attoday<-t(sapply(1:nrow(attempters),function(x){difftime(Sys.Date(),lubridate::ymd(as.character(attempters[x,col_date])),units = "days")}))# today - attemptdate
conat<-t(sapply(1:nrow(attempters),function(x){difftime(lubridate::as_date(attempters$mincondate[x]),lubridate::ymd(as.character(attempters[x,col_date])),units = "days")})) # attemptdate - condate
any(attoday<0,na.rm = T)
attempters<-cbind(attempters,attoday,conat)
colnames(attempters)<-newcolname

attempters$att_num<-rowSums(!is.na(attempters[col_date])) # number of attempts
attempters$missingls_num<-attempters$att_num - rowSums(!is.na(attempters[col_ls])) # number of missing ls

forgraph<-attempters[c("registration_redcapid","att_num","missingls_num")]
forgraph$attlabel<-forgraph$att_num
forgraph$attlabel[which(forgraph$att_num>4)]<-"more than 4 attempts"
table(forgraph$att_num)
tableforgraph<-table(forgraph$attlabel)
library(ggplot2)
ggplot(forgraph, aes(x=attlabel)) +
  geom_histogram(stat="count")+
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + 
  labs(title="Distribution of attempts by participants in Protect",x="Number of attempts")

forpie<-as.data.frame(table(forgraph$missingls_num))
colnames(forpie)<-c("Number of missing LS","Attepters missing LS")
forpie
bp<- ggplot(forpie, aes(x="counts", y=`Attepters missing LS`, fill=`Number of missing LS`))+
  geom_bar(width = 1, stat = "identity")+
  geom_text(label = forpie$`Attepters missing LS`, size=5)
bp
pie <- bp + coord_polar("y", start=0)
pie



attempters[grep("at3$",colnames(attempters))]


class(unlist(attempters[1,col_date]))
as.Date(attempters[1,col_date])
