# startup
rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
source('~/Documents/github/UPMC/startup.R')

#note: here uses 'QOL interview' as the 'training form'. 

# rctransfer.dataclean <- function(
# [variables]
#curdb = bsoc
protocol.cur <- ptcs$bsocial
bsoc<- bsrc.checkdatabase2()
#range
replace_w_na = FALSE
#) {

library(tidyverse)
library(chron)

#prepare functions
# make a fun to report abnormal values 
report_wrong <- function(id = NULL, which_var = NULL, wrong_val = NULL, which_form = NULL, comments = NA, 
                         report = wrong_val_report,rbind=T){
  report <<- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                        which_form=as.character(),comments=as.character(),stringsAsFactors = F) # initialize
  new_repo <- data.frame(id = id)
  new_repo[1:nrow(new_repo),2]<- which_var
  new_repo[1:nrow(new_repo),3]<- wrong_val
  new_repo[1:nrow(new_repo),4]<- which_form
  new_repo[1:nrow(new_repo),5]<- comments
  colnames(new_repo)<-c('id','var_name','wrong_val', 'which_form','comments')
  ifelse(rbind,return(rbind(report,new_repo)),return(new_repo))
}
#Grabs data from bsocial, everything that starts with x, minus the variable for complete
rd.var.map<-function(x){
  bsocnames<-c('id',names(bsoc$data[c(which(grepl(x,names(bsoc$data))))]))
  bsocnames[-which(grepl("complete$", bsocnames))]->bsocnames
  return(bsocnames)
}
#Gives value of 1 if not in range # TO BE CHANGED - MAKE A REPORT INSTEAD OF A COL 
qol.range<-function(range, tar_cols){for (i in 1:nrow(QOL_fresh)){
  if (any(sapply(QOL_fresh[i, tar_cols], function(x){
    !x %in% range & !is.na(x)
  }))){
    QOL_fresh$probs4[i]<-1} # TO BE GENERALIZED
  else{QOL_fresh$probs4[i]<-0}} # TO BE GENERALIZED
  return(QOL_fresh)}
#if not in range, changes the values to NA
qol.na<-function(range, tar_cols,df=QOL_fresh){for (i in 1:nrow(QOL_fresh)){
  QOL_fresh[i, tar_cols]<-
    sapply(QOL_fresh[i, tar_cols], function(x){
      ifelse (!x %in% range & !is.na(x) ,x<-NA,x<-x)})}
  return(QOL_fresh)}

# import data from access and match variables  # TO BE GENERALIZED 
QOL_raw <- read.csv(paste0(rootdir,"QOL_raw.csv"), stringsAsFactors = F) 
#rename the variables to something more reasonable (i.e. var names in redcap): 
QOL_fresh <- dplyr::select(QOL_raw, ID, #FOLOQOL, DATEQOL, 
                           TIME.BEGAN, QOLBA1:TIME.ENDED)
#get variables for qol
rd.var.map("qol")->qolvarmap
#change variable names to match redcap
names(QOL_fresh)<-qolvarmap[-c(18:23, 26, 77)]


## identify wrong values/datatypes, correct and report 
log_replace <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # initialize

#change data type 
# identify all non-integer/numeric col

#Report 999 AND if replace_w_na=T, replace change 999's to NA
log_replace<-rbind(log_replace,(do.call("rbind",apply(which(QOL_fresh==999,arr.ind = T),1,function(indeX){ # TO BE GENERALIZED
  report_wrong(report = log_replace, id=QOL_fresh[indeX[1],1],which_var = colnames(QOL_fresh)[indeX[2]],
               wrong_val = 999, which_form = 'QOL', rbind = F,
               comments = ifelse(replace_w_na,'Replaced with NA','Not replaced with NA yet'))
})))) # TO BE GENERALIZAED
if(replace_w_na){QOL_fresh[which(QOL_fresh==999)]<-NA}




#}



#### original codes   




#Range problems:
##Range problems for DT scale (1-7)
#which ones don't fit get probs=1
qol.range(range=c(1:7), c(3, 20:22, 32:35, 38, 39, 
                          44:46, 70:72, 78:80, 84:86, 88:91))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 3, 20:22, 32:35, 38, 39, 44:46, 70:72, 78:80, 84:86, 88:91)]->qolprobs
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs %>% gather(key="question", value="original",-registration_redcapid)->qolprobs
qolprobs[which(!qolprobs$original %in% c(1:7) & !is.na(qolprobs$original)),]->qolprobs
mutate(qolprobs, new=NA)->qolprobs
#Change the ones that don't fit to NA
qol.na(range=c(1:7), cols=c(3, 20:22, 32:35, 38, 39, 44:46, 70:72, 78:80, 84:86, 88:91))->QOL_fresh

##Range problems for living situations (1-16)
qol.range(range=c(1:16), c(4, 8, 10, 12, 14, 16))->QOL_fresh
#which ones don't fit (No range problems here)
QOL_fresh[which(QOL_fresh$probs==1),c(1, 4, 8, 10, 12, 14, 16)]->qolprobs2

##Range problems for YES/NO
qol.range(range=c(0:1,9), c(23:30, 47:60,65:69, 81:82))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 23:30, 47:60,65:69, 81:82)]->qolprobs3
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs3 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs3
qolprobs3[which(!qolprobs3$original %in% c(0:1) & !is.na(qolprobs3$original)),]->qolprobs3
mutate(qolprobs3, new=NA)->qolprobs3
#Change the ones that don't fit to NA
qol.na(range=c(1:7), cols=c(23:30, 47:60,65:69, 81:82))->QOL_fresh

##Range problems for 1:4 items
qol.range(range=c(1:4), c(31,64))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 31, 64)]->qolprobs4
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs4 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs4
qolprobs4[which(!qolprobs4$original %in% c(1:4) & !is.na(qolprobs4$original)),]->qolprobs4
mutate(qolprobs4, new=NA)->qolprobs4
#Change the ones that don't fit to NA
qol.na(range=c(1:4), cols=c(31,64))->QOL_fresh

##Range problems for 0:5 items
qol.range(range=c(0:5), c(36:37))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 36:37)]->qolprobs5
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs5 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs5
qolprobs5[which(!qolprobs5$original %in% c(0:5) & !is.na(qolprobs5$original)),]->qolprobs5
mutate(qolprobs5, new=NA)->qolprobs5
#Change the ones that don't fit to NA
qol.na(range=c(0:5), cols=c(36:37))->QOL_fresh

##Range problems for 1:5
qol.range(range=(1:5), c(40:43, 87))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 40:43, 87)]->qolprobs6
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs6 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs6
qolprobs6[which(!qolprobs6$original %in% c(1:5) & !is.na(qolprobs6$original)),]->qolprobs6
mutate(qolprobs6, new=NA)->qolprobs6
#Change the ones that don't fit to NA
qol.na(range=c(1:5), cols=c(40:43, 87))->QOL_fresh

##Range problems for 0:2- no issues
which(!QOL_fresh$qol_i_1 %in% c(1:5))


#Put all range problems together    
  qol.range.probs<-rbind(qolprobs, qolprobs3, qolprobs4, qolprobs5, qolprobs6)
    
#Check for duplicates: in the event that the same ID has two entries within a single follow-up, just take the earliest one
    any(duplicated(QOL_fresh$registration_redcapid))

#FIGURE OUT IDS LAST
    bsrc.findid(QOL_fresh,idmap = idmap,id.var = "registration_redcapid")->QOL_fresh
    if(any(!QOL_fresh$ifexist)){message("ERROR: NOT ALL IDS EXIST IN MASTER DEMO, PLEASE FIX. Here are their soloff ids:")
                print(QOL_fresh[which(!QOL_fresh$ifexist),"registration_redcapid"])}
#Figure out NAs    
    qol.remove.na<-function(cols){for (i in 1:nrow(QOL_fresh)){
        QOL_fresh[i, cols]<-
          sapply(QOL_fresh[i, cols], function(x){
          ifelse (is.na(x), x<-999, x<-x)})}
        return(QOL_fresh)}
    qol.remove.na(c(3, 4, 8, 10, 12, 14, 16, 19:60, 65:73, 78:82, 84:86, 88:91))->QOL_fresh  
    as.data.frame(names(QOL_fresh))->r
    
    
    
    
    
    
    