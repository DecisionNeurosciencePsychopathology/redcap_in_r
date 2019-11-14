library(tidyverse)
library(chron)
#Where to find data
  rootdir="C:/Users/buerkem/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
#Jiazhou's functions
  startup()
  md<-bsrc.checkdatabase2(ptcs$masterdemo, batch_size=200L)
  idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
  names(idmap)<-c("masterdemoid","wpicid","soloffid")
#Functions
  #Grabs data from bsocial, everything that starts with x, minus the variable for complete
  rd.var.map<-function(x){
    names(bsoc$data[c("registration_redcapid",names(bsoc$data)[which(grepl(x,names(bsoc$data)))])])->bsocnames
    bsocnames[-which(grepl("complete$", bsocnames))]->bsocnames
    return(bsocnames)
    }
##QOL interview
  #Functions
  #Gives value of 1 if not in range
  qol.range<-function(range, cols){for (i in 1:nrow(QOL_fresh)){
        if (any(sapply(QOL_fresh[i, cols], function(x){
          !x %in% range & !is.na(x)
          }))){
          QOL_fresh$probs[i]<-1}
        else{QOL_fresh$probs[i]<-0}} 
        return(QOL_fresh)}
  #Changes these values to NA
  qol.na<-function(range, cols){for (i in 1:nrow(QOL_fresh)){
        QOL_fresh[i, cols]<-
          sapply(QOL_fresh[i, cols], function(x){
          ifelse (!x %in% range & !is.na(x) ,x<-NA,x<-x)})}
        return(QOL_fresh)}
  #Get form
  QOL_raw <- read.csv(paste0(rootdir,"QOL_raw.csv"))
  #rename the variables to something more reasonable:
  QOL_fresh <- select(QOL_raw, ID, #FOLOQOL, DATEQOL, 
                      TIME.BEGAN, QOLBA1:TIME.ENDED)
  #get redcap names for each form
  bsoc<-bsrc.checkdatabase2(ptcs$bsocial, batch_size=200L)
  #get variables for qol
  rd.var.map("qol")->qolvarmap
  #change variable names to match redcap
  names(QOL_fresh)<-qolvarmap[-c(18:23, 26, 77)]
  as.character(QOL_fresh$qol_startdate)->QOL_fresh$qol_startdate
  as.character(QOL_fresh$qol_endtime)->QOL_fresh$qol_endtime
  as.character(QOL_fresh$qol_b_1_os)->QOL_fresh$qol_b_1_os
  as.character(QOL_fresh$qol_b_2_a_des)->QOL_fresh$qol_b_2_a_des
  as.character(QOL_fresh$qol_b_2_b_des)->QOL_fresh$qol_b_2_b_des
  as.character(QOL_fresh$qol_b_2_c_des)->QOL_fresh$qol_b_2_c_des
  as.character(QOL_fresh$qol_b_2_d_des)->QOL_fresh$qol_b_2_d_des
  as.character(QOL_fresh$qol_b_2_e_des)->QOL_fresh$qol_b_2_e_des
  as.character(QOL_fresh$qol_b_2_f_des)->QOL_fresh$qol_b_2_f_des
  as.character(QOL_fresh$qol_f_1_others)->QOL_fresh$qol_f_1_others
  as.character(QOL_fresh$qol_g_2)->QOL_fresh$qol_g_2
  
  #Change 999's to NA
  for (i in 1:nrow(QOL_fresh)){
    QOL_fresh[i,]<-
      sapply(QOL_fresh[i, ], function(x){
      ifelse (x==999, x<-NA,x<-x)})}
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
    
    

    
    
    