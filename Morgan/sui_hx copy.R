#root
root="C:/Users/buerkem/Box/skinner/data/Redcap Transfer/suicide hx/"
#libraries
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(na.tools) 
#Attempt characteristics
    #Negative outcomes form
      #grab form
      negout<-read.csv(file = paste0(root, "A_NEGOUT.csv"))             
      negout[c(1,2,20:23)]->negout
      #fix dates
      as.Date(mdy(negout$CDATE))->negout$CDATE
      as.Date(mdy(negout$ATTDATE))->negout$ATTDATE
      as.Date(mdy(negout$ATTDATE2))->negout$ATTDATE2
      as.Date(mdy(negout$ATTDATE3))->negout$ATTDATE3
      as.Date(mdy(negout$ATTDATE4))->negout$ATTDATE4
      #Make one column with all atts and dates
      gather(negout,key="ATT",value="DATE", -ID,-CDATE)->negout2
      #date fix again  
      as.Date(negout2$DATE)->negout2$DATE
      #remove non-attempts
      negout2[-which(is.na(negout2$DATE)),]->negout2
      #Grab lethalities form
      leths<-read.csv(file = paste0(root, "S_LETH.csv"))
      #Remove 99's
      for (i in 1:nrow(leths[c(11,13, 15:21)])){
      leths[i,c(11,13, 15:21)]<-sapply(leths[i,c(11,13, 15:21) ], function(x){
      ifelse (x==99, x<-NA,x<-x)})}
      #Grab max leth (only one we care about)
      apply(leths[c(11,13, 15:21)], 1, function(x){max(x, na.rm=T)})->leths$max_leth
      #If infinity, all were NA
      leths[which(leths$max_leth==-Inf),"max_leth"]<-98
      #Only non-aborted attempts
      leths[which(leths$ABORT==1),"max_leth"]<-99
      #Fix CDATE
      as.Date(mdy(leths$CDATE))->leths$CDATE
      #remove unwanted vars
      leths[c(1,2,22)]->leths2
      #make new variable, id and cdate
      negout2 %>% mutate(newvar=paste0(ID, CDATE))->negout2
      leths %>% mutate(newvar=paste0(ID, CDATE))->leths
      #combine based on newvar
      leths$max_leth[match(negout2$newvar, leths$newvar)]->negout2$max_leth
      #Two people's CDATEs were off by 1 day, add in manually
      negout2[which(negout2$ID=="114567"),"max_leth"]<-4
      negout2[which(negout2$ID=="114572"),"max_leth"]<-8
      #Remove 99's, these are aborted attempts
      negout2[-which(negout2$max_leth==99),]->negout2
      #remove unwanted variables
      negout2[c(1,4,6)]->negout3
      names(negout3)[3]<-"Lethality"
    #Suicide questions form
      #Grab form
      suique<-read.csv(file = paste0(root, "S_SQUEST.csv"))
      suique[c(1,2,16,18,20,22,24,25,27,28,30,31,33,34,36,37)]->suique
      #fix dates
      as.Date(mdy(suique$CDATE))->suique$CDATE
      as.Date(mdy(suique$MRATTMPT))->suique$MRATTMPT
      as.Date(mdy(suique$MLDATE))->suique$MLDATE
      as.Date(mdy(suique$DATE1))->suique$DATE1
      as.Date(mdy(suique$DATE2))->suique$DATE2
      as.Date(mdy(suique$DATE3))->suique$DATE3
      as.Date(mdy(suique$DATE4))->suique$DATE4
      as.Date(mdy(suique$DATE5))->suique$DATE5
      #put dates with lethalities
        suique[c(1,3,4)]->suiatt1
        names(suiatt1)[c(2,3)]<-c("DATE","Lethality")
        suique[c(1,5:6)]->suiatt2
        names(suiatt2)[c(2,3)]<-c("DATE","Lethality")
        suique[c(1,7,9,11,13,15)]->suique
        gather(suique,key="ATT",value="DATE", -ID)->suique
        suique[-2]->suique
        na.rm(suique)->suique
        full_join(suique,suiatt1)->suiques
        full_join(suiques,suiatt2)->suiques
        suiques[which(!is.na(suiques$DATE)),]->suiques
      #Put together
        rbind(suiques, negout3)->atts
      
      

        
        
      