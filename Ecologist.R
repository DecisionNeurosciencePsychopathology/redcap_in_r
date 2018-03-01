###
title: "Ecologist"
Author: "Jiazhou Chen"
###

#!!Not yet include micro burst calculation!!

#Read EMA Data:
emadata.raw<- read.csv("~/Box Sync/skinner/data/EMA/6733_J WOO_EMA.csv", stringsAsFactors=FALSE) #find function
variname<-read.csv("variname.csv") #find variname
variname<-as.character(variname$variname)
names(emadata.raw)<-as.list(variname)


#1st get RedCap ID [check funbsrc exists]:
mwmatch<-data.frame(funbsrc$registration_redcapid,funbsrc$ema_studyidentifier)
names(mwmatch)<-c('registration_redcapid','funbsrc$ema_studyidentifier')
emadata.raw$RedcapID<-mwmatch$registration_redcapid[match(emadata.raw$User_Id,mwmatch$`funbsrc$ema_studyidentifier`)]
lRedcapID<-unique(emadata.raw$RedcapID)
linitial<-unique(subreg$registration_initials[match(lRedcapID,subreg$registration_redcapid)])

#Here is where you can do multiple ID processing loop: However, it might not be even useful bc individual files
#Currently take out nas, should only be one item:
RedcapID<-lRedcapID[1]
Initial<-linitial[1]

#process the date:
d<-as.Date(emadata.raw$Survey_Submitted_Date,format = "%d/%m/%Y")
emadata.raw$Survey_Submitted_Date<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))

#Use datatable for graphic function:
table.emadata<-data.table(emadata.raw$RedcapID,emadata.raw$Survey_Submitted_Date,emadata.raw$TriggerName)
names(table.emadata)<-c("redcapID","date","Type")
table.emadata<-table.emadata[order(table.emadata$Type,table.emadata$date),]
table.emadata[,count:=seq_len(.N), by=Type]


#santatize the datatable for multiple participants, use for loop to loop through muiltiple participants
table.emadata<-na.omit(table.emadata)
table.emadata<-table.emadata[which(table.emadata$Type %in% c("DoD","BoD","EoD"))]

#Aggregate Total:
table.emadata$redcapID<-as.character(table.emadata$redcapID)
emadata<-aggregate(table.emadata,FUN = max,by=list(interaction(table.emadata$date,table.emadata$Type)))
emadata$Group.1<-NULL
emadata<-reshape(emadata,idvar = "date",timevar = "Type",direction = "wide", v.names = c("count"))
emadata<-emadata[order(emadata$date),]
names(emadata)<-c("redcapID","date","BoD","DoD","EoD")


#Generate Expectation Grid:
lengthofema<-21
startdate<-as.Date(funbsrc$ema_setuptime[which(funbsrc$registration_redcapid==unique(table.emadata$redcapID) & funbsrc$ema_setuptime!="")])
enddate<-startdate+lengthofema



emaseqdate<-seq.Date(from=startdate,to=enddate,by="days")
emaseq.one<-seq(from=0,to=lengthofema,length.out = length(emaseqdate))
emaseq.six<-seq(from=0,to=(6*lengthofema),length.out = length(emaseqdate))
emaseq.total<-seq(from=0,to=(8*lengthofema),length.out = length(emaseqdate))
ematotal.donly<-as.data.frame(emaseqdate)
names(ematotal.donly)<-c("date")
ematotal<-ematotal.donly
ematotal$BoD<-emaseq.one
ematotal$EoD<-emaseq.one
ematotal$DoD<-emaseq.six
ematotal.melt<-melt(ematotal,id.var='date',variable.name="Type",value.name="expectation")

#melt data
emadata.full<-na.locf(merge(ematotal.donly,emadata,all = T))
emadata.full.melt<-melt(emadata.full,id.var=c("redcapID","date"), measure.vars=c("BoD","DoD","EoD"),variable.name="Type",value.name="actual")
emadata.full.melt$date<-as.Date(emadata.full.melt$date)

#Merge
#emamerge<-merge(emadata,ematotal,all = T)
#emamerge<-emamerge[which(!emamerge$date==startdate),] #Take out startdate
#emamerge<-emamerge[which(!emamerge$date>enddate),]    #Take out after enddate
#emamerge<-na.locf(emamerge) #fill in NAs that are missing.
#emamerge.melt<-melt(emamerge,id.var=c("redcapID","date"),variable.name="Type",value.name="count",measure.vars=c("BoD","DoD","EoD")) 

#New Merge
emamelt.merge<-merge(emadata.full.melt,ematotal.melt,all=T)
emamelt.merge<-emamelt.merge[which(!emamelt.merge$date==startdate),] #Take out startdate
emamelt.merge<-emamelt.merge[which(!emamelt.merge$date>enddate),] 
emamelt.merge$actual<-as.numeric(emamelt.merge$actual)
emamelt.merge$expectation<-as.numeric(emamelt.merge$expectation)
emamelt.merge$diff<-emamelt.merge$actual - emamelt.merge$expectation
emamelt.merge$porp<-round(emamelt.merge$actual / emamelt.merge$expectation *100,2)
emamelt.merge$percentage<-paste(emamelt.merge$porp, "%")
emamelt.merge$Type<-as.factor(emamelt.merge$Type)

#Percentage Plot
emapercentplot<-ggplot(data = emamelt.merge, aes(x=date, y=porp, group=Type, shape=Type)) +
  ggtitle(paste(Initial," EMA Progress"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  ylab("Percentage")+
  scale_x_date(name="Date",limits = c(startdate+1,NA) ,date_breaks = "2 days")+
  geom_point()+
  geom_label(data = emamelt.merge[(which(emamelt.merge$date %in% c(startdate+7,startdate+14,startdate+21))),], aes(x=date, y=porp,label=percentage))

#Completion Plot
emapercentplot<-ggplot(data = emamelt.merge, aes(x=date, y=porp, color=Type)) +
  ggtitle("J WOO EMA Progress")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  ylab("Percentage")+
  scale_x_date(name="Date",limits = c(startdate+1,NA) ,date_breaks = "2 days")+
  geom_point()+
  geom_label(data = emamelt.merge[(which(emamelt.merge$date %in% c(startdate+7,startdate+14,startdate+21))),], aes(x=date, y=porp,label=percentage))

    
    
    
    