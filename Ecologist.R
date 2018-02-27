###
title: "Ecologist"
Author: "Jiazhou Chen"
###

#Read EMA Data:
emadata<- read.csv("~/Box Sync/skinner/data/EMA/6733_J WOO_EMA.csv", stringsAsFactors=FALSE) #find function
variname<-read.csv("variname.csv") #find variname
variname<-as.character(variname$variname)
names(emadata)<-as.list(variname)


#1st get RedCap ID [check funbsrc exists]:
emadata$RedcapID<-mwmatch$registration_redcapid[match(emadata$User_Id,mwmatch$`funbsrc$ema_studyidentifier`)]

#process the date:
d<-as.Date(emadata$Survey_Submitted_Date,format = "%d/%m/%Y")
emadata$Survey_Submitted_Date<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))

#Use datatable for graphic function:
table.emadata<-data.table(emadata$RedcapID,emadata$Survey_Submitted_Date,emadata$TriggerName)
names(table.emadata)<-c("redcapID","date","Type")
table.emadata<-table.emadata[order(table.emadata$Type,table.emadata$date),]
table.emadata[,count:=seq_len(.N), by=Type]


#santatize the datatable for multiple participants, use for loop to loop through muiltiple participants
table.emadata<-na.omit(table.emadata)
table.emadata<-table.emadata[which(table.emadata$Type %in% c("DoD","BoD","EoD"))]

#Aggregate Total:
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
ematotal<-as.data.frame(emaseqdate)
names(ematotal)<-c("date")
ematotal$BoD.expected<-emaseq.one
ematotal$EoD.expected<-emaseq.one
ematotal$DoD.expected<-emaseq.six
ematotal.melt<-melt(ematotal,id.var='date')
names(ematotal.melt)<-c("date","Type","count")


#Merge
emamerge<-merge(emadata,ematotal,all = T)
emamerge<-emamerge[which(!emamerge$date==startdate),] #Take out startdate
emamerge<-emamerge[which(!emamerge$date>enddate),]    #Take out after enddate





ggplot() +
  ggtitle("J WOO EMA Progress")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(data = table.emadata, aes(x=date, y=count, color=Type)) +
  scale_color_manual(values=c('red', 'brown','blue',"pink","gray","navy"))+
  geom_line(data = ematotal.melt, aes(x=date,y=count, color=Type))+
  geom_label(data = table.emadata[(which(table.emadata$date %in% c(startdate+7,startdate+14,startdate+21) & table.emadata$Type %in% c("BoD"))),], aes(x=date, y=count,label=round(count)))+
  geom_label(data = table.emadata[max(which(table.emadata$date %in% c(startdate+7,startdate+14,startdate+21) & table.emadata$Type %in% c("DoD"))),], aes(x=date, y=count,label=round(count)))
