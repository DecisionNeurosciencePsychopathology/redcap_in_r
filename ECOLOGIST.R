###
title: "Ecologist"
Author: "Jiazhou Chen"
Version: 0.2
###
#Version 0.2 Changelog:
  #bsrc.ema.main() gets updated parameters to better function and minimize confusion, adds on new upload function to send data
    #to redcap
  #bsrc.ema.redcapupload now fully function and redcap is updated to take in such data, auto updates redcap when pt is done
  #bsrc.ema.oneshotupload intergrates both for ease 
  #bsrc.ema.reshape function has no change

###Version 0.1
#Initial Version
#Functions:
  #bsrc.ema.main() process data file from MetricWire and produce progress report including graphic information
  #bsrc.ema.redcapupload upload such progress data and the data file
  #bsrc.ema.redcapreshape will change the data format for better use in R [intergrated with bsrc.getform]

#!!Not yet include micro burst calculation!!

bsrc.ema.main<-function(filename,path=NULL,forcerun.e=F,forceupdate.e=F,token.e=input.token,ifupload=F,uri.e=input.uri)
  {if (missing(filename)) {
  print("No file specified, please choose the target file")  
  filename<-file.choose()
  }
  ifrun<-bsrc.checkdatabase(forcerun = forcerun.e,token = token.e, forceupdate = forceupdate.e)
  if (ifrun){
    #Read EMA Data:
    emadata.raw<- read.csv(filename, stringsAsFactors=FALSE) #find function
    variname<-read.csv("variname.csv") #find variname
    variname<-as.character(variname$variname)
    names(emadata.raw)<-as.list(variname)
    
    #1st get RedCap ID [check funbsrc exists]:
    mwmatch<-data.frame(funbsrc$registration_redcapid,funbsrc$ema_studyidentifier)
    names(mwmatch)<-c('registration_redcapid','funbsrc$ema_studyidentifier')
    emadata.raw$RedcapID<-mwmatch$registration_redcapid[match(emadata.raw$User_Id,mwmatch$`funbsrc$ema_studyidentifier`)]
    lRedcapID<-unique(emadata.raw$RedcapID)
    linitial<-unique(subreg$registration_initials[match(lRedcapID,subreg$registration_redcapid)])
    
    #MAKE SURE TO CHECK REDCAP
    #Here is where you can do multiple ID processing loop: However, it might not be even useful bc individual files
    #Currently take out nas, should only be one item:
    RedcapID<-as.character(lRedcapID[1])
    Initial<-linitial[1]
    
    #process the date:
    d<-as.Date(emadata.raw$Survey_Submitted_Date,format = "%d/%m/%Y")
    emadata.raw$Survey_Submitted_Date<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))

    
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
    emadata<-na.locf(emadata)
    emadata$date<-as.Date(emadata$date)
    emadata$Total<-as.numeric(emadata$BoD)+as.numeric(emadata$DoD)+as.numeric(emadata$EoD)
    
    
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
    ematotal$Total<-as.numeric(ematotal$BoD)+as.numeric(ematotal$DoD)+as.numeric(ematotal$EoD)
    ematotal.melt<-melt(ematotal,id.var='date',variable.name="Type",value.name="expectation")
    
    #melt data
    emadata.full<-na.locf(merge(ematotal.donly,emadata,all = T))
    emadata.full.melt<-melt(emadata.full,id.var=c("redcapID","date"), measure.vars=c("BoD","DoD","EoD","Total"),variable.name="Type",value.name="actual")
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
    emamelt.merge<-emamelt.merge[which(!emamelt.merge$date>ifelse(enddate>Sys.Date(),Sys.Date(),enddate)),] 
    emamelt.merge$actual<-as.numeric(emamelt.merge$actual)
    emamelt.merge$expectation<-as.numeric(emamelt.merge$expectation)
    emamelt.merge$diff<-emamelt.merge$actual - emamelt.merge$expectation
    emamelt.merge$porp<-round(emamelt.merge$actual / emamelt.merge$expectation *100,2)
    emamelt.merge$per<-paste(emamelt.merge$porp, "%")
    
    
    if (ifupload) {redcap_upload_file_oneshot(file_name = filename,redcap_uri = uri.e,token = token.e,record = RedcapID, field = "emapg_fileupload", event = "ema_arm_1")}
    
    
    ##################################GRAPH#############################################
    #Percentage Plot
    emaplot.percent<-ggplot(data = emamelt.merge, aes(x=date, y=porp, group=Type, shape=Type, color=Type)) +
      ggtitle(paste(Initial,"EMA Progress (Percentage)"))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line()+
      ylab("Percentage")+
      scale_x_date(name="Date",limits = c(startdate+1,NA) ,date_breaks = "2 days")+
      geom_point()+
      geom_label_repel(data = emamelt.merge[(which(emamelt.merge$date %in% c(startdate+7,startdate+14,ifelse(enddate>Sys.Date(),Sys.Date(),enddate)))),], aes(x=date, y=porp,label=per))
    
    ggsave(paste(Initial,"_",Sys.Date(),"_EMAPro_PercentPlot.jpeg",sep = ""),device = "jpeg",plot = emaplot.percent,dpi = 300,path = path)
    print("Percentage Plot Saved to Working Directory")
    
    #Completion Plot
    emaplot.count<-ggplot(data = emamelt.merge, aes(x=date, y=actual, color=Type, group=Type, shape=Type)) +
      ggtitle(paste(Initial,"EMA Progress (Count)"))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line()+
      ylab("Percentage")+
      scale_x_date(name="Date",limits = c(startdate+1,NA) ,date_breaks = "2 days")+
      geom_point()+
      geom_label_repel(data = emamelt.merge[(which(emamelt.merge$date %in% c(startdate+7,startdate+14,ifelse(enddate>Sys.Date(),Sys.Date(),enddate)))),], aes(x=date, y=actual,label=actual))+
      geom_label_repel(data = emamelt.merge[(which(emamelt.merge$date %in% c(startdate+7,startdate+14,ifelse(enddate>Sys.Date(),Sys.Date(),enddate)) & emamelt.merge$Type %in% c("BoD","DoD","Total"))),], aes(x=date, y=expectation,label=expectation),color="black")
    
    ggsave(paste(Initial,"_",Sys.Date(),"_EMAPro_CountPlot.jpeg",sep = ""),device = "jpeg",plot = emaplot.count,dpi = 300,path = path)
    print("Completion (count) Plot Saved to Working Directory")
    return(emamelt.merge)
    }
}




bsrc.ema.redcapupload<-function(emamelt.merge=NULL,uri=input.uri,token=input.token, output=T,ifupload=T,curver="2"){
  emamelt.merge$check<-NA
  lengthofema<-21
  startdate<-as.Date(funbsrc$ema_setuptime[which(funbsrc$registration_redcapid==unique(emamelt.merge$redcapID) & funbsrc$ema_setuptime!="")])
  enddate<-startdate+lengthofema
  emamelt.merge$check[which(emamelt.merge$date %in% c(startdate+7))]<-"7Days"
  emamelt.merge$check[which(emamelt.merge$date %in% c(startdate+14))]<-"14Days"
  emamelt.merge$check[which(emamelt.merge$date %in% c(startdate+21))]<-"21Days"
  test1<-reshape(emamelt.merge[!is.na(emamelt.merge$check),],idvar = "check",timevar = "Type",direction = "wide", v.names = c("actual","per"),drop = c("date","porp","expectation","diff"))
  test2<-reshape(test1,idvar = "redcapID",timevar = "check",direction = "wide", v.names = names(test1)[-c(1,2)])
  
  test3<-test2
  names(test3)[1]<-"registration_redcapid"
  names(test3)[2:length(names(test3))]<-paste("emapg_",names(test3)[2:length(names(test3))],sep = "")
  names(test3)<-tolower(gsub("[.]","_",names(test3)))
  
      if(Sys.Date()<enddate+1) {
        test3$ema_completed___ip<-1
        currentexp<-paste("test3$ema_completed___",curver,"<-0", sep = "")
        eval(parse(text=currentexp))
        test3$ema_completed___999<-0
        
      }
      else {
      test3$ema_completed___ip<-0
      currentexp<-paste("test3$ema_completed___",curver,"<-1", sep = "")
      eval(parse(text=currentexp))
      test3$ema_completed___999<-0
      test3$ema_termdate<-enddate+1
      }
  test3$redcap_event_name<-"ema_arm_1"
  
  if (ifupload) {
  result.test3<-redcap_write(test3,token = input.token,redcap_uri = input.uri)
  if (result.test3$success) {print("DONE")}}
  
  if(Sys.Date()>enddate+1) {test2$finishdate<-enddate+1}
  if (output) {
  return(test3)}
}

bsrc.ema.oneshotupload<-function(filename.e,forceupdate.e=F,ifupload=T,curver.e=2){
  if (missing(filename.e)) {
  print("No file specified, please choose the target file")
  filename.c<-file.choose()}
  else {filename.e->filename.c}
  bsrc.ema.redcapupload(emamelt.merge = bsrc.ema.main(filename = filename.c, forceupdate.e = forceupdate.e, ifupload = T),ifupload = T,curver = curver.e)
}

bsrc.ema.redcapreshape<-function(){
  gsub("emaprog_","",names(LGER))
  melt.LGER<-cbind(melt.LGER,data.frame(t(as.data.frame(strsplit(melt.LGER$variable,split = "[_]")))))
  rownames(melt.LGER)<-NULL
  
}