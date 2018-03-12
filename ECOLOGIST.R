###
title: "Ecologist"
Author: "Jiazhou Chen"
Version: 0.4
###
#Version 0.4 Changelog:
  #Revision for bsrc.ema.patch() for more detials on MB
  #Revisions on bsrc.ema.getevent(), bsrc.ema.main() and bsrc.ema.getfile() to intergrate bsrc.ema.patch()
  #New feature added to bsrc.ema.main()
    #inclusion of Mirco-burst data management
  #New function: bsrc.ema.loopit()
    #As the name suggest, loop all data within a certain folder

#Verion 0.3 Changelog:
  #New function: bsrc.ema.getevent()
    #Isolate certrain event in EMA file
    #Argument to support additional variables 
  #New function: bsrc.ema.scaletonum()
    #Change scale to numeric scale
  #New function: bsrc.ema.getfile()
    #Pre-proc EMA data files
    #Compatible with other functions
  #New function: bsrc.ema.patch()
    #patch DoD entries that supposed to trigger a MB
  #Revised: all previous functions to use bsrc.ema.getfile() first 
  #Decreased priority: bsrc.ema.reshape() to paused

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


bsrc.ema.getfile<-function(filename){
  if (missing(filename)) {
    print("No file specified, please choose the target file")  
    filename<-file.choose()
  }
  tryCatch({
  emadata.raw<- read.csv(filename, stringsAsFactors=FALSE)}, error=function(x){}) #find function
  variname<-read.csv("variname.csv") #find variname
  variname<-as.character(variname$variname)
  names(emadata.raw)<-as.list(variname)
  
  #1st get RedCap ID [check funbsrc exists]:
  mwmatch<-data.frame(funbsrc$registration_redcapid,funbsrc$ema_studyidentifier)
  names(mwmatch)<-c('registration_redcapid','funbsrc$ema_studyidentifier')
  emadata.raw$RedcapID<-mwmatch$registration_redcapid[match(emadata.raw$User_Id,mwmatch$`funbsrc$ema_studyidentifier`)]
  
  #Make things easier:
  emadata.raw$Survey_Class<-emadata.raw$TriggerName
  emadata.raw$Survey_Class[which(!emadata.raw$Survey_Class %in% c("BoD","EoD","DoD"))]<-"MB"

  #process the date:
  d<-as.Date(emadata.raw$Survey_Submitted_Date,format = "%d/%m/%Y")
  emadata.raw$Survey_Submitted_Date<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))
  
  d<-as.Date(emadata.raw$Survey_Started_Date,format = "%d/%m/%Y")
  emadata.raw$Survey_Started_Date<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))
  
  d<-as.Date(emadata.raw$TriggerDate,format = "%d/%m/%Y")
  emadata.raw$TriggerDate<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))
  
  return(emadata.raw)
}

##############################################################


bsrc.ema.main<-function(emadata.raw,path=NULL,forcerun.e=F,forceupdate.e=F,token.e=input.token,ifupload=F,uri.e=input.uri){
  if (missing(emadata.raw)){
    print("Using bsrc.ema.getfile() for data")
    emadata.raw<-bsrc.ema.getfile()
  }
  ifrun<-bsrc.checkdatabase(forcerun = forcerun.e,token = token.e, forceupdate = forceupdate.e)
  
  lRedcapID<-unique(emadata.raw$RedcapID)
  linitial<-unique(subreg$registration_initials[match(lRedcapID,subreg$registration_redcapid)])
  
  #MAKE SURE TO CHECK REDCAP
  #Here is where you can do multiple ID processing loop: However, it might not be even useful bc individual files
  #Currently take out nas, should only be one item:
  RedcapID<-as.character(lRedcapID[1])
  Initial<-linitial[1]
  
  #Patch the data
  emadata.raw<-bsrc.ema.patch(emadata.raw = emadata.raw)
  
  if (ifrun){
    #Read EMA Data:
    table.emadata<-data.table(emadata.raw$RedcapID,emadata.raw$Survey_Submitted_Date,emadata.raw$Survey_Class)
    names(table.emadata)<-c("redcapID","date","Type")
    table.emadata<-table.emadata[order(table.emadata$Type,table.emadata$date),]
    table.emadata[,count:=seq_len(.N), by=Type]
    table.emadata[table.emadata$Type=="MB",count:=seq_len(.N), by=date]
    
    table.emadata<-na.omit(table.emadata)
    #table.emadata<-table.emadata[which(table.emadata$Type %in% c("DoD","BoD","EoD"))]
    
    #Aggregate Total:
    table.emadata$redcapID<-as.character(table.emadata$redcapID)
    emadata<-aggregate(table.emadata,FUN = max,by=list(interaction(table.emadata$date,table.emadata$Type)))
    emadata$Group.1<-NULL
    emadata<-reshape(emadata,idvar = "date",timevar = "Type",direction = "wide", v.names = c("count"))
    emadata<-emadata[order(emadata$date),]
    names(emadata)<-c("redcapID","date","BoD","DoD","EoD","MB")
    emadata$MB[which(is.na(emadata$MB))]<-0
    emadata<-na.locf(emadata)
    emadata$date<-as.Date(emadata$date)
    emadata[is.na(emadata)]<-0
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
    
    mbonly<-as.data.table(emadata.raw[which(emadata.raw$MBYES),c("Survey_Submitted_Date","MBCount")])
    mbonly<-mbonly[, sum(MBCount), by = Survey_Submitted_Date]
    names(mbonly)<-c("date","MB")
    ematotal<-merge(ematotal,mbonly,all=T)
    ematotal$Total<-as.numeric(ematotal$BoD)+as.numeric(ematotal$DoD)+as.numeric(ematotal$EoD)
    ematotal$MB[which(is.na(ematotal$MB))]<-0
    ematotal.melt<-melt(ematotal,id.var='date',variable.name="Type",value.name="expectation")
    
    #melt data
    emadata.full<-merge(ematotal.donly,emadata,all = T)
    
    emadata.full<-na.locf(emadata.full)
    emadata.full.melt<-melt(emadata.full,id.var=c("redcapID","date"), measure.vars=c("BoD","DoD","EoD","Total","MB"),variable.name="Type",value.name="actual")
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
    
    #Safe guard the plot:
    emamelt.merge->emamelt.merge.x
    emamelt.merge<-emamelt.merge[emamelt.merge$Type!="MB",]
    
    #Percentage Plot
    emaplot.percent<-ggplot(data = emamelt.merge, aes(x=date, y=porp, group=Type, shape=Type, color=Type)) +
      ggtitle(paste(Initial,"EMA Progress (Percentage)"))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line()+
      ylab("Percentage")+
      scale_x_date(name="Date",limits = c(startdate+1,NA) ,date_breaks = "2 days")+
      geom_point()+
      geom_label_repel(data = emamelt.merge[(which(emamelt.merge$date %in% c(startdate+7,startdate+14,ifelse(enddate>Sys.Date(),Sys.Date(),enddate)))),], aes(x=date, y=porp,label=per))
    
    ggsave(paste(Initial,"_",Sys.Date(),"_EMAPro_PercentPlot.jpeg",sep = ""),device = "jpeg",plot = emaplot.percent,dpi = 300,path = path, height = 8.3, width = 11.7)
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
    
    ggsave(paste(Initial,"_",Sys.Date(),"_EMAPro_CountPlot.jpeg",sep = ""),device = "jpeg",plot = emaplot.count,dpi = 300,path = path, height = 8.3, width = 11.7)
    print("Completion (count) Plot Saved to Working Directory")
    return(emamelt.merge.x)
    }
}


#####################################################################


bsrc.ema.redcapupload<-function(emamelt.merge=NULL,uri=input.uri,token=input.token, output=T,ifupload=T,curver="2"){
  #safe gurad the function:
  emamelt.merge<-emamelt.merge[emamelt.merge$Type!="MB",]
  
  emamelt.merge$check<-NA
  lengthofema<-21
  startdate<-as.Date(funbsrc$ema_setuptime[which(funbsrc$registration_redcapid==unique(emamelt.merge$redcapID) & funbsrc$ema_setuptime!="")])
  enddate<-startdate+lengthofema
  
  emamelt.merge$check[which(emamelt.merge$date %in% c(startdate+7))]<-"7Days"
  emamelt.merge$check[which(emamelt.merge$date %in% c(startdate+14))]<-"14Days"
  emamelt.merge$check[which(emamelt.merge$date %in% c(startdate+21))]<-"21Days"

  
  if (length(which(is.na((emamelt.merge$check)))) != length(emamelt.merge$date)) {
    test1<-reshape(emamelt.merge[!is.na(emamelt.merge$check),],idvar = "check",timevar = "Type",direction = "wide", v.names = c("actual","per"),drop = c("porp","expectation","diff"))
    test1[which(test1$date>=Sys.Date()),grep("actual",names(test1))[1]:length(test1)]<-"NOT FINISH"
    test2<-reshape(test1,idvar = "redcapID",timevar = "check",direction = "wide", v.names = names(test1)[-c(2,3)])
    test3<-test2
    names(test3)[1]<-"registration_redcapid"
    names(test3)[2:length(names(test3))]<-paste("emapg_",names(test3)[2:length(names(test3))],sep = "")
    names(test3)<-tolower(gsub("[.]","_",names(test3)))
    if(Sys.Date()<enddate+1) {
      test3$ema_completed___ip<-1
      currentexp<-paste("test3$ema_completed___",curver,"<-0", sep = "")
      eval(parse(text=currentexp))
      test3$ema_completed___999<-0
      test3$redcap_event_name<-"ema_arm_1"}
    else {
      test3$ema_completed___ip<-0
      currentexp<-paste("test3$ema_completed___",curver,"<-1", sep = "")
      eval(parse(text=currentexp))
      test3$ema_completed___999<-0
      test3$ema_termdate<-enddate+1
      test3$redcap_event_name<-"ema_arm_1"
      test3$prog_emastatus_di<-NA
      }}
  else {print(paste("Nothing to upload yet, come back after: ", startdate+7))}
  if (ifupload) {
    result.test3<-redcap_write(test3,token = input.token,redcap_uri = input.uri)
    if (result.test3$success) {print("DONE")}}
  if (output) {
  return(test3)}
  }

#####################################################################
bsrc.ema.oneshotupload<-function(filename.e,forceupdate.e=F,ifupload=T,curver.e=2){
  if (missing(filename.e)) {
  print("No file specified, please choose the target file")
  filename.c<-file.choose()}
  else {filename.e->filename.c}
  bsrc.ema.redcapupload(emamelt.merge = bsrc.ema.main(emadata.raw = bsrc.ema.getfile(filename = filename.c), forceupdate.e = forceupdate.e, ifupload = T),ifupload = T,curver = curver.e)
}
#####################################################################
bsrc.ema.redcapreshape<-function(){
  gsub("emaprog_","",names(LGER))
  melt.LGER<-cbind(melt.LGER,data.frame(t(as.data.frame(strsplit(melt.LGER$variable,split = "[_]")))))
  rownames(melt.LGER)<-NULL
  
}
#####################################################################
bsrc.ema.getevent<-function(emadata.raw,pick,additional=NA) {
  if (missing(emadata.raw)) {
    print("Using bsrc.ema.getfile() for data")
    emadata.raw<-bsrc.ema.getfile()
    }
  if (missing(pick)) {
    pick <- readline(prompt = "Please type in BoD, DoD, EoD or MB: ")
    }
  test<-emadata.raw
  test$Survey_Class<-test$TriggerName
  test$Survey_Class[which(!test$Survey_Class %in% c("BoD","EoD","DoD"))]<-"MB"
  
  switch (pick,
    "DoD" = {pick.w<-c("DoD","rp_")},
    "EoD" = {pick.w<-c("EoD","eod_")},
    "BoD" = {pick.w<-c("BoD","bod_")},
    "MB" = {pick.w<-c("MB","mb_")})
  
  test1<-test[which(test$Survey_Class==pick.w[1]),grep(paste(pick.w[2],additional,'User_Id',sep = '|'),names(test))]
  
  return(test1)
}

#####################################################################
bsrc.ema.patch<-function(emadata.raw){
  if (missing(emadata.raw)){
    print("Using bsrc.ema.getfile() for data")
    emadata.raw<-bsrc.ema.getfile()}
  
  emadata.raw<-bsrc.ema.scaletonum(emadata.raw = emadata.raw)  
  dodonly<-bsrc.ema.getevent(emadata.raw = emadata.raw, pick = "DoD")
  negnum<-grep(paste("angry","nervous","sad","irritated",sep = "|"),names(dodonly))
  negnum<-negnum[negnum >20]
  dodonly$ifnegative<-rowSums(dodonly[,negnum] >= 2)>0
  dodonly$ifintime<-dodonly$rp_time %in% c("Just happened","15 minutes","30 minutes","45 minutes")
  rownum<-as.numeric(rownames(dodonly[which(dodonly$ifintime & dodonly$ifnegative),]))
  
  emadata.raw$MBYES<-NA
  emadata.raw$MBCount<-NA
  emadata.raw$MBYES[rownum]<-TRUE
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "Just happened")]<-4
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "15 minutes")]<-3
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "30 minutes")]<-2
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "45 minutes")]<-1
  
  return(emadata.raw)
  }
    
#####################################################################
bsrc.ema.scaletonum<-function(emadata.raw){
  if (missing(emadata.raw)){
    print("Using bsrc.ema.getfile() for data")
    emadata.raw<-bsrc.ema.getfile()}

  emadata.nodate<-emadata.raw[,-grep("Date",names(emadata.raw))]
  emadata.nodate[emadata.nodate == "Very Slightly or Not at All"]<-1
  emadata.nodate[emadata.nodate == "A Little"]<-2
  emadata.nodate[emadata.nodate == "Moderately"]<-3
  emadata.nodate[emadata.nodate == "Quite a Bit"]<-4
  emadata.nodate[emadata.nodate == "A great deal"]<-5
  emadata.nodate[emadata.nodate == "CONDITION_SKIPPED"]<-NA
  emadata.nodate[emadata.nodate == ""]<-NA
  emadata.nums<-cbind(emadata.nodate,emadata.raw[,grep("Date",names(emadata.raw))])
  
  return(emadata.nums)
}

################################################################
ema.loop.path<<-paste(getwd(),"/EMA-Completed",sep = "")

bsrc.ema.loopit<-function(path, style="long") {
  if(missing(path)){path=getwd()}
  temp<-list.files(path<-path,pattern="*.csv")
  for (i in 1:length(temp)){
    print(paste("Now processing ",i," out of ",length(temp),sep = ""))
    filename<-paste(path,temp[i],sep = "/")
    emadata.raw<-bsrc.ema.getfile(filename = filename)
    output<-bsrc.ema.main(emadata.raw = emadata.raw)
    if (i==1){outcome<-output}
    outcome<-merge(outcome,output,all=T)
    output<-NULL
  }
  return(outcome)
}





