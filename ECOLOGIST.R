###
#Title: "Ecologist"
#Author: "Jiazhou Chen"
#Version: 0.9
###
#Version 0.9 Changelog:
  #bsrc.ema.mwredcapmatch() has new function to match duplicated records.
      #____CONSIDER: keep track of 

#Version 0.8 Changelog:
  #Temp change to bsrc.ema.main to adopt the new changes that happened in version 3.0
  #Temp changes to bsrc.ema.getfile() to adopt the new changes in version 3.0

#Version 0.7 changelog:
  #bsrc.mwredcapmatch() deal with complication in mulisubject single file system

#version 0.6 Changelog:
  #bsrc.ema.getevent() & bsrc.ema.patch now updated to support upcoming EMA Version 3 

#Version 0.5 Changelog:
  #Revision for bsrc.ema.loopit(): Now takes out MB ones that are NA, and will update redcap accordingly at oneshot.
  #Fixed error in bsrc.ema.getevent() where additional argument would not function porperly
  #Fixed errors in bsrc.ema.getfile() & bsrc.ema.main() where file would not uplaod

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


#EMA 3 Exclusive Functions:
bsrc.ema.mwredcapmatch<-function(ema3.raw=NULL) {
  ema3<-ema3.raw
  ema3$ema_id[which(ema3$ema_id=="")]<-NA
  localmatch<-ema3[which(!is.na(ema3$ema_id) & !duplicated(ema3$ema_id)),grep(paste("User.Id","ema_id",sep = "|"),names(ema3))]
  names(localmatch)<-c("ema_studyidentifier","registration_redcapid")
  funema<<-bsrc.getform(formname = "ema_session_checklist")

  if (any(duplicated(localmatch$ema_studyidentifier))){
    print("HMM,Maybe it's right in another entry?")
   
     
  }
  
  
  if (any(is.na(!match(funema$ema_studyidentifier[match(localmatch$registration_redcapid,funema$registration_redcapid)],
                       localmatch$ema_studyidentifier) == 1:length(localmatch$ema_studyidentifier)))) 
  {print("ARGHHHHHH!!! THESE PAIRS DON'T MATCH")
  disrupt<-localmatch[which(is.na(!match(funema$ema_studyidentifier[match(localmatch$registration_redcapid,funema$registration_redcapid)],
                                  localmatch$ema_studyidentifier) == 1:length(localmatch$ema_studyidentifier))),]
  print(disrupt)
    #
      for (i in 1:length(disrupt$ema_studyidentifier)) {
          if (disrupt$ema_studyidentifier[i] %in% funema$ema_studyidentifier) {
            if (length(which(localmatch$ema_studyidentifier %in% disrupt$ema_studyidentifier)) > 1) {
              actualid.try<-localmatch$registration_redcapid[which(localmatch$ema_studyidentifier %in% localmatch$ema_studyidentifier[which(duplicated(localmatch$ema_studyidentifier))])][!localmatch$registration_redcapid[which(localmatch$ema_studyidentifier %in% localmatch$ema_studyidentifier[which(duplicated(localmatch$ema_studyidentifier))])] %in% disrupt$registration_redcapid]
              print("Try to grab from localmatch duplicates")
              if (length(actualid.try)==1){
              stepone<-TRUE
              actualid.try->actualid
              print("success!")}
              }else {stepone<-FALSE}
          if (!stepone){
          print("Try to grab it from RedCap.")
          maybeid<-funema$registration_redcapid[match(disrupt$ema_studyidentifier[i],funema$ema_studyidentifier)]  
          idq<-readline(prompt = paste("Is ",maybeid," the right RedCap ID for this person? y/n?   :"))
          if (idq=="y"){idq<-TRUE} else (idq<-FALSE)
          if (idq) {maybeid->actualid} else {actualid<-readline(prompt = "What's their actual RedCap ID? : ")}}
          localmatch$registration_redcapid[which(localmatch$ema_studyidentifier %in% disrupt$ema_studyidentifier)]<-actualid
          } else {actualid<-readline(prompt =paste("MetricWire Identifier Has No Match; Please provide an RedCap ID for [",disrupt$ema_studyidentifier[i],"]: "))
         localmatch$registration_redcapid[which(localmatch$ema_studyidentifier %in% disrupt$ema_studyidentifier)]<-actualid}
      }
  }    
  else {print("NO DISRUPT")}
  localmatch<-localmatch[-which(duplicated(localmatch)),]
  return(localmatch)
    #
  }

############### General Get File
bsrc.ema.getfile<-function(filename,ifupload=F,uri.e=input.uri,token.e=input.token, curver="2"){
  if (missing(filename)) {
    print("No file specified, please choose the target file")  
    filename<-file.choose()
  }
  tryCatch({
    emadata.raw<- read.csv(filename, stringsAsFactors=FALSE)}, error=function(x){}) #find function
  run2<-F
  run3<-F
  switch(curver, "2" = {run2<-T}, "3" = {run3<-T})
  if (run2){
  variname<-read.csv("variname.csv") #find variname
  variname<-as.character(variname$variname)
  names(emadata.raw)<-as.list(variname)
  mwmatch<-data.frame(funbsrc$registration_redcapid,funbsrc$ema_studyidentifier)
  names(mwmatch)<-c('registration_redcapid','funbsrc$ema_studyidentifier')
  emadata.raw$RedcapID<-mwmatch$registration_redcapid[match(emadata.raw$User_Id,mwmatch$`funbsrc$ema_studyidentifier`)]
  RedcapID<-unique(emadata.raw$RedcapID)
  emadata.raw$Survey_Class<-emadata.raw$TriggerName
  emadata.raw$Survey_Class[which(!emadata.raw$Survey_Class %in% c("BoD","EoD","DoD"))]<-"MB"
  d<-as.Date(emadata.raw$Survey_Submitted_Date,format = "%d/%m/%Y")
  emadata.raw$Survey_Submitted_Date<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))
  d<-as.Date(emadata.raw$Survey_Started_Date,format = "%d/%m/%Y")
  emadata.raw$Survey_Started_Date<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))
  d<-as.Date(emadata.raw$TriggerDate,format = "%d/%m/%Y")
  emadata.raw$TriggerDate<-as.Date(ifelse(d < "2012-12-31", format(d, "20%y-%m-%d"), format(d)))}
  if(run3) {
    emadata.raw$Survey_Class<-emadata.raw$TriggerName
    emadata.raw$Survey_Class[which(emadata.raw$Survey_Class %in% c("BoD_U"))]<-"BoD"
    emadata.raw$Survey_Class[which(emadata.raw$Survey_Class %in% c("DoD_U"))]<-"DoD"
    emadata.raw$Survey_Class[which(emadata.raw$Survey_Class %in% c("EoD_U"))]<-"EoD"
    emadata.raw$Survey_Class[which(emadata.raw$Survey_Class %in% c(""))]<-"SetUp"
    emadata.raw$Survey_Class[which(!emadata.raw$Survey_Class %in% c("BoD","EoD","DoD","SetUp",""))]<-"MB"
    idmatch<-bsrc.ema.mwredcapmatch(emadata.raw)
    emadata.raw$RedcapID<-idmatch$registration_redcapid[match(emadata.raw$User_Id, idmatch$ema_studyidentifier)]
    lRedcapID<-unique(emadata.raw$RedcapID)
    emadata.raw$Survey_Started_Date<-as.Date(emadata.raw$Survey_Started_Date)
    emadata.raw$Survey_Submitted_Date<-as.Date(emadata.raw$Survey_Submitted_Date)
    emadata.raw$TriggerDate<-as.Date(emadata.raw$TriggerDate)
  }
  if (ifupload) {redcap_upload_file_oneshot(file_name = filename,redcap_uri = uri.e,token = token.e,record = RedcapID, field = "emapg_fileupload", event = "ema_arm_1")}
  return(emadata.raw)
}

##############################################################
#Check
#subactivity$fordate <- as.Date(strptime(subactivity$For.Time, '%d/%m/%Y %H:%M:%S'))

############### EMA2 Main function:
#####Currently hard fixed for EMA 3; new main function needed:
bsrc.ema.main<-function(emadata.raw,path=NULL,forcerun.e=F,forceupdate.e=F,token.e=input.token,ifupload=F,uri.e=input.uri,graphic=T){
  if (missing(emadata.raw)){
    print("Using bsrc.ema.getfile() for data")
    emadata.raw<-bsrc.ema.getfile(curver = "2")
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
  emadata.raw<-bsrc.ema.patch(emadata.raw = emadata.raw,vers = "2")
  
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
    
    #New Merge
    emamelt.merge<-merge(emadata.full.melt,ematotal.melt,all=T)
    emamelt.merge<-emamelt.merge[which(!emamelt.merge$date==startdate),] #Take out startdate
    emamelt.merge<-emamelt.merge[which(!emamelt.merge$date>ifelse(enddate>Sys.Date(),Sys.Date(),enddate)),] 
    emamelt.merge$actual<-as.numeric(emamelt.merge$actual)
    emamelt.merge$expectation<-as.numeric(emamelt.merge$expectation)
    emamelt.merge$diff<-emamelt.merge$actual - emamelt.merge$expectation
    emamelt.merge$porp<-round(emamelt.merge$actual / emamelt.merge$expectation *100,2)
    emamelt.merge$per<-paste(emamelt.merge$porp, "%")
    
    #Safe guard the plot:
    emamelt.merge->emamelt.merge.x
    emamelt.merge<-emamelt.merge[emamelt.merge$Type!="MB",]
   
    if (graphic){ 
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
      #End Graphic
    }
    return(emamelt.merge.x)
    }
}
############### EMA 2 RedCap update function: 
bsrc.ema.redcapupload<-function(emamelt.merge=NULL,uri=input.uri,token=input.token, output=T,ifupload=T,curver="2"){
  #safe gurad the function:
  emamelt.merge<-emamelt.merge[emamelt.merge$Type!=c("MB","SetUp"),]
  
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
    result.test3<-redcap_write(test3,token = token,redcap_uri = uri)
    if (result.test3$success) {print("DONE")}}
  if (output) {
  return(test3)}
  }
############## Revert RedCap data back into long format [IN DEV]
bsrc.ema.redcapreshape<-function(){
  gsub("emaprog_","",names(LGER))
  melt.LGER<-cbind(melt.LGER,data.frame(t(as.data.frame(strsplit(melt.LGER$variable,split = "[_]")))))
  rownames(melt.LGER)<-NULL
  
}
################ Get certrain part of EMA data
bsrc.ema.getevent<-function(emadata.raw,pick.input,additional=NA, vers="3") {
  if (missing(emadata.raw)) {
    print("Using bsrc.ema.getfile() for data")
    emadata.raw<-bsrc.ema.getfile()
    }
  if (missing(pick.input)) {
    pick.input <- readline(prompt = "Please type in BoD, DoD, EoD or MB: ")
  }
  switch (vers,
          "2" = ltrigger<-c("BoD","DoD","EoD","MB"),
          "3" = ltrigger<-c("BoD_U","DoD_U","EoD_U","MB")
  )
  pick<-ltrigger[grep(pick.input,ltrigger)]
  
  test<-emadata.raw
  
  switch (pick,
    "DoD" = {pick.w<-c("DoD","rp_")},
    "EoD" = {pick.w<-c("EoD","eod_")},
    "BoD" = {pick.w<-c("BoD","bod_")},
    "DoD_U" = {pick.w<-c("DoD_U","rp_")},
    "EoD_U" = {pick.w<-c("EoD_U","eod_")},
    "BoD_U" = {pick.w<-c("BoD_U","bod_")},
    "MB" = {pick.w<-c("MB","mb_")})
  
  test1<-test[which(test$Survey_Class==pick.w[1]),grep(paste(pick.w[2],additional,'User_Id',sep = '|',collapse = "|"),names(test))]
  
  return(test1)
}

################ Patch the data for emadata.raw and counts for 'em:
bsrc.ema.patch<-function(emadata.raw,vers="3"){
  if (missing(emadata.raw)){
    print("Using bsrc.ema.getfile() for data")
    emadata.raw<-bsrc.ema.getfile()}
  switch (vers,
    "2" = ltrigger<-c("BoD","DoD","EoD","MB"),
    "3" = ltrigger<-c("BoD_U","DoD_U","EoD_U","MB")
  )
  
  rownames(emadata.raw)<-NULL
  emadata.raw<-bsrc.ema.scaletonum(emadata.raw = emadata.raw)  
  dodonly<-bsrc.ema.getevent(emadata.raw = emadata.raw, pick.input = ltrigger[2], vers = vers)
  negnum<-grep(paste("angry","nervous","sad","irritated",sep = "|",collapse = "|"),names(dodonly))
  negnum<-negnum[negnum >20]
  dodonly$ifnegative<-rowSums(dodonly[,negnum] >= 2)>0
  dodonly$ifintime<-dodonly$rp_time %in% c("Just happened","15 minutes","30 minutes","45 minutes")
  rownum<-as.numeric(rownames(dodonly[which(dodonly$ifintime & dodonly$ifnegative),]))
  
  emadata.raw$MBYES<-FALSE
  emadata.raw$MBCount<-NA
  emadata.raw$MBYES[rownum]<-TRUE
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "Just happened")]<-4
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "15 minutes")]<-3
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "30 minutes")]<-2
  emadata.raw$MBCount[which(emadata.raw$MBYES & emadata.raw$rp_time == "45 minutes")]<-1
  
  return(emadata.raw)
  }
    
############### Scale to Num
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

########################################
########### LOOOOOOOOOOOOOPS ###########
########################################
#Might not be useful with the introduction of EMA 3;
#Whelp, shame on you, cuzzzzz it's totally still useful
bsrc.ema.loopit<-function(path=NULL, file=NULL, style="long", graphic=T,ifupload.e=T, curver.e="2",forceupdate=F,outputstyle="outcome",ifupload = T,uri=input.uri,token=input.token) {
  if(curver.e=="2" & is.null(path)){path<-getwd()}
  if(curver.e=="3" & is.null(file)){file<-file.choose()}
  run2<-F
  run3<-F
  switch(curver.e, 
         "2" = {
  temp<-list.files(path<-path,pattern="*.csv")
  print("This is to upload and update redcap")
  for (i in 1:length(temp)){
    print(paste("Now processing ",i," out of ",length(temp),sep = ""))
    filename<-paste(path,temp[i],sep = "/")
    emadata.raw<-bsrc.ema.getfile(filename = filename, curver = "2")
    output<-bsrc.ema.main(emadata.raw = emadata.raw, graphic = graphic)
    output.r<-bsrc.ema.redcapupload(emamelt.merge = output,output = T, ifupload = F)
      if (i==1){outcome<-output
      outcome.r<-output.r}
    outcome<-merge(outcome,output,all=T)
    output<-NULL
    outcome.r<-merge(outcome.r,output.r,all=T)
    output.r<-NULL
  }
  #upload after combined;
  if (ifupload) {
    result.outcome.r<-redcap_write(outcome.r,token = token,redcap_uri = uri)
    if (result.outcome.r$success) 
    {print("DONE")}
  }
  },
  "3" = {
    emadata.raw<-bsrc.ema.getfile(filename = file, curver = "3")
    for (i in 1:length(unique(emadata.raw$RedcapID))) {
      print("##########################")
      print(paste("Now processing ",i," out of ",length(unique(emadata.raw$RedcapID)),sep = ""))
      curredcap<-unique(emadata.raw$RedcapID)[i]
      currda<-emadata.raw[which(emadata.raw$RedcapID==curredcap),]
      rownames(currda)<-NULL
      fstatus<-F
      tryCatch({
        output<-bsrc.ema.main(emadata.raw = currda, graphic = graphic)}, error=function(x){
        fstatus<-T
        print("EMA MAIN NOT DONE")
        print(unique(emadata.raw$RedcapID)[i])}) 
      tryCatch({
        output.r<-bsrc.ema.redcapupload(emamelt.merge = output,output = T, ifupload = F)}, error=function(x){
        fstatus<-T
        print("REDCAP UPLOAD NOT DONE")
        print(unique(emadata.raw$RedcapID)[i])}) 
      
      if (i==1){outcome<-output
      outcome.r<-output.r}
        if (!is.null(output)) {
        print("MERGING MAIN")
        outcome<-merge(outcome,output,all=T)
        }
        if (!is.null(output.r)) {
        print("MERGING REDCAP")
        outcome.r<-merge(outcome.r,output.r,all=T)
        }
      
      ouput<-NULL
      output.r<-NULL
      }
    #upload after combined;
    if (ifupload) {
      result.outcome.r<-redcap_write(outcome.r,token = token,redcap_uri = uri)
      if (result.outcome.r$success) 
      {print("DONE")}
      }
    })
  outcome<-outcome[which(!outcome$porp %in% c("NaN")),]
  
  return(list(main=outcome,redcapupload=outcome.r))
}


##############Pretty much done/Old functions
if (FALSE) {
############### Intergrated main and redcapupload:
  bsrc.ema.oneshotupload<-function(filename.e,forceupdate.e=F,ifupload=T,curver.e=2, graphic.e=T){
    if (missing(filename.e)) {
      print("No file specified, please choose the target file")
      filename.c<-file.choose()}
    else {filename.e->filename.c}
    bsrc.ema.redcapupload(emamelt.merge = bsrc.ema.main(emadata.raw = bsrc.ema.getfile(filename = filename.c), forceupdate.e = forceupdate.e, ifupload = T, graphic = graphic.e),ifupload = T,curver = curver.e)
  }
###############  
}
######################################
#Unified

#bsrc.ema.loopitupdate<-function(path.e, style="long", ifupload.e=T, curver.e=2, graphic=F,forceupdate=F) {
  #if(missing(path.e)){path.e=getwd()}
  #temp<-list.files(path<-path.e,pattern="*.csv")
  #for (i in 1:length(temp)){
    #print(paste("Now processing ",i," out of ",length(temp),sep = ""))
    #filename<-paste(path.e,temp[i],sep = "/")
    #output.r<-bsrc.ema.oneshotupload(filename.e = filename,ifupload = F,curver.e = curver.e, graphic.e = graphic, forceupdate.e = forceupdate)
    #if (i==1){outcome.r<-output.r}
    #outcome.r<-merge(outcome.r,output.r,all=T)
   # output.r<-NULL
  #}
  #if(ifupload.e){
   # result.outcome.r<-redcap_write(outcome.r,token = input.token,redcap_uri = input.uri)
  # return(outcome.r)
#}




