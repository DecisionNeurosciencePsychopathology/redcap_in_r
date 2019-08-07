#Reporter

#EMA Chunk:


#Load up the db:
ema_db<-bsrc.attachngrab(rdpath = rdpaths$ema)

completedInfo<-ema_db$fulldata.ema$info[ema_db$fulldata.ema$info$Status!="IN-PROGRESS",]

ggplot(completedInfo,aes(x=CompletionRate))+geom_histogram(fill="skyblue",color="black",bins = 10)+
  geom_vline(aes(xintercept=mean(completedInfo$CompletionRate)),color="darkred") + ggtitle("Completion Rate (Overall)")

#by Group
ggplot(completedInfo,aes(x=Group,y=CompletionRate,color=Group))+geom_boxplot()+geom_jitter()

#MB count and completion Rate
ggplot(completedInfo[which(!completedInfo$MBProp==Inf),],aes(x=Group,y=MBCount,color=MBProp))+
  geom_boxplot()+geom_jitter()+scale_color_gradient2(limits = c(0,1)) + ggtitle("MB Count, gradient by percentage completed")

#By Time point!
tpDF <- do.call(rbind,ema_db$fulldata.ema$progress_data)
tpDF$Group<-ema_db$fulldata.ema$info$Group[match(tpDF$RedCapID,ema_db$fulldata.ema$info$RedcapID)]
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

tp_sumDF<-data_summary(tpDF,varname = "porp",groupnames = c("Type","daysinstudy","Group"))
tp_sumDF<-tp_sumDF[which(tp_sumDF$Group!="Ineligible / Not Applicable"),]

ggplot(tp_sumDF, aes(x=daysinstudy, y=porp, group=Type, color=Type)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=porp-sd, ymax=porp+sd), width=.2,
                position=position_dodge(0.05)) + facet_wrap(~Group)

#
allMBs<-lapply(ema_db$fulldata.ema$proc_data,function(x) {x$MB})

summarizeMB<-function(dfx){
  dfx<-dfx[!is.na(dfx$TriggerName),]
  dfx$timepoint<-sapply(strsplit(gsub("mins","",dfx$TriggerName),split = "_"),function(x){x[x=="JH"]<-0;sum(as.numeric(x))})
  clean_dfx<-cbind(dfx[grep("mb_",names(dfx))],dfx[c("DateTime","RedcapID","timepoint")])
}

proc_MB<-do.call(rbind,lapply(allMBs,summarizeMB))
neg_items<-which(names(proc_MB) %in% c("mb_af_angry","mb_af_nervous","mb_af_sad","mb_af_irritated"))
pos_items<-which(names(proc_MB) %in% c("mb_af_happy","mb_af_excited","mb_af_content"))

proc_MB$SUM_NEG<-apply(proc_MB,1,function(x){sum(as.numeric(x[neg_items]))})
proc_MB$SUM_POS<-apply(proc_MB,1,function(x){sum(as.numeric(x[pos_items]))})
proc_MB$Group<-ema_db$fulldata.ema$info$Group[match(proc_MB$RedcapID,ema_db$fulldata.ema$info$RedcapID)]
proc_MB<-proc_MB[which(proc_MB$Group!="Ineligible / Not Applicable"),]
proc_MB$Group<-relevel(proc_MB$Group,ref = "Healthy Control")

ggplot(proc_MB,aes(x = as.factor(timepoint), y=SUM_NEG,color=Group))+geom_boxplot()
ggplot(proc_MB,aes(x = as.factor(timepoint), y=SUM_POS,color=Group))+geom_boxplot()

summary(lmerTest::lmer(SUM_POS~as.factor(timepoint)*Group+(1|RedcapID),data = proc_MB))
summary(lmerTest::lmer(SUM_NEG~as.factor(timepoint)*Group+(1|RedcapID),data = proc_MB))








