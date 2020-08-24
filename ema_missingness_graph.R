
regularize_zero2one<-function(vec=NULL){
 vec <- as.numeric(vec)
 vec[vec < 0] <- NA
 vec[vec > 1] <- 1
 return(vec)
}

masterdemo <- bsrc.checkdatabase2(protocol = ptcs$masterdemo,curdb=NULL)
idmap_new <- masterdemo$data[c("registration_redcapid","registration_wpicid",
                           "registration_soloffid","registration_group","registration_lethality")]
names(idmap_new) <- c("redcapid","wpicid","soloffid","ms_group","lethality")
ema <- bsrc.attachngrab(rdpath = rdpaths$ema)

info_df <- ema$fulldata.ema$info

#Update group status:
info_df <- bsrc.findid(df = info_df,idmap = idmap_new,id.var = "RedcapID")
info_df$Group<-as.character(info_df$Group)
info_df$Group[info_df$Group %in% c("Not Sure Yet") | is.na(info_df$Group)]<-info_df$ms_group[info_df$Group %in% c("Not Sure Yet") | is.na(info_df$Group)]
info_df$Group[info_df$Group=="ATT"] <- ifelse(info_df$lethality[info_df$Group=="ATT"]=="hl","High Lethality","Low Lethality")
info_df$Group[info_df$Group=="HC"] <- "Healthy Control"

ggplot(info_df,aes(x=regularize_zero2one(CompletionRate),color=Group,fill=Group)) +
 geom_histogram(binwidth = 0.05) + xlab("Completion Rate") +ylab("Frequency")

ggplot(info_df,aes(x=MBCount,color=Group,fill=Group)) +
 geom_histogram(binwidth = 20) + xlab("Trigerred Microbursts") +ylab("Frequency")

ggplot(info_df,aes(x=MBCount,y=regularize_zero2one(MBProp),color=Group,fill=Group)) +
 geom_point() + xlab("Trigerred Microbursts") +ylab("Percentage of Completed")

progress_df <- do.call(rbind,ema$fulldata.ema$progress_data)
progress_df <- merge(progress_df,info_df[c("RedcapID","Group")],by.x="RedCapID",by.y = "RedcapID",all.x = T)

ag_prog_df <- aggregate(data=progress_df,porp~Group+Type+daysinstudy,FUN = function(x){mean(regularize_zero2one(x[x!=Inf]),na.rm = T)})
ag_prog_df <- ag_prog_df[!ag_prog_df$Group %in% c("Ineligible / Not Applicable"),]
ggplot(ag_prog_df,aes(x=as.numeric(daysinstudy),y=as.numeric(porp),color=Group,fill=Group)) +
 geom_line() +geom_point() + xlab("Days in study") +ylab("Completion Rate") + facet_wrap(~Type,ncol = 1)

ag_prog_noHC <- ag_prog_df[!ag_prog_df$Group %in% c("Healthy Control"),]
mean(ag_prog_noHC$porp[ag_prog_noHC$Type=="Total"])
mean(ag_prog_noHC$porp[ag_prog_noHC$Type=="MB"])

ag_prog_HCOnly <- ag_prog_df[ag_prog_df$Group %in% c("Healthy Control"),]
mean(ag_prog_HCOnly$porp[ag_prog_HCOnly$Type=="Total"])
mean(ag_prog_HCOnly$porp[ag_prog_HCOnly$Type=="MB"])





