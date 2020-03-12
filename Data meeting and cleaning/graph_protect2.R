###Graph the visit dates:

protect_db <- bsrc.checkdatabase2(protocol = ptcs$protect,online = F)
# v_id <- c("registration_redcapid")
# v_date_baseline <- c("bq_date")
# v_date_follow_up <- c("fug_date")
# df_b <- protect_db$data[c(v_id,"redcap_event_name",v_date_baseline)]
# df_fu <- protect_db$data[c(v_id,"redcap_event_name",v_date_follow_up)]
# names(df_b)[grepl(v_date_baseline,names(df_b))]<-"date"
# names(df_fu)[grepl(v_date_follow_up,names(df_fu))]<-"date"

visit_vari <- c("bq_date","fug_date")
dxq<-lapply(visit_vari,function(v_date) {
  dfx <- protect_db$data[c(v_id,"redcap_event_name",v_date)]
  names(dfx)[grepl(v_date,names(dfx))]<-"date"
  dfx[dfx == ""] <- NA
  dfx <- na.omit(dfx)
  return(dfx)
})
names(dxq) <- visit_vari
df_rb <- do.call(rbind,dxq)
df_rb$date <- as.Date(df_rb$date)
df_init<-do.call(rbind,lapply(split(df_rb,df_rb$registration_redcapid),function(dx){
  init_df<-dx[which.min(dx$date),]
  names(init_df)<-c("registration_redcapid","First_Event","First_Date")
  last_df<-dx[which.max(dx$date),2:3]
  names(last_df)<-c("Last_Event","Last_Date")
  return(cbind(init_df,last_df))
}))



#Transfer errors: 
df_init$Is_First_Baseline <- grepl("baseline_arm_.*",df_init$First_Event)
df_init <- df_init[order(df_init$First_Date),]
df_init$registration_redcapid <- factor(as.character(df_init$registration_redcapid),levels = df_init$registration_redcapid[order(df_init$First_Date)])
df_init$Dur_days <- df_init$Last_Date - df_init$First_Date
#need to order ID by earliest entry:
df_gx <- merge(df_rb, df_init,by = "registration_redcapid",all = T)
df_gx$date <- as.Date(df_gx$date)
df_gx <- df_gx[order(df_gx$First_Date),]
df_gx$registration_redcapid <- factor(as.character(df_gx$registration_redcapid),levels = df_init$registration_redcapid[order(df_init$First_Date)])

ggplot(df_gx, aes(y=date,x=registration_redcapid)) + geom_point() + scale_y_date(date_labels = "%Y/%m",date_breaks = "6 months") +
  theme(axis.title.y =element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(data=df_init, aes(x=registration_redcapid,y=Dur_days/30)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  theme(axis.title.x =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

df_init$Dur_days <- df_init$Last_Date - df_init$First_Date

ham_p2 <- bsrc.getform(protocol = ptcs$protect,formname = "ham24",online = T,batch_size = 1000L)
