delete this ****

mbstat<-xj$fulldata.ema$pdata[xj$fulldata.ema$pdata$Type=="MB",]

mrstat<-do.call(rbind,lapply(split(mbstat,mbstat$redcapID),function(xr){
  data.frame(exp=sum(xr$expectation),
             actual=sum(xr$actual))
}))


mrstat$Group<-bsocial$data$registration_group[match(rownames(mrstat),bsocial$data$registration_redcapid)]


lapply(split(mrstat,mrstat$Group),summary)


scandb<-bsrc.conredcap2(protocol = ptcs$scandb,online = T,batch_size = 500)
masterdemo<-bsrc.conredcap2(protocol = ptcs$masterdemo,online = T,batch_size = 1000)

####LEARN
LEARN_df<-bsrc.getform(formname = c("learn","explore"),curdb = scandb)
rxa<-LEARN_df[c("registration_redcapid","learn_scandate","explore_scandate")]
rxa$date<-rxa$learn_scandate
rxa$date[is.na(rxa$date)]<-rxa$explore_scandate[is.na(rxa$date)]
rxa_max<-aggregate(date~registration_redcapid,rxa,min)
#we double check with previous number:
#146 as of 4/20/18. That was the last renewal
length(which(rxa_max$date < as.Date("2018-04-20"))) #only has 132; jk it's right 

LEARN_MD<-masterdemo$data[which(as.logical(masterdemo$data$registration_ptcstat___learn ) | as.logical(masterdemo$data$registration_ptcstat___explore)),]
LEARN_MD[LEARN_MD==""]<-NA
mxa<-LEARN_MD[c("registration_redcapid","reg_condate_learn","reg_condate_explore")]
mxa$mdate<-mxa$reg_condate_learn
mxa$mdate[is.na(mxa$mdate)]<-mxa$reg_condate_explore[is.na(mxa$mdate)]


jra<-merge(x = rxa_max,y = mxa[c("registration_redcapid","mdate")],by = "registration_redcapid",all = T)

###P2
p2x<-masterdemo$data[which(as.logical(masterdemo$data$registration_ptcstat___protect2)),]


#BS
olddb<-read.csv("Updated_ID_List.csv")
olddb$registration_redcapid<-NULL
olddbmap<-unique(olddb[c("Status","StatusWord")])
grx<-bsrc.findid(olddb,id.var = "ID")
grx$race<-subreg$Race[match(grx$registration_redcapid,subreg$registration_redcapid)]
grk<-grx[which(!grx$Status %in% c(5,4,0,-1)),]
grk$gender<-subreg$Gender[match(grk$registration_redcapid,subreg$registration_redcapid)]


newsubreg<-subreg[as.Date(subreg$registration_consentdate) < as.Date("2018-04-31"),]
#newconsent<-newconsent[newconsent$Group!="Healthy Control",]
newconsent<-newconsent[which(!newconsent$registration_status %in% c(4,6,5,88,89)),]
nrow(newconsent)
table(newconsent$Race,newconsent$Gender,newconsent$registration_hispanic)

sapply(unique(newconsent$Race),function(xr){
  length(which(newconsent$Race==xr)) / nrow(newconsent)
})

sapply(unique(newconsent$registration_hispanic),function(xr){
  length(which(newconsent$registration_hispanic==xr)) / nrow(newconsent)
})


bsocial<-bsrc.checkdatabase2(ptcs$bsocial)
masterdemo<-bsrc.conredcap2(ptcs$masterdemo,output = T)
ksocial<-bsrc.conredcap2(protocol = ptcs$ksocial,output = T)

subreg<-bsrc.getevent(eventname = "enrollment_arm_1",subreg = T,curdb = curdb)

kdemo<-bsrc.checkbox(dfx = kdemo)
masterdemo$data<-bsrc.checkbox(dfx = masterdemo$data)
subreg<-bsrc.checkbox(dfx=subreg)
outid<-kdemo$registration_redcapid[which(!kdemo$registration_redcapid %in% masterdemo$data$registration_redcapid)]
klite<-kdemo[c("registration_redcapid","registration_group","registration_gender","registration_race","registration_hispanic")]
klite$numna<-as.numeric(apply(klite,1,function(x){length(which(is.na(x)))}))
mk_ovlap<-masterdemo$data[which(masterdemo$data$registration_redcapid %in% klite$registration_redcapid),
                          c("registration_redcapid","registration_group","registration_gender","registration_race","registration_hispanic")]
mk_ovlap$numna<-as.numeric(apply(mk_ovlap,1,function(x){length(which(is.na(x)))}))
bkoverlap<-subreg[which(subreg$registration_redcapid %in% klite$registration_redcapid),
                  c("registration_redcapid","registration_group","registration_gender","registration_race","registration_hispanic")]
bkoverlap$numna<-as.numeric(apply(bkoverlap,1,function(x){length(which(is.na(x)))}))


allkdemo<-rbind(mk_ovlap,klite,bkoverlap)
sp_kde<-split(allkdemo,allkdemo$registration_redcapid)
kdemo<-do.call(rbind,lapply(sp_kde,function(xz){
  xz[which.min(xz$numna),]
}))
kdemo$registration_group<-plyr::mapvalues(kdemo$registration_group,from = c(1,2,4,6,7),to = c("HC","DEP","IDE","ATT","ATT"))
kdemo$registration_race<-sapply(kdemo$registration_race,function(xr){if(length(xr)>1){"Mixed"}else{xr}})
kdemo$registration_gender<-plyr::mapvalues(kdemo$registration_gender,from = c("F2M","M2F","FALSE"),to = c("F","M","F"))
table(kdemo$registration_race,kdemo$registration_gender,kdemo$registration_hispanic,kdemo$isclinical)

# This will be the master demo sync through the bsocial and ksocial projects;
#func requirement:
funclist<-list(masterdemoptc=ptcs$masterdemo,fromproj=ptcs$bsocial)
list2env(funclist,envir = .GlobalEnv)
masterdemo<-bsrc.conredcap2(protocol = masterdemoptc,batch_size = "2000",output = T)
fromwhere <- bsrc.checkdatabase2(protocol = fromproj,batch_size = "200",output = T)




























