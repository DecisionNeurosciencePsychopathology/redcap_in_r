#New data meeting: 

masterdemo <- bsrc.checkdatabase2(protocol = ptcs$masterdemo,batch_size=1000L)
bsocial <- bsrc.checkdatabase2(protocol = ptcs$bsocial,forceskip = T)
idmap <- masterdemo$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]

#Proc edu:
edumap<-data.frame(yrlable=c("0-12","13-14","15-16","17-18","18+"),yrstar=c(0,13,15,17,19))
subreg$Edu<-as.character(edumap$yrlable[findInterval(subreg$eduyrs,edumap$yrstar)])

masterdemo$data<-bsrc.checkbox(variablename = "registration_ptcstat",dfx = masterdemo$data)

#ptcs_all <- unique(unlist(masterdemo$data$registration_ptcstat,recursive = T))

ptcs_toget <- c("bsocial","protect2","ksocial","explore","snake","pie")


md_sp<-lapply(ptcs_toget,function(ptcx){
  list(df=masterdemo$data[which(sapply(masterdemo$data$registration_ptcstat,function(x){ptcx %in% x})),],ptc=ptcx)
})
names(md_sp)<-ptcs_toget

lapply(md_sp,function(lsa)){
  lsa$df->dfa
  
}





