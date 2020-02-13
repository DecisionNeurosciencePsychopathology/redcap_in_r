#startup
startup()
#libraries

#get databases
 md<-bsrc.checkdatabase2(ptcs$masterdemo,forceskip = T,online = T,batch_size=1000L)
 pt<-bsrc.checkdatabase2(ptcs$protect, batch_size=500L, online=T)
 
#Baseline checks- check that everyone's baseline is in the right place