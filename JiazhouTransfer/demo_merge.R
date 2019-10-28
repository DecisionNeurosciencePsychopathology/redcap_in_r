####Jiazhou's mergeing script

masterdemo <- bsrc.checkdatabase2(protocol = ptcs$masterdemo)

bs_demo<-bsrc.getform(protocol = ptcs$bsocial,formname = "record_registration",online = T,batch_size = 1000L)

