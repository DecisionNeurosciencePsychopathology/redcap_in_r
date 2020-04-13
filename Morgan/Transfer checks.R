#startup
startup()
#libraries

#get databases
 md<-bsrc.checkdatabase2(ptcs$masterdemo,forceskip = T,online = T,batch_size=1000L)
 pt<-bsrc.checkdatabase2(ptcs$protect, batch_size=500L, online=T)
 
#Set up idmap
 idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
 names(idmap)<-c("masterdemoid","wpicid","soloffid")
 
 
#Master demo checks
 #Registration Checks
   #Consent date checks for all protocols
    allreg<-data.frame(md$data[grepl("registration|reg",names(md$data))])
    names(allreg)[grepl("ptcstat",names(allreg))]<-gsub("registration_ptcstat___", "",names(allreg)[grepl("ptcstat",names(allreg))])
      #For all protocols, if they have a check for being in the protocol, they should have a consent date
        for (j in 1:length(names(allreg[5:18]))){
          x<-names(allreg[4+j])
          for(i in 1:nrow(allreg)){
            if(allreg[i, 4+j]=='1'){
              if(is.na(allreg[i, paste0("reg_condate_",x)]) | allreg[i, paste0("reg_condate_",x)]==""){
                message(paste0("Problem with ", names(allreg[4+j])))
                message(paste(allreg[i, "registration_redcapid"],
                              allreg[i,"registration_initials"],
                              allreg[i,paste0("reg_condate_",x)], sep=" "))}}}}
    #Termination checks
      #For all old protcols, if they are consented, they should also be terminated
       for (j in c(5:6,11:12,17:18)){
          x<-names(allreg[j])
          for(i in 1:nrow(allreg)){
            if(allreg[i, j]=='1'){
              if(is.na(allreg[i, paste0("reg_termdate_",x)]) | allreg[i, paste0("reg_termdate_",x)]==""){
                message(paste0("Problem with ", names(allreg[j])))
                message(paste(allreg[i, "registration_redcapid"],
                              allreg[i,"registration_initials"],
                              allreg[i,paste0("reg_termdate_",x)], sep=" "))}}}}
      #If someone is consented for P3, they should be terminated from P2
        for(i in 1:nrow(allreg)){
          if(allreg[i,"protect3"]==1 & allreg[i,"protect2"]==1){
            if(is.na(allreg[i, ("reg_termdate_protect2")]) | allreg[i, ("reg_termdate_protect2")]==""){
              message(paste(allreg[i, "registration_redcapid"],
                            allreg[i,"registration_initials"],
                            allreg[i,"reg_termdate_protect2"], sep=" "))}}}
#Protocol-Specific Checks
  #Protect only checks
    #Everyone in P2 has been consented for P2 and everyone consented has SOME data in P2
      #Get people in arm 2 of redcap protect and IDmap
      pt$data[which(grepl("arm_2",pt$data$redcap_event_name)),c("registration_redcapid","startup_init")]->p2records
      bsrc.findid(p2records, idmap=idmap, "registration_redcapid")->p2records
      #check who has no master demo id
      if(any(!p2records$ifexist)){
        message(paste("These IDs have NO master demo id, fix!!! They will be removed: ", 
                unique(p2records[which(!p2records$ifexist),"registration_redcapid"]), sep=" "))
      }
      #Get people consented for P2 in master demo
      allreg[which(allreg$protect2==1),"registration_redcapid"]->p2mdrecords
      #find out who doesn't match
      p2mdrecords[which(!p2mdrecords %in% p2records$masterdemoid)]
      p2records[which(!p2records$masterdemoid %in% p2mdrecords),"masterdemoid"]
    #Everyone in S1-P1 has been consented for S1-P1
    
  #Only protect people in at least 1 protocol
  pro<-pro[which(rowSums(pro[grepl("registration_ptcstat",names(pro))])>0),]
   
 
#Baseline checks- check that everyone's baseline is in the right place