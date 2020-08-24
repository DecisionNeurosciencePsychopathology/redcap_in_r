#Step 1: Setup
  #Everyone has their own startup script, this always starts out the data pull
    startup()
  #Libraries
    library(na.tools)
    library(lubridate)
    library(dplyr)
    library(eeptools)
#Step 2: Gather data from each protocol
    #Bsrc.checkdatabase2 gathers data from each protocol. online=T allows you to get the most updated data (data in redcap right this second). 
      #Batch size refers to how big the batches of data are that you want (otherwise redcap may take forever to load them)
    #MD is master demographic (must load this for every data pull)
      md<-bsrc.checkdatabase2(ptcs$masterdemo,forceskip = T,online = T,batch_size=1000L)
    #PT is PROTECT
      pt<-bsrc.checkdatabase2(ptcs$protect, batch_size=500L, online=T)
    #KS is KSOCIAL
      ks<-bsrc.checkdatabase2(ptcs$ksocial, batch_size=100L, online=T)
    #Scan is the Scanning Database (soon to be retired)
      scan<-bsrc.checkdatabase2(ptcs$scandb, batch_size=100L, online=T)
    #BS is BSOCIAL
      bs<-bsrc.checkdatabase2(ptcs$bsocial, batch_size=1000L, online=T)
    #Ema_db is BSOCIAL EMA (although Morgan has a full EMA pull script)
      ema_db<-bsrc.attachngrab(rdpath = rdpaths$ema)
#Step 3: Figure out what IDs you need
    #This depends on what task you are using/what protocol you are gathering data from
      #This may be a list of IDs that have completed a behavioral task, or you may need to gather the list from a protocol
      #For my example, I am going to use those who completed the MCQ in PROTECT
      MCQ<-pt$data[c("registration_redcapid","redcap_event_name","bq_date",names(pt$data)[grepl("mcq",names(pt$data))])]
      MCQ[which(rowSums(is.na(MCQ[4:35]))<30),]->MCQ
      MCQ$registration_redcapid->IDs
#Step 4: ID match and check
    #No matter what data you are using, the IDs are unlikely to be the same across behavioral tasks as protocols so it NEEDS to be id matched
      #1. Set up ID map: this pulls each ID from the master demo and renames it to more intuitive ID names
        idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
        names(idmap)<-c("masterdemoid","wpicid","soloffid")
      #2. ID match using bsrc.findid (this function does not work with a list, so I will make a df with IDs in 1 column and nothing in the next)
        data.frame(IDs, extravar=NA)->IDs
        #Many times you the IDs will be coerced to a factor, therefore you have to change it to character
        as.character(IDs$IDs)->IDs$IDs
        bsrc.findid(IDs,idmap = idmap,id.var = "IDs")->IDs
      #3. Check that all IDs are matched in the master demo
        if(any(!IDs$ifexist)){
          stop("Some IDs that you gathered do not match the master demo, please check")
          print(IDs[which(!IDs$ifexist),])}
#Step 5. Grab exclusionary criteria for these IDs
    #Some of the people in your sample are bound to be ineligible for one reason or another.
    #Some things you need to check: 
        #1. They are consented to your protocol(s) of interest (registration_ptcstat tells use which protocols they are in)
        #2. They are not marked as having "unusable data" for those/that protocols
        #3. They are not terminated d/t being ineligible
    #For this example, I will want anyone consented to Suicide1-Protect3 to be eligible 
        MDids<-md$data[c("registration_redcapid", #ID
             names(md$data)[grepl("registration_ptcstat___protect",names(md$data))], #Ptcstat for P1-P3
             names(md$data)[grepl("registration_ptcstat___suicid",names(md$data))], #Ptcstat for S1-S2
             names(md$data)[grepl("reg_term_excl_protect",names(md$data))], #Exclude variable
             names(md$data)[grepl("reg_term_excl_suicid",names(md$data))], #Exclude variable
             names(md$data)[grepl("reg_term_reason_protect",names(md$data))], #Term reason variable
             names(md$data)[grepl("reg_term_reason_suicid",names(md$data))])] #Term reason variable
      #Get only IDs in your sample
        MDids[which(MDids$registration_redcapid %in% IDs$masterdemoid),]->newids
      #1.Should be consented for S1-P3, otherwise give an error
        if(any(!rowSums(newids[grepl("ptcstat",names(newids))]==1)>0)){
          stop("Someone is not consented into a protect protocol, look into them")
          print(newids[which(!rowSums(newids[grepl("ptcstat",names(newids))]==1)>0),"registration_redcapid"])}
      #2. Should not be excluded from any protocol, otherwise give list of IDs and remove
        if(any(rowSums(newids[grepl("excl",names(newids))]==1 & !is.na(newids[grepl("excl",names(newids))]))>0)){
          message("There are people to exclude, here are their IDs")
          newids[which(rowSums(newids[grepl("excl",names(newids))]==1 & !is.na(newids[grepl("excl",names(newids))]))>0),"registration_redcapid"]->excludedids
          print(excludedids)
          newids[-which(newids$registration_redcapid %in% excludedids),]->newids}
      #3. Should not be terminated from any protocol d/t being ineligible (term reason=3 for protect)
        if(any(newids[grepl("term",names(newids))]==3 & !is.na(newids[grepl("term",names(newids))]))){
          message("Some people have been terminated d/t being ineligible, here are their IDs")
          newids[which(rowSums(newids[grepl("term",names(newids))]==3 & !is.na(newids[grepl("term",names(newids))]))>0),"registration_redcapid"]->termedids
          print(termedids)
          newids[-which(newids$registration_redcapid %in% termedids),]->newids}
#Step 6. Grab demographic information for these IDs
    #This will always be listed in the master demographic. Most commonly pulled information includes: 
        #Regular demo info: ID, group, ethnicity, gender, education, age at baseline or event, marital status, consent date, race
        demo<-md$data[c(paste0("registration_",c("redcapid","group","hispanic","gender","edu","dob","marrs")),
                      paste0("reg_condate_",c("suicide","suicid2","protect","protect2","protect3")),
                      names(md$data)[grepl("race",names(md$data))])]
        #Only your Ids
        demo[which(demo$registration_redcapid %in% newids$registration_redcapid),]->demo
        #Change race from checkbox to noncheckbox (6=multirace)
        demo$race=ifelse(rowSums(demo[grepl("race",names(demo))]=='1')==0,
          NA,ifelse(rowSums(demo[grepl("race",names(demo))]=='1')>1,
          6, ifelse(demo$registration_race___1==1,
          1, ifelse(demo$registration_race___2==1,
          2, ifelse(demo$registration_race___3==1,
          3, ifelse(demo$registration_race___4==1,
          4, ifelse(demo$registration_race___5==1,
          5,NA)))))))
        #Get first consent date (for Protect only)
        #change dates to date
          ymd(demo$reg_condate_suicide)->demo$reg_condate_suicide
          ymd(demo$reg_condate_suicid2)->demo$reg_condate_suicid2
          ymd(demo$reg_condate_protect)->demo$reg_condate_protect
          ymd(demo$reg_condate_protect2)->demo$reg_condate_protect2
          ymd(demo$reg_condate_protect3)->demo$reg_condate_protect3
        #get variable which is first date (called consentdate)
          demo<-mutate(demo, consentdate=pmin(reg_condate_suicide, reg_condate_suicid2, reg_condate_protect, 
                                 reg_condate_protect2, reg_condate_protect3, na.rm=T))
        #calculate age (I am using consent date but this could be based on any variable)
          is.Date(demo$consentdate)
          as.Date(demo$registration_dob)->demo$registration_dob
          demo<-mutate(demo, blage=age_calc(registration_dob, enddate=consentdate, units="years",precise=F))
    #Suicide attempt information (also always listed in master demo form sahx)
      #During almost every data pull, there are 2 things that people usually want:
      #1) Age at first attempt
      #2) Highest lethality attempt (this may be highest ever or highest up to a certain point( highest until MCQ date)
        #1. grab the variables you need
          sahx<-md$data[c("registration_redcapid", names(md$data)[grepl("sahx_sadate",names(md$data))],
                          names(md$data)[grepl("sahx_lr",names(md$data))])]
            #dates
            gather(data=sahx[c("registration_redcapid",
                      names(sahx)[grepl("date", names(sahx))])], key="att", value="date",-registration_redcapid)->dates
            gsub("sahx_sadate_","",dates$att)->dates$att
            #lethalities
            gather(data=sahx[c("registration_redcapid",
                          names(sahx)[grepl("lr", names(sahx))])], key="att", value="lethality",-registration_redcapid)->leths
            gsub("sahx_lr_","",leths$att)->leths$att
          merge(dates,leths, all=T)->sahx
          sahx[which(is.na(sahx$date)),"date"]<-""
          ymd(sahx$date)->sahx$date
          sahx[-which(is.na(sahx$date) & is.na(sahx$lethality)),]->sahx
        #2. Get date of first att
        sahx %>% group_by(registration_redcapid) %>% mutate(firstatt=min(na.omit(date)))->sahx
        #3. Get highest lethality attempt
        sahx %>% group_by(registration_redcapid) %>% mutate(hlatt=max(na.omit(lethality)))->sahx
        sahx[which(sahx$hlatt==-Inf),"hlatt"]<-NA
        #4. Merge w/ original demo (only keeping IDs in original demo)
        merge(demo, sahx, all.x = T)->demo2
        #5. Calculate age of first att
        demo2 %>% group_by(registration_redcapid) %>% mutate(age1statt=ifelse(is.na(firstatt),NA, 
                                                      age_calc(registration_dob, firstatt, "years", precise=F)))->demo2
 #Remove unnecessary demographic variables, also rename to better variables
    gsub("registration_","",names(demo2))->names(demo2)
    demo2[!grepl("___|condate",names(demo2))]->demo2
    demo2[c("att","date","lethality","multirace","firstatt")]<-NULL
    demo2[-which(duplicated(demo2$redcapid)),]->demo2
        
        
        
        
        
        
        
        
        
        