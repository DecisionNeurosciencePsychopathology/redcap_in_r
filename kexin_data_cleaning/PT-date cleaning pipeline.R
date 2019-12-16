## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
source('~/Documents/github/UPMC/startup.R')
rootdir="~/Box/skinner/data/Redcap Transfer/All protect data/"
allsub<-read.csv(paste0(rootdir,"ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt2.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
var_map$baseline<-"TRUE"
var_map$baseline<-as.logical(var_map$baseline)
var_map_ham<-subset(var_map,Form_name=="HRSD and BPRS") # seperate ham from ther var map 
var_map<-subset(var_map,!Form_name=="HRSD and BPRS") # var map w/o form HRSD and BPRS
combine<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/combing forms.csv',stringsAsFactors = FALSE)
combine[which(combine=="",arr.ind = T)]<-NA

#Initialize reports 
log_out_of_range <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                               which_form=as.character(),comments=as.character(),stringsAsFactors = F) #Report out-of-range values 
log_replace <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report wrong values/datatypes, correct and report 
log_comb_fm <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 
log_comb_fm2 <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                           which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 
#deleted_rows<-list()
#comb_rows<-list() # IDDATE absent in at least one form when combining 
#####################################start of the function#########################################
# rctransfer.dataclean <- function(
# [VARIABLES]
#curdb = bsoc
#protocol.cur <- ptcs$bsocial
#db = 
#pt<-bsrc.checkdatabase2(protocol = ptcs$protect)

forms = NULL # A vector. must be exactly the same as the a subset of the form names in the variable mapping. Case sensitive. Space sensitive. 
skipotherforms = TRUE
#replace_999 = TRUE # by defult, replace all 999 with NA 
remove_dupid = FALSE # if T, remove all rows that involve duplicated IDs  
replace_w_na = FALSE
#) {

## verify Morgan's var_map. 
####for the col is.box. NA should mean represent unecessary variables. i.e. 
# if redcap_var and access_var both exist, is.checkbox cannot be NA
na.omit(var_map$redcap_var)[which(!na.omit(var_map$redcap_var)%in%colnames(pt$data))]

#var_map<-var_map[which(is.na(var_map$redcap_var)|var_map$redcap_var%in%colnames(pt$data)),] #temperary remove rc var that cannot be found in the protect data
chckmg<-subset(var_map,select = c('redcap_var','access_var'),is.na(is.checkbox))
chckmg[which(!is.na(chckmg$redcap_var)&(!is.na(chckmg$access_var))),] #shoule give us nothing
# vice versa 
chckmg<-subset(var_map,select = c('redcap_var','access_var','is.checkbox','FIX'),!is.na(is.checkbox)&as.logical(FIX))
#which(is.na(chckmg),arr.ind = T) # should give us nothing. if yes, try run the following line of code 
sum(is.na(var_map$is.checkbox)) #of unecessary variabels (based on rows. duplicates included)
#var_map$is.checkbox[which(is.na(var_map$redcap_var)&!var_map$is.checkbox)]<-NA
#var_map$is.checkbox[which(is.na(var_map$access_var)&!var_map$is.checkbox)]<-NA
#sum(is.na(var_map$is.checkbox)) #of unecessary variabels (based on rows. duplicates included)
####remove all blank rows 

# PREPARE variable: access forms
all_formnm<-with(var_map,gsub(".csv","",unique(na.omit(path)))) #get all redcap formnames  
if (is.null(forms)){
  forms<-all_formnm
} else {  
  # check if form names can be found in variable mapping   
  if (!is.vector(forms)){stop(message('`forms` must be a vector. Use "c("example1","example2")" or "example".'))}
  if (sum(!forms %in% all_formnm)>1) {
    stop(message('One of the formnames cannot be found in the variable mapping. Please note that form names are case sensitive and space sensitive.'))
  }
  # removed duplicates and NA from `forms`
  forms<-unique(forms[!is.na(forms)])
} 
rm(all_formnm)

## PREPARE functions
# make a fun to report abnormal values 
report_wrong <- function(id = NA, which_var = NA, wrong_val = NA, which_form = NA, comments = NA, 
                         report = wrong_val_report,rbind=T){
  new_repo <- data.frame(id = id, stringsAsFactors = F)
  new_repo[1:nrow(new_repo),2]<- which_var
  new_repo[1:nrow(new_repo),3]<- wrong_val
  new_repo[1:nrow(new_repo),4]<- which_form
  new_repo[1:nrow(new_repo),5]<- comments
  colnames(new_repo)<-c('id','var_name','wrong_val', 'which_form','comments')
  ifelse(rbind,return(rbind(report,new_repo)),return(new_repo))
}

#check double entries (one redcap variabels exist in multiple forms) across froms  
repeatedrcvar<-unique(na.omit(var_map$redcap_var[duplicated(var_map$redcap_var)]))
dbentry<-subset(var_map,redcap_var%in%repeatedrcvar&!is.na(redcap_var)&!access_var%in%c("ID","MISSCODE","CDATE"),select = c("Form_name","redcap_var","access_var","path"))
dbentry #will check double entry after data cleaning  

for (form_i in 1:length(forms)) {
  #STEP1: Select a Access form - baseline(bl) first and then other forms- match with redcap variables, splited ordinary variables with checkbox variables, removed calculated variables 
  STEP1<-function(){
    #STEP1.1 Select a RC form. Check if multiple origianl forms need to be combined into one form 
    formname <- forms[form_i] 
    cat(paste0("Cleaning form: ",formname," now...\n"))
    fm_dir<-paste0(formname,".csv")
    vm<-subset(var_map, path==fm_dir) #subset of var mapping for the current form
    if(!(sum(vm$baseline)==0|sum(vm$baseline)==nrow(vm))){stop(message("check the column 'baseline' in the var map"))
    }else{ifbl<-any(vm$baseline)}
    
    if(!skipotherforms|ifbl){ # skip the form if skipotherforms is T AND ifbl is F
      acvar_nonch<-with(vm,split(access_var,is.checkbox))$'FALSE' #non-checkbox var
      acvar_chk<-unique(na.omit(with(vm,split(access_var,is.checkbox))$'TRUE')) #checkbox var
      wrongvar<-intersect(acvar_chk,acvar_nonch) # check that no access var is both checkbx var and nonchck var 
      if(length(wrongvar)>0){stop(paste0("Access variable ",paste(wrongvar,collapse = ", ")," in form ", formname, "is marked as both checkbox and non-chckbox variable in the var map."))}
      if (any(is.na(vm$path))){
        stop(message('At least one row in var mapping does not give the path of directory for the original forms')) # path cannot be NA
      }else{if(any(!file.exists(paste0(rootdir,fm_dir)))){stop(message('At least one row of path in var mapping does not exist.'))}}#path must be valid
      #STEP1.2 Get raw. Grab forms, remove unecessary people and variables
      RAWDATA <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)#grab form 
      colnames(RAWDATA)<-gsub("^X","",colnames(RAWDATA)) # raname colnames to remove "X" in "X1", "X2"...
      RAWDATA<-RAWDATA[which(RAWDATA$ID%in%allsub$ID),] #remove people not in our study
      w_acvar<-setdiff(colnames(RAWDATA),vm$access_var)#all access variesbles should be in var map
      if(length(w_acvar)>0){message(paste("Warning:",paste(w_acvar,collapse = ","),"cannot be found in the var_map."))} # report ^
      w_rcvar<-setdiff(na.omit(vm$access_var),colnames(RAWDATA))# all access_var in var mapping should be in actual Access forms
      if(length(w_rcvar)>0){stop(message(paste("Stop:",paste(w_rcvar,collapse = ", "),"in the var_map does not match any variables in the forms.")))} # report^
      RAWDATA<-RAWDATA[,which(colnames(RAWDATA)%in%c(acvar_nonch,acvar_chk))] #remove unncessary var 
      #STEP1.3 no NA in ID or CDATE. create IDDATE. IDDATE must be unique
      RAWDATA[which(RAWDATA=="",arr.ind = T)]<-NA
      if(any(is.na(RAWDATA$ID)|is.na(RAWDATA$CDATE))){stop(message(paste("NA in ID or CDATE of RAWDATA. Form:",formname)))}
      if("CDATE"%in%colnames(RAWDATA)){ #if the dataframe has CDATE
        #confm<-readline(prompt = paste0("Enter T to confirm CDATE '",RAWDATA[1,"CDATE"],"' follows the format %mm/%dd/%yy: ")) # confirm the format of CDATE
        if(as.logical(confm)){
          RAWDATA$CDATE<-as.Date(RAWDATA$CDATE,format = "%m/%d/%y")
          RAWDATA$CDATECOPY<-RAWDATA$CDATE # create a col CDATECOPY so that after var mapping the form still has a col called CDATE 
        }}else{message(paste0("Warn: ",formname," does not have CDATE."))}
      RAWDATA$IDDATE<-paste0(RAWDATA$ID,RAWDATA$CDATE)
      RAWDATA<-unique(RAWDATA) #remove duplicated rows before checking duplicated IDDATE 
      if(ifbl){
        dup_id<-unique(RAWDATA[which(duplicated(RAWDATA$ID)),"ID"])# shoule have no duplicates in ID
        if(length(dup_id)>0){
          message(paste0("Warn: ",formname," is a baseline form and has duplicated ID. Please refer to formname_dup_id_rows.csv. The rows are removed."))
          reportdup<-RAWDATA[which(RAWDATA$ID%in%dup_id),]
          reportdup<-reportdup[order(reportdup$ID),];reportdup[which(is.na(reportdup),arr.ind = T)]<-""
          write.csv(reportdup,file = paste0("~/Documents/github/UPMC/TRANSFER/PT/dup_id/",formname,"_dup_id_rows.csv"))
          RAWDATA<-RAWDATA[-which(RAWDATA$ID%in%dup_id),]}      #remove duplicated rows 
      }else{
        dup_id<-unique(RAWDATA[which(duplicated(RAWDATA$IDDATE)),"IDDATE"])# shoule have no duplicates in IDDATE
        if(length(dup_id)>0){
          message(paste0("Warn: ",formname," has duplicated IDDATE. Please refer to formname_dup_id_rows.csv. The rows are removed."))
          reportdup<-RAWDATA[which(RAWDATA$IDDATE%in%dup_id),]
          reportdup<-reportdup[order(reportdup$IDDATE),];reportdup[which(is.na(reportdup),arr.ind = T)]<-""
          write.csv(reportdup,file = paste0("~/Documents/github/UPMC/TRANSFER/PT/dup_id/",formname,"_dup_id_rows.csv"))
          RAWDATA<-RAWDATA[-which(RAWDATA$IDDATE%in%dup_id),]}      #remove duplicated rows
      }
      #SPECIAL for SCID: add back some records with dup id that Morgan manually find 
      if (formname%in%c("A_SCIDIV","A_SCIDCHRON","L_CONDIAG")){
        special<-read.csv(paste0("~/Documents/github/UPMC/TRANSFER/PT/dup_id/DEC12 MANUALLY/",formname,"_special_dup_id.csv"),stringsAsFactors = F)
        special<-subset(special,ifkeep=="TRUE",select = 1:(ncol(special)-1))[-1]
        special$CDATE<-as.Date(special$CDATE,format = "%m/%d/%y");special$CDATECOPY<-as.Date(special$CDATECOPY,format = "%m/%d/%y")
        if(!(any(duplicated(special$ID))|any(special$ID%in%RAWDATA$ID))){
          RAWDATA<-rbind(RAWDATA,special)
          message(paste0("Note: added back observations Morgan identified manually on Dec 12."))
        }else{stop(message("Something is wrong"))}
      }
      #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx vars to df: 'raw_chk'
      if(!is.null(acvar_chk)){
        raw_nonch<-RAWDATA[,-which(colnames(RAWDATA)%in%acvar_chk)] #keep only non-checkbx variables 
        raw_chk<-RAWDATA[c("ID","CDATE","IDDATE",acvar_chk)]
      }else{raw_nonch<-RAWDATA}
      #STEP1.5 remove calculated fields 
      cal_var<-subset(vm,fix_what=='calculated_field')$access_var
      if(length(cal_var)>0){raw_nonch<-raw_nonch[,-which(colnames(raw_nonch)%in%cal_var)]}
      #STEP1.6 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames NOTE: ONE ACVAR CAN MATCH MULTIPLE RCVAR
      VMAP<-unique(subset(vm,select=c(access_var,redcap_var),is.checkbox=='FALSE'&!is.na(redcap_var)))
      if(any(duplicated(na.omit(VMAP$access_var)))){message(paste("Variable mapping... \nWarning: some access variable matches multiple redcap variabels in form",formname))} #check if one ac var matches multiple rc var 
      colnames(raw_nonch)<-plyr::mapvalues(colnames(raw_nonch),from = VMAP$access_var, to = VMAP$redcap_var,warn_missing = F)
      colnames(raw_nonch)[grep("^CDATE",colnames(raw_nonch))]<-"CDATE"
      if(!all(colnames(raw_nonch)%in%c(VMAP$redcap_var,"CDATE","IDDATE","MISSCODE"))){stop(message(paste0(formname," has an error when checking: new colnames should contain only CDATE, IDDATE, and redcap variables")))} # check: new colnames should contain only CDATE, IDDATE, and redcap variables 
      raw_nonch<-cbind(raw_nonch[,-which(colnames(raw_nonch)=="CDATE")],CDATE=raw_nonch$CDATE) # keep only one col of CDATE
      if(any(duplicated(colnames(raw_nonch)))){stop(message(paste0("Stop: ",formname,": Duplicated colnames.")))}
      #STEP1.7 copy the column CDATE and rename as cdate_formname
      raw_nonch<-cbind(raw_nonch,newcol=raw_nonch$CDATE)
      colnames(raw_nonch)<-gsub("newcol",tolower(paste0("cdate_",formname)),colnames(raw_nonch))
      #STEP1.8 SPECIAL for some forms that have "condition" issue, merge the checkbox df with certain non-chk access var. 
      if ("condition" %in% vm$fix_what){
        raw_chk<-cbind(raw_chk,RAWDATA[,subset(vm,fix_what=="condition",select = value1)[[1]]])
      }
      
      cat(paste0(formname,": STEP1 done.\n"))
      vm<<-vm
      formname<<-formname
      acvar_chk<<-acvar_chk
      RAWDATA<<-RAWDATA
      #deleted_rows<<-deleted_rows
      if(!is.null(acvar_chk)){raw_chk<<-raw_chk}
      raw_nonch<<-raw_nonch
      log_replace<<-log_replace
      log_comb_fm<<-log_comb_fm
      ifbl<<-ifbl
      skipotherforms<<-skipotherforms
      
    }else{cat(paste0(formname," is not a baseline form. Skiped it.\n"))
      ifbl<<-ifbl
      skipotherforms<<-skipotherforms}
    
  }# the function is writen and editted in another script. Above is a copy of the script
  STEP1() # get 'raw_nonch': redcap variables, 
  if(!skipotherforms|ifbl){ # skip the form if skipotherforms is T AND ifbl is F
    ##STEP4 fix data with systematic issues (eg: shifted range) identified in 'var_map'
    STEP4<-function(){
      fresh_nonch<-raw_nonch
      cat(paste("#",form_i,formname,"- performning STEP4 now...\n"))
      #STEP4.01 range_fix: range in access is not the same as range in redcap, specifies first access variable, then redcap variable to change to
      fixmap<-unique(subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)))
      if(nrow(fixmap)>0) {for (step4_i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
       valuemap<-matrix(gsub(" ","",strsplit(fixmap$instructions[step4_i],",")[[1]]),ncol = 2,byrow = T)
        if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
          message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
        }else{
          fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = F)
        }}}
      #STEP4.02 range_allowed: The range in Redcap allows more values than we accept from what should have been the range in redcap. Specifies the new range
      fixmap<-unique(subset(vm,fix_what=='range_allowed',select = c(redcap_var,instructions)))
      if(nrow(fixmap)>0) {for (step4_i in 1:nrow(fixmap)){ #if there's 'range_allowed' problem, fix the problem one variable by one var
        thecol<-fresh_nonch[fixmap$redcap_var[step4_i]] # the col with the problem 
        if(!is.numeric(thecol[[1]])){ # values in the col should be all numeric (or NA)
          stop(message(paste0('Form "',formname,'" has non-numeric values in column "',fixmap$redcap_var[step4_i],'" so "range_allowed" cannot be fixed')))
        }else{
          eval(parse(text=paste0('rg<-seq(',fixmap$instructions[step4_i],')'))) #get rg: range specified in var_map
          row_i<-which(!((thecol[[1]] %in% rg) | is.na(thecol[[1]]))) # report values that is not in the range. NA is acceptable 
          if (length(row_i)==0){
            cat(paste('Fixing issue "range_allowed" GOOD.:', formname,fixmap$redcap_var[step4_i],'are within the range (NA is allowed).\n'))
          }else{
            log_out_of_range<-report_wrong(report = log_out_of_range,id=ifelse(ifbl,fresh_nonch[row_i,1],fresh_nonch[row_i,"IDDATE"]),which_form = formname, which_var = fixmap$redcap_var[step4_i],wrong_val = thecol[row_i,1],
                                           comments = 'range_allowed')
            if(replace_w_na){ #Replace out-of-range values with NA 
              fresh_nonch[row_i,fixmap$redcap_var[step4_i]]<-NA
              log_replace<-report_wrong(id=ifelse(ifbl,fresh_nonch[row_i,1],fresh_nonch[row_i,"IDDATE"]),which_var = fixmap$redcap_var[row_i], wrong_val = thecol[row_i,1],which_form = formname,comments = 'Fixing "range_allowed": Out of the range.The value is replaced with NA',report = log_replace)
              message('Fixing issue "range_allowed": Some values are out of range. Refer to log_out_of_range for more details. The out-of-range values are replcaed with NA.')}
          }}}}
      #STEP4.03 date: must be converted to date (YYYY-MM-DD)
      fixmap<-unique(subset(vm,fix_what=='date',select = c(redcap_var,instructions)))
      if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
        fresh_nonch[fixmap$redcap_var[step4_i]]<-as.Date(as.character(fresh_nonch[[fixmap$redcap_var[step4_i]]]),format = "%m/%d/%y")
        if(!all(na.omit(as.integer((fresh_nonch[[fixmap$redcap_var[step4_i]]]-Sys.Date())/365))%in%seq(-100,100))){stop(message("Something went wrong when changing the format of date"))} # check that the new date is in reasonable range: +-100years of today
      }}
      #STEP4.04 value_set: import this value for EVERYONE who we import this form for
      fixmap<-unique(subset(vm,fix_what=='value_set',select = c(redcap_var,instructions)))
      if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
        fresh_nonch<-cbind(fresh_nonch,newcolumn=fixmap$instructions[step4_i])
        colnames(fresh_nonch)<-gsub("^newcolumn",fixmap$redcap_var[step4_i],colnames(fresh_nonch))
      }}
      if(nrow(fixmap)>0) { # if there's 'value_set' problem
        fresh_nonch$"ipde_excludeitem"<-replicate(nrow(fresh_nonch),1)
      }
      #STEP4.05 value_set2: if value 1 is not null, value 2, otherwise, value 3
      fixmap<-unique(subset(vm,fix_what=='value_set2',select = c(redcap_var,instructions,value1,value2,value3)))
      if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
        fresh_nonch<-cbind(fresh_nonch,newcolumn=as.integer(fixmap[step4_i,c("value2","value3")[as.integer(is.na(fresh_nonch[[fixmap$value1[step4_i]]]))+1]]))
        colnames(fresh_nonch)<-gsub("newcolumn",fixmap$redcap_var[step4_i],colnames(fresh_nonch))
      }}
      
      fresh_nonch<<-fresh_nonch
      log_out_of_range<<-log_out_of_range
      log_replace<<-log_replace
      cat(paste0(formname,": STEP4 done.\n"))
    }
    STEP4()

    ##STEP5 
    #Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
    STEP5<-function(){
      cat(paste("#",form_i,formname,"- performning STEP5 now...\n"))
      for (j in 1:length(colnames(fresh_nonch))) { # get the range by col (variable) and then get the rows of out-of-range values
        if(!colnames(fresh_nonch)[j]%in%vm$redcap_var){next()} #skip access var in the current form 
        if (grepl("___",colnames(fresh_nonch)[j])){next()} # skip checkbox var brougt in during fixing "value_set2"
        rg<-bsrc.getchoicemapping(variablenames = colnames(fresh_nonch)[j],metadata = pt$metadata)[[1]] # get the range 
        if(is.null(rg)){log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname, id='OKAY-NO_RANGE',which_var = colnames(fresh_nonch)[j],comments = 'This variable has no range');next()} # variable should have a range 
        #get the rows of out-of-range values; replace and report out-of-range values
        i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]]))) # report values that is not in the range. NA is acceptable 
        if (length(i)==0){
          cat(paste('GOOD. All values of', formname, colnames(fresh_nonch)[j],'are within the range.\n'))
          log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname,id='GOOD',which_var = colnames(fresh_nonch)[j],comments = 'GOOD. All values are within the range.')
        }else{
          log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[i,1],which_form = formname, which_var = colnames(fresh_nonch)[j],wrong_val = fresh_nonch[i,j],
                                         comments = paste0('Correct range: ', do.call('paste',as.list(rg)),'. Not replaced with NA.'))
          #log_replace<-report_wrong(id=fresh_nonch[i,1],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i,j], which_form = formname,comments = 'Step5: Out of range values.',report = log_replace)
          #fresh_nonch[i,j]<-NA
          message(paste0('Warn: Some values from ',formname," ", colnames(fresh_nonch)[j], ' are out of range. Refer to log_out_of_range for more details.'))
        }
      }
      fresh_nonch<<-fresh_nonch
      log_out_of_range<<-log_out_of_range
      log_replace<<-log_replace
      cat(paste0(formname,": STEP5 done.\n"))
    }
    STEP5()
    
    ##STEP6 identify systematic issues based on the log by calculating the number of observations that have the same issue. 
    #If almost all of them have the same issue it may be very likely to be systematic. 
    
    ##STEP7 for checkbox
    STEP7<-function(){
      cat(paste("#",form_i,formname,"- performning STEP7 now...\n"))
      fresh_chk<-raw_chk
      vm<-subset(vm,is.checkbox=="TRUE") #subset of var_map where is.checkbox = T
      
      #STEP7.1
      #####need to check the values of ac var first!????
      #STEP7.2 redcap checkbox
      vm_rcchk<-subset(vm,fix_what=="redcap_check") # subset of vm of redcap_check var
      if (nrow(vm_rcchk)>0){
        #STEP7.2.1 cbind the original df with an empty dataframe containing rc col
        newcolname<-append(colnames(fresh_chk),unique(vm_rcchk$redcap_var))#get the colname for the new df
        fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_rcchk$redcap_var))))) 
        colnames(fresh_chk)<-newcolname
        #STEP7.2.2 fill in redcap cols
        #for each row of fresh_chk, if values of acvar == x1 then values of rcvar == y1
        for (df_i in 1:nrow(fresh_chk)) { # for every observation, [swtich values from access forms to coresponding values in redcap]  
          for (vm_i in 1:nrow(vm_rcchk)){ #for every row in var_map (i.e. for every pair of [accessvalue,redcapvalue]), replace access value with redcap value
            acvar<-vm_rcchk$access_var[vm_i]
            rcvar<-vm_rcchk$redcap_var[vm_i]
            if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
              iftrue<-as.numeric(fresh_chk[df_i,acvar])==vm_rcchk$value1[vm_i]
              fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_rcchk$value2[vm_i],0)
            }}}
      }
      #STEP7.3 access checkbox
      vm_achk<-subset(vm,fix_what=="access_check") # subset of vm of redcap_check var
      if (nrow(vm_achk)>0){
        #STEP7.3.1 cbind the original df with an empty dataframe containing rc col
        newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
        fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
        colnames(fresh_chk)<-newcolname
        #STEP7.3.2 fill in redcap cols
        #for each row of fresh_chk, if values of acvar contains x1 then values of rcvar == x2, otherwise rcvar == x3
        for (df_i in 1:nrow(fresh_chk)){
          for (vm_i in 1:nrow(vm_achk)){
            acvar<-vm_achk$access_var[vm_i]
            rcvar<-vm_achk$redcap_var[vm_i]
            if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
              iftrue<-grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
              fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],vm_achk$value3[vm_i])
            }}}
      }
      #STEP7.4 both_check1 both access and redcap are checkboxes, not case sensitive, if not in value1, then value 2 
      vm_achk<-subset(vm,fix_what=="both_check1") # subset of vm of redcap_check var
      if (nrow(vm_achk)>0){
        #STEP7.4.1 cbind the original df with an empty dataframe containing rc col
        newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
        fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
        colnames(fresh_chk)<-newcolname
        #STEP7.4.2 fill in redcap cols
        #for each row of fresh_chk, if not contain value1, then value 2 
        for (df_i in 1:nrow(fresh_chk)){
          for (vm_i in 1:nrow(vm_achk)){
            acvar<-vm_achk$access_var[vm_i]
            rcvar<-vm_achk$redcap_var[vm_i]
            if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
              iftrue<-!grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
              fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],0)
            }}}
      }
      #STEP7.5 both_check2 both access and redcap are checkboxes, not case sensitive, if in value1, then value 2 
      vm_achk<-subset(vm,fix_what=="both_check2") # subset of vm of redcap_check var
      if (nrow(vm_achk)>0){
        #STEP7.5.1 cbind the original df with an empty dataframe containing rc col
        newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
        fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
        colnames(fresh_chk)<-newcolname
        #STEP7.5.2 fill in redcap cols
        #for each row of fresh_chk, if contain value1, then value 2 
        for (df_i in 1:nrow(fresh_chk)){
          for (vm_i in 1:nrow(vm_achk)){
            acvar<-vm_achk$access_var[vm_i]
            rcvar<-vm_achk$redcap_var[vm_i]
            if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
              iftrue<-grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
              fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],0)
            }}}
      }
      #STEP7.6 condition if value1=value2, match here, otherwise assign value 3
      vm_achk<-subset(vm,fix_what=="condition") # subset of vm of redcap_check var
      if (nrow(vm_achk)>0){
        #STEP7.6.1 cbind the original df with an empty dataframe containing rc col
        newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
        fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
        colnames(fresh_chk)<-newcolname
        #STEP7.6.2 fill in redcap cols
        #for each row of fresh_chk, if value1=value2, match here, otherwise assign value 3
        for (df_i in 1:nrow(fresh_chk)){
          for (vm_i in 1:nrow(vm_achk)){
            #for (vm_i in 1:12){
            acvar0<-vm_achk$value1[vm_i]
            acvar<-vm_achk$access_var[vm_i]
            rcvar<-vm_achk$redcap_var[vm_i]
            if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
              iftrue<-fresh_chk[df_i,acvar0]==vm_achk$value2[vm_i]
              fresh_chk[df_i,rcvar]<-ifelse(iftrue,fresh_chk[df_i,acvar],vm_achk$value3[vm_i])
            }}}
      }
      #STEP7.7 SPECIAL special_6 range fix and make copies of this variable
      vm_achk<-subset(vm,fix_what=="special_6") # subset of vm of redcap_check var
      if (nrow(vm_achk)>0){
        #STEP7.7.1 cbind the original df with an empty dataframe containing rc col
        newcolname<-append(colnames(fresh_chk),vm_achk$redcap_var)#get the colname for the new df
        fresh_chk<-cbind(fresh_chk,fresh_chk$EATYP,fresh_chk$EATYP,fresh_chk$EATYP) 
        colnames(fresh_chk)<-newcolname
        #STEP7.7.1 range fix 
        for (step4_i in 1:nrow(vm_achk)){
          valuemap<-matrix(gsub(" ","",strsplit(vm_achk$instructions[step4_i],",")[[1]]),ncol = 2,byrow = T)
          fresh_chk[vm_achk$redcap_var[step4_i]]<-plyr::mapvalues(fresh_chk[[vm_achk$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = F)
        }
      }
      
      fresh_chk<<-fresh_chk
      cat(paste("#",form_i,formname,"- STEP7 done.\n"))
    }
    if(!is.null(acvar_chk)){STEP7()}
    
    
    #STEP8 for checkbox -- match checkbox variabels with other variabels using matching_id
    STEP8<-function(){
      if(is.null(acvar_chk)){
        fresh_alldata<-fresh_nonch
      }else{
        fresh_alldata<-dplyr::inner_join(fresh_nonch,fresh_chk[-1],by = "matching_id")
        if(!max(fresh_alldata$matching_id)==nrow(fresh_alldata)){stop(message("The last check: something is wrong."))}
        fresh_alldata<<-fresh_alldata
        message("STEP8 done.")
      }
      fresh_alldata<<-fresh_alldata
      message(paste0(formname,": STEP8 done. - DATA CLEANING COMPLETED!"))
    }
    STEP8()
    
    assign(paste0("df_",form_i),fresh_alldata)
    write.csv(unique(fresh_alldata),file = paste0("~/Documents/github/UPMC/TRANSFER/form_",form_i,".csv"))
    #  write.csv(unique(log_comb_fm),file = paste0("~/Documents/github/UPMC/TRANSFER/log_comb_fm_",form_i,".csv"))
    #  write.csv(unique(log_out_of_range),file = paste0("~/Documents/github/UPMC/TRANSFER/log_out_of_range_",form_i,".csv"))
    #  write.csv(unique(log_replace),file = paste0("~/Documents/github/UPMC/TRANSFER/log_replace_",form_i,".csv"))
  }}

# check double entries across forms (dbentry)

# check IDDATE following combining_form.csv (eg: some forms should have identical IDDATE, every IDDATE has all forms?). Refer to combine and check forms.R


write.csv(unique(log_comb_fm),file = paste0("~/Documents/github/UPMC/TRANSFER/log_comb_fm.csv"))
write.csv(unique(log_out_of_range),file = paste0("~/Documents/github/UPMC/TRANSFER/log_out_of_range.csv"))
write.csv(unique(log_replace),file = paste0("~/Documents/github/UPMC/TRANSFER/log_replace_.csv"))
for (del_i in 1:length(deleted_rows)){
  write.csv(deleted_rows[[del_i]],file = paste0("~/Documents/github/UPMC/TRANSFER/DeletedRows_",names(deleted_rows)[del_i],".csv"))
  rm(del_i)
}
newdeleted<-do.call("rbind",lapply(deleted_rows[4:13],function(x){x[1:2]}))
write.csv(newdeleted,file = paste0("~/Documents/github/UPMC/TRANSFER/DeletedRows_IPDE.csv"))
#}




#####################################end of the function#########################################



