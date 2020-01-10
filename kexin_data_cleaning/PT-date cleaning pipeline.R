## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
source('~/Documents/github/UPMC/startup.R')
############startuptransfer#############
rootdir="~/Box/skinner/data/Redcap Transfer/All protect data/"
logdir="~/Documents/github/UPMC/TRANSFER/PT/dup_id/" # directory of folder to save duplicated IDs that are removed from database
allsub<-read.csv(paste0(rootdir,"ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
#var_map$path<-gsub("\"","",var_map$path) #temperary: remove quotation marks from paths 
var_map$baseline<-"FALSE"#temperary
var_map$baseline<-as.logical(var_map$baseline)#temperary
#var_map<-subset(var_map,!is.na(path)) # temperary remove all rows without paths 
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
log_branching <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                            which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 
#deleted_rows<-list()
#comb_rows<-list() # IDDATE absent in at least one form when combining 
#####################################start of the function#########################################
# rctransfer.dataclean <- function(
# [VARIABLES]
#curdb = bsoc
#protocol.cur <- ptcs$bsocial
#db = 
pt<-bsrc.checkdatabase2(protocol = ptcs$protect)

forms = NULL # A vector. must be exactly the same as the a subset of the form names in the variable mapping. Case sensitive. Space sensitive. 
skipotherforms = FALSE
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
chckmg[which(!is.na(chckmg$redcap_var)&(!is.na(chckmg$access_var))),] #shoule give us nothing other than ID, MISSCODE, RATER
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
  new_repo[1:nrow(new_repo),3]<- as.character(wrong_val)
  new_repo[1:nrow(new_repo),4]<- which_form
  new_repo[1:nrow(new_repo),5]<- comments
  colnames(new_repo)<-c('id','var_name','wrong_val', 'which_form','comments')
  ifelse(rbind,return(rbind(report,new_repo)),return(new_repo))
}
# make a function to convert date type data correctly (so that 01/01/99 is not converted to 2099-01-01)
mdy<-function(x){
  x<-lubridate::mdy(x)
  index<-which(x>as.Date("2030-01-01"))
  if(length(index)>0){x[index]<-lubridate::ymd(x[index])-lubridate::years(100)}
  index2<-which(x>as.Date(Sys.Date()+lubridate::years(10))|x<as.Date("1930-01-01"))#check if any dates are not reasonable
  if (length(index2)>0){warning(paste0(c("Some dates are not reasonable: ",x[index2]),collapse = " "))}
  return(x)
}
#x=c("01/30/1999","01/30/68","01/30/2010","01/30/11",NA)
#x=c("01/30/2999","01/30/2968","01/30/2010","01/30/2011",NA)

#check double entries (one redcap variabels exist in multiple forms) across froms  
repeatedrcvar<-unique(na.omit(var_map$redcap_var[duplicated(var_map$redcap_var)]))
dbentry<-subset(var_map,redcap_var%in%repeatedrcvar&!is.na(redcap_var)&!access_var%in%c("ID","MISSCODE","CDATE"),select = c("Form_name","redcap_var","access_var","path"))
dbentry #will check double entry after data cleaning  
#########################################

#STEP1: Select a Access form - baseline(bl) first and then other forms- match with redcap variables, splited ordinary variables with checkbox variables, removed calculated variables 
STEP1<-function(){
  #STEP1.1 Select a RC form. 
  formname <- forms[form_i] 
  cat(paste0("STEP1: #", form_i," Cleaning form: ",formname," now...\n"))
  fm_dir<-paste0(formname,".csv")
  vm<-subset(var_map, path==fm_dir) #subset of var mapping for the current form
  if(!(sum(vm$baseline)==0|sum(vm$baseline)==nrow(vm))){stop(message("check the column 'baseline' in the var map"))
  }else{ifbl<-any(vm$baseline)} #ifbl: if baseline 
  
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
    if(length(w_acvar)>0){warning(paste("step1.2","#",form_i,formname,"Warning:",paste(w_acvar,collapse = ","),"cannot be found in the var_map."))} # report ^
    w_rcvar<-setdiff(na.omit(vm$access_var),colnames(RAWDATA))# all access_var in var mapping should be in actual Access forms
    if(length(w_rcvar)>0){warning(paste("step1.2","#:",form_i,formname,"Warning:",paste(w_rcvar,collapse = ", "),"in the var_map does not match any variables in the forms."))} # report^
    RAWDATA<-RAWDATA[,which(colnames(RAWDATA)%in%c(acvar_nonch,acvar_chk))] #remove unncessary var 
    #STEP1.3 no NA in ID or CDATE. create IDDATE. IDDATE must be unique
    RAWDATA[which(RAWDATA=="",arr.ind = T)]<-NA
    if(any(is.na(RAWDATA$ID)|is.na(RAWDATA$CDATE))){stop(message(paste("NA in ID or CDATE of RAWDATA. Form:",formname)))}
    if("CDATE"%in%colnames(RAWDATA)){ #if the dataframe has CDATE
      readline(prompt = paste0("Enter any key to confirm CDATE '",RAWDATA[1,"CDATE"],"' is in format month date year.")) # confirm the format of CDATE
      RAWDATA$CDATE<-mdy(RAWDATA$CDATE)
      RAWDATA$CDATECOPY<-RAWDATA$CDATE # create a col CDATECOPY so that after var mapping the form still has a col called CDATE 
      RAWDATA$IDDATE<-paste0(RAWDATA$ID,RAWDATA$CDATE)
    }else{
      RAWDATA$IDDATE<-RAWDATA$ID
      warning(paste0("#",form_i,formname," step1.3 Warn: ",formname," does not have CDATE."))}
    RAWDATA<-unique(RAWDATA) #remove duplicated rows before checking duplicated IDDATE 
    if(ifbl){ 
      dup_id<-unique(RAWDATA[which(duplicated(RAWDATA$ID)),"ID"])# shoule have no duplicates in ID
      if(length(dup_id)>0){
        warning(paste0("Warn: ",formname," is a baseline form and has duplicated ID. Please refer to formname_dup_id_rows.csv. The rows are removed."))
        reportdup<-RAWDATA[which(RAWDATA$ID%in%dup_id),]
        reportdup<-reportdup[order(reportdup$ID),];reportdup[which(is.na(reportdup),arr.ind = T)]<-""
        write.csv(reportdup,file = paste0(logdir,formname,"_dup_id_rows.csv"))
        RAWDATA<-RAWDATA[-which(RAWDATA$ID%in%dup_id),]}      #remove duplicated rows 
    }else{
      dup_id<-unique(RAWDATA[which(duplicated(RAWDATA$IDDATE)),"IDDATE"])# shoule have no duplicates in IDDATE
      if(length(dup_id)>0){
        warning(paste0("Warn: ",formname," has duplicated IDDATE. Please refer to formname_dup_id_rows.csv. The rows are removed."))
        reportdup<-RAWDATA[which(RAWDATA$IDDATE%in%dup_id),]
        #SPECIAL for SIS: remove blank rows 
        if (grepl("SIS",formname)){
          realvar<-setdiff(colnames(reportdup),c("ID","CDATE","CDATECOPY","IDDATE"))
          allnarow<-which(rowSums(is.na(reportdup[realvar]))==length(realvar)) #to be removed
          reportdup<-reportdup[-allnarow,] # removed all-NA rows 
          duprows<-which(duplicated(reportdup$IDDATE,fromLast = T)|duplicated(reportdup$IDDATE)) #rows where IDDATE have duplicates
          RAWDATA<-rbind(RAWDATA,reportdup[-duprows,])#add back unique rows 
          reportdup<-reportdup[duprows,] #remove unique rows from the reportdup
        }
        reportdup<-reportdup[order(reportdup$IDDATE),];reportdup[which(is.na(reportdup),arr.ind = T)]<-""
        write.csv(reportdup,file = paste0(logdir,formname,"_dup_idcdate_rows.csv"))
        RAWDATA<-RAWDATA[-which(RAWDATA$IDDATE%in%dup_id),]}      #remove duplicated rows
    }
    #SPECIAL for SCID: add back some records with dup id that Morgan manually find. These five forms are the only forms that have duplicaetd IDs. 
    #if (formname%in%c("A_SCIDIV","A_SCIDCHRON","L_CONDIAG","LSU2_PAIN","A_SUPP")){
    #  special<-read.csv(paste0(rootdir,"deleted_duplicated_id/",formname,"_special_dup_id.csv"),stringsAsFactors = F)
    #  special<-subset(special,ifkeep=="TRUE",select = 1:(ncol(special)-1))[-1]
    #  special[which(special=="",arr.ind = T)]<-NA
    #  special$CDATE<-as.Date(special$CDATE,format = "%m/%d/%y");special$CDATECOPY<-as.Date(special$CDATECOPY,format = "%m/%d/%y")
    #  if(!(any(duplicated(special$ID))|any(special$ID%in%RAWDATA$ID))){
    #    RAWDATA<-rbind(RAWDATA,special)
    #    message(paste0("Note: added back observations Morgan identified manually on Dec 12."))
    #  }else{stop(message("Something is wrong"))}
    #}
    #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx vars to df: 'raw_chk'
    if(!is.null(acvar_chk)){
      raw_nonch<-RAWDATA[,-which(colnames(RAWDATA)%in%acvar_chk)] #keep only non-checkbx variables 
      ifelse("CDATE"%in%colnames(RAWDATA), raw_chk<-RAWDATA[,c("ID","CDATE","IDDATE",acvar_chk)],raw_chk<-RAWDATA[,c("ID","IDDATE",acvar_chk)])
    }else{raw_nonch<-RAWDATA}
    #STEP1.5 remove calculated fields 
    cal_var<-subset(vm,fix_what=='calculated_field')$access_var
    if(length(cal_var)>0){raw_nonch<-raw_nonch[,-which(colnames(raw_nonch)%in%cal_var)]}
    #STEP1.6 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames. make sure there's a column cdate 
    VMAP<-unique(subset(vm,select=c(access_var,redcap_var),is.checkbox=='FALSE'&!is.na(redcap_var)))
    if(any(duplicated(na.omit(VMAP$access_var)))){warning(paste("setp1.6","#",form_i,formname,"Variable mapping... \nWarning: some non-checkbox access variable matches multiple redcap variabels in form",formname))} #check if one ac var matches multiple rc var 
    colnames(raw_nonch)<-plyr::mapvalues(colnames(raw_nonch),from = VMAP$access_var, to = VMAP$redcap_var,warn_missing = F)
    if("CDATE"%in%colnames(RAWDATA)){colnames(raw_nonch)[grep("^CDATE",colnames(raw_nonch))]<-"CDATE"} # rename cdatecopy 
    if(!all(colnames(raw_nonch)%in%c(VMAP$redcap_var,"CDATE","IDDATE","MISSCODE"))){stop(message(paste0(formname," has an error after var mapping when checking: new colnames should contain only CDATE, IDDATE, and redcap variables")))} # check: new colnames should contain only CDATE, IDDATE, and redcap variables 
    if("CDATE"%in%colnames(RAWDATA)){raw_nonch<-cbind(raw_nonch[,-which(colnames(raw_nonch)=="CDATE")],CDATE=raw_nonch$CDATE)} # keep only one col of CDATE
    if(any(duplicated(colnames(raw_nonch)))){stop(message(paste0("Stop: ",formname,": Duplicated colnames.")))}
    #STEP1.7 copy the column CDATE and rename as cdate_formname
    if("CDATE"%in%colnames(RAWDATA)){raw_nonch<-cbind(raw_nonch,newcol=raw_nonch$CDATE)
    colnames(raw_nonch)<-gsub("newcol",tolower(paste0("cdate_",formname)),colnames(raw_nonch))}
    #STEP1.8 SPECIAL for some forms that have "condition" issue, merge the checkbox df with certain non-chk access var. 
    if ("condition" %in% vm$fix_what){
      raw_chk<-cbind(raw_chk,RAWDATA[,unique(subset(vm,fix_what=="condition",select = value1)[[1]])])
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
  }else{cat(paste0(formname," is not a baseline form. Skiped it.\n"))
    ifbl<<-ifbl}
  
}# the function is writen and editted in another script. Above is a copy of the script
##STEP2 fix data with systematic issues (eg: shifted range) identified in 'var_map'
STEP2<-function(){
  fresh_nonch<-raw_nonch
  cat(paste("#",form_i,formname,"- performning STEP2 now...\n"))
  #STEP2.01 range_fix: range in access is not the same as range in redcap
  fixmap<-unique(subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)))
  if(nrow(fixmap)>0) {for (step4_i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
    valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T) # don't use str split because we want NA rather than "NA"
    if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
      message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
    }else{
      fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = F)
    }}}
  #STEP2.02 range_allowed: The range in Redcap allows more values than we accept from what should have been the range in redcap. Specifies the new range
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
  #STEP2.03 date: must be converted to date (YYYY-MM-DD)
  fixmap<-unique(subset(vm,fix_what=='date',select = c(redcap_var,instructions)))
  if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
    if(all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){next()}
    eg<-as.character(na.omit(fresh_nonch[[fixmap$redcap_var[step4_i]]]))[1]
    fresh_nonch[fixmap$redcap_var[step4_i]]<-as.Date(mdy(as.character(fresh_nonch[[fixmap$redcap_var[step4_i]]])))
    if(all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){stop((message(paste0("After transforming date, all data in",fixmap$redcap_var[step4_i]," are removed. somthing must be wrong."))))}
    if(!all(na.omit(as.integer((fresh_nonch[[fixmap$redcap_var[step4_i]]]-Sys.Date())/365))%in%seq(-100,20))){stop(message("Something went wrong when changing the format of date"))} # check that the new date is in reasonable range: +-100years of today
  }}
  #STEP2.04 value_set: import this value for EVERYONE who we import this form for
  fixmap<-unique(subset(vm,fix_what=='value_set',select = c(redcap_var,instructions)))
  if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
    fresh_nonch<-cbind(fresh_nonch,newcolumn=fixmap$instructions[step4_i])
    colnames(fresh_nonch)<-gsub("^newcolumn",fixmap$redcap_var[step4_i],colnames(fresh_nonch))
  }}
  if(nrow(fixmap)>0) { # if there's 'value_set' problem
    fresh_nonch$"ipde_excludeitem"<-replicate(nrow(fresh_nonch),1)
  }
  #STEP2.05 value_set2: if value 1 is not null, value 2, otherwise, value 3
  fixmap<-unique(subset(vm,fix_what=='value_set2',select = c(redcap_var,instructions,value1,value2,value3)))
  if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
    fresh_nonch<-cbind(fresh_nonch,newcolumn=as.integer(fixmap[step4_i,c("value2","value3")[as.integer(is.na(fresh_nonch[[fixmap$value1[step4_i]]]))+1]]))
    colnames(fresh_nonch)<-gsub("newcolumn",fixmap$redcap_var[step4_i],colnames(fresh_nonch))
  }}
  
  fresh_nonch<<-fresh_nonch
  log_out_of_range<<-log_out_of_range
  log_replace<<-log_replace
  cat(paste0(formname,": STEP2 done.\n"))
}
##STEP3 Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
STEP3<-function(){
  cat(paste("#",form_i,formname,"- performning STEP3 now...\n"))
  deleted_rows<-fresh_nonch[1,];deleted_rows<-deleted_rows[-1,];deletedrownum<-c()
  if(formname=="A_SCIDIV"){fresh_nonch$rownum<-1:nrow(fresh_nonch)}
  if(sum(apply(fresh_nonch,2,function(x){as.character(x)==""}),na.rm = T)>0){stop(message("Stop: The df contains \"\". Check your codes!"))} # all "" should be replaced with NA
  #which(apply(fresh_nonch,2,function(x){as.character(x)==""}),arr.ind = T) #find out "" 
  for (j in 1:length(colnames(fresh_nonch))) { # get the range by col (variable) and then get the rows of out-of-range values
    if(!colnames(fresh_nonch)[j]%in%vm$redcap_var){next()} #skip access var in the current form 
    if(colnames(fresh_nonch)[j]%in%c("scid_s211","scid_xii_c168")){next()} #SPECIAL skip "scid_s211","scid_xii_c168"
    if (grepl("___",colnames(fresh_nonch)[j])){next()} # skip checkbox var brougt in during fixing "value_set2"
    rg<-bsrc.getchoicemapping(variablenames = colnames(fresh_nonch)[j],metadata = pt$metadata)[[1]] # get the range 
    if(is.null(rg)){log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname, id='OKAY-NO_RANGE',which_var = colnames(fresh_nonch)[j],comments = 'This variable has no range');next()} # variable should have a range 
    if(("na"%in%rg&sum(is.na(as.numeric(rg)))==1)|!any(is.na(as.numeric(rg)))){rg<-as.numeric(rg)} # turn range as numeric if rg contains only "na" and numbers  
    #get the rows of out-of-range values; replace and report out-of-range values
    i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]]))) # report values that is not in the range. NA is acceptable 
    if(formname=="A_SCIDIV"){# if the the form is "A_SCIDIV", remove the whole row where the value is 0
      i0<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]])) & fresh_nonch[[j]]==0) # if the wrong value is 0, remove the whole row; in the next step, remove these rows from checkbox df too
      #cat(j);print(i0);next()
      if(length(i0>0)){log_replace<-report_wrong(id=fresh_nonch[i0,"IDDATE"],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i0,j], which_form = formname,comments = 'STEP3: Out of range values. The rows are removed.',report = log_replace)
      message(paste("col#",j,":",length(i0),"rows are removed from",formname,"because many of them have 0 as an out-of-range value. Row#",fresh_nonch$rownum[i0],"\n"))
      deleted_rows<-rbind(deleted_rows,fresh_nonch[i0,])
      deletedrownum<-append(deletedrownum,fresh_nonch$rownum[i0])
      fresh_nonch<-fresh_nonch[-i0,]
      i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]])))}
    }
    #i9<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]])) & fresh_nonch[[j]]%in%c(9,2,3,50)) # if the wrong value is 9, replace them with NA;
    #if(length(i9)>0){
    #  log_replace<-report_wrong(id=fresh_nonch[i9,"IDDATE"],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i9,j], which_form = formname,comments = 'STEP3: Out of range values. Replaced with NA',report = log_replace)
    #  fresh_nonch[i9,j]<-NA
    #  i<-setdiff(i,i9)
    #}
    if (length(i)==0){
      cat(paste('GOOD. All values of', formname, colnames(fresh_nonch)[j],'are within the range.\n'))
      log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname,id='GOOD',which_var = colnames(fresh_nonch)[j],comments = 'GOOD. All values are within the range.')
    }else{
      log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[i,1],which_form = formname, which_var = colnames(fresh_nonch)[j],wrong_val = fresh_nonch[i,j],
                                     comments = paste0('Correct range: ', do.call('paste',as.list(rg)),'. Replaced with NA.'))
      log_replace<-report_wrong(id=fresh_nonch[i,"IDDATE"],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i,j], which_form = formname,comments = 'STEP3: Out of range values. Replaced with NA',report = log_replace)
      message(paste0('Warn: Some values from ',formname," ", colnames(fresh_nonch)[j], ' are out of range. Refer to log_out_of_range for more details.'))
    }
    # check 295.3 in scid s211 and 29..0 in scid_xii_c168
    # check 
    
    fresh_nonch[i,j]<-NA
    #cat(paste(j,"done."))
  }
  fresh_nonch<<-fresh_nonch
  log_out_of_range<<-log_out_of_range
  log_replace<<-log_replace
  deleted_rows<<-deleted_rows
  deletedrownum<<-unique(deletedrownum)
  cat(paste0(formname,": STEP3 done.\n"))
}
##STEP4 for checkbox (fix_what1)
STEP4<-function(){
  cat(paste("#",form_i,formname,"- performning STEP4 now...\n"))
  fresh_chk<-raw_chk
  vm<-subset(vm,is.checkbox=="TRUE") #subset of var_map where is.checkbox = T
  
  #STEP4.1
  #####need to check the values of ac var first!????
  #STEP4.2 redcap checkbox
  vm_rcchk<-subset(vm,fix_what=="redcap_check") # subset of vm of redcap_check var
  if (nrow(vm_rcchk)>0){
    #STEP4.2.1 cbind the original df with an empty dataframe containing rc col
    newcolname<-append(colnames(fresh_chk),unique(vm_rcchk$redcap_var))#get the colname for the new df
    fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_rcchk$redcap_var))))) 
    colnames(fresh_chk)<-newcolname
    #STEP4.2.2 fill in redcap cols
    #for each row of fresh_chk, if values of acvar == x1 then values of rcvar == y1
    for (df_i in 1:nrow(fresh_chk)) { # for every observation, [swtich values from access forms to coresponding values in redcap]  
      for (vm_i in 1:nrow(vm_rcchk)){ #for every row in var_map (i.e. for every pair of [accessvalue,redcapvalue]), replace access value with redcap value
        acvar<-vm_rcchk$access_var[vm_i]
        rcvar<-vm_rcchk$redcap_var[vm_i]
        if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
          iftrue<-as.numeric(fresh_chk[df_i,acvar])==vm_rcchk$value1[vm_i]
          fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_rcchk$value2[vm_i],NA)
        }}}
    cat("redcap_check done.")
  }
  #STEP4.3 access checkbox
  vm_achk<-subset(vm,fix_what=="access_check") # subset of vm of redcap_check var
  if (nrow(vm_achk)>0){
    #STEP4.3.1 cbind the original df with an empty dataframe containing rc col
    newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
    fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
    colnames(fresh_chk)<-newcolname
    #STEP4.3.2 fill in redcap cols
    #for each row of fresh_chk, if values of acvar contains x1 then values of rcvar == x2, otherwise rcvar == x3
    for (df_i in 1:nrow(fresh_chk)){
      for (vm_i in 1:nrow(vm_achk)){
        acvar<-vm_achk$access_var[vm_i]
        rcvar<-vm_achk$redcap_var[vm_i]
        if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
          iftrue<-grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
          fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],vm_achk$value3[vm_i])
        }}}
    cat("access_check done.")
  }
  #STEP4.4 both_check1 both access and redcap are checkboxes, not case sensitive, if not in value1, then value 2 
  vm_achk<-subset(vm,fix_what=="both_check1") # subset of vm of redcap_check var
  if (nrow(vm_achk)>0){
    #STEP4.4.1 cbind the original df with an empty dataframe containing rc col
    newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
    fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
    colnames(fresh_chk)<-newcolname
    #STEP4.4.2 fill in redcap cols
    #for each row of fresh_chk, if not contain value1, then value 2 
    for (df_i in 1:nrow(fresh_chk)){
      for (vm_i in 1:nrow(vm_achk)){
        acvar<-vm_achk$access_var[vm_i]
        rcvar<-vm_achk$redcap_var[vm_i]
        if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
          iftrue<-!grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
          fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],NA)
        }}}
    cat("both_check done.")
  }
  #STEP4.5 both_check2 both access and redcap are checkboxes, not case sensitive, if in value1, then value 2 
  vm_achk<-subset(vm,fix_what=="both_check2") # subset of vm of redcap_check var
  if (nrow(vm_achk)>0){
    #STEP4.5.1 cbind the original df with an empty dataframe containing rc col
    newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
    fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
    colnames(fresh_chk)<-newcolname
    #STEP4.5.2 fill in redcap cols
    #for each row of fresh_chk, if contain value1, then value 2 
    for (df_i in 1:nrow(fresh_chk)){
      for (vm_i in 1:nrow(vm_achk)){
        acvar<-vm_achk$access_var[vm_i]
        rcvar<-vm_achk$redcap_var[vm_i]
        if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
          iftrue<-grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
          fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],NA)
        }}}
    cat("both_check done.")
  }
  #STEP4.6 condition if value1=value2, match here, otherwise assign value 3
  vm_achk<-subset(vm,fix_what=="condition") # subset of vm of redcap_check var
  if (nrow(vm_achk)>0){
    #STEP4.6.1 cbind the original df with an empty dataframe containing rc col
    newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
    fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
    colnames(fresh_chk)<-newcolname
    #STEP4.6.2 fill in redcap cols
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
    cat("condition done.")
  }
  #STEP4.7 SPECIAL special_6 range fix and make copies of this variable
  vm_achk<-subset(vm,fix_what=="special_6") # subset of vm of redcap_check var
  if (nrow(vm_achk)>0){
    #STEP4.7.1 cbind the original df with an empty dataframe containing rc col
    newcolname<-append(colnames(fresh_chk),vm_achk$redcap_var)#get the colname for the new df
    fresh_chk<-cbind(fresh_chk,fresh_chk$EATYP,fresh_chk$EATYP,fresh_chk$EATYP) 
    colnames(fresh_chk)<-newcolname
    #STEP4.7.1 range fix 
    for (step4_i in 1:nrow(vm_achk)){
      valuemap<-matrix(eval(parse(text = paste0("c(",vm_achk$instructions[step4_i],")"))),ncol = 2,byrow = T)
      fresh_chk[vm_achk$redcap_var[step4_i]]<-plyr::mapvalues(fresh_chk[[vm_achk$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = F)
    }
  }
  #STEP4.8 SPECIAL special_7 Q3,Q3a; Q3NEW,Q3aNEW Two access variables go to one redcap, only one should have value
  fixmap<-subset(vm,fix_what=="special_7") # subset of vm of redcap_check var
  if (nrow(fixmap)>0){
    if(any(!is.na(fresh_chk$Q3)&!is.na(fresh_chk$Q3NEW))){stop(message("Stop: Special_7 - both Q3 and Q3NEW have value"))}
    if(any(!is.na(fresh_chk$Q3a)&!is.na(fresh_chk$Q3aNEW))){stop(message("Stop: Special_7 - both Q3a and Q3aNEW have value"))}
    fresh_chk$macarthur_3<-gsub("NA","",paste0(fresh_chk$Q3,fresh_chk$Q3NEW))
    fresh_chk$macarthur_3_n<-gsub("NA","",paste0(fresh_chk$Q3a,fresh_chk$Q3aNEW))
    fresh_chk$macarthur_3[which(fresh_chk$macarthur_3=="")]<-NA
    fresh_chk$macarthur_3_n[which(fresh_chk$macarthur_3_n=="")]<-NA
  }
  #STEP4.9 VERSION fixed manually 
  fresh_chk<<-fresh_chk
  cat(paste("\n#",form_i,formname,"- STEP4 done.\n"))
}
#STEP5 match checkbox variabels with other variabels using matching_id or IDDATE
STEP5<-function(){
  cat(paste("#",form_i,formname,"- performning STEP5 now...\n"))
  if(is.null(acvar_chk)){
    fresh_alldata<-fresh_nonch
    cat("STEP5 done. No checkbox col needs to be merged.\n")
  }else{
    if(length(deletedrownum)>0){ # some rows are deleted in step 5 for A_SCIDIV
      fresh_alldata<-dplyr::full_join(fresh_nonch,fresh_chk[-deletedrownum,],by=c("CDATE","IDDATE")) # if some rows are removed from the nonch_df, remove them from the chk df too
      deleted_rows<-dplyr::full_join(deleted_rows,fresh_chk[deletedrownum,],by=c("CDATE","IDDATE"))
    }else{fresh_alldata<-dplyr::full_join(fresh_nonch,fresh_chk[-1],by = c("CDATE","IDDATE"))}
    if(!(nrow(fresh_chk)-length(deletedrownum))==nrow(fresh_alldata)|!nrow(fresh_nonch)==nrow(fresh_alldata)){stop(message("STEP5: something is wrong."))}
    fresh_alldata<<-fresh_alldata
    cat("STEP5 done. Checkbox col and non-check box cols have been merged.\n")
  }
  fresh_alldata<<-fresh_alldata
  deleted_rows<<-deleted_rows
}
#STEP6 branching (fix_what2)
STEP6<-function(){
  cat(paste("#",form_i,formname,"- performning STEP6 branching now...\n"))
  #branch if value4!=value5, there should be NO data in this variable
  vm_br<-subset(vm,fix_what2=="branch")
  if(is.null(vm_br[["value6"]])){vm_br$value6<-NA}
  if (nrow(vm_br)>0){for (df_i in 1:nrow(fresh_alldata)){
    for (vm_i in 1:nrow(vm_br)){
      brlogic<-sum(fresh_alldata[df_i,vm_br$value4[vm_i]]==vm_br$value5[vm_i],na.rm = T)==1 # branching logic: if T then branch (i.e. have data in the redcap variable); if F or NA then NOT branch.
      if(!brlogic&!is.na(fresh_alldata[df_i,vm_br$redcap_var[vm_i]])){ # report if brlogic ==F but is.na()==F
        if(!is.na(vm_br$value6[vm_i]) & fresh_alldata[df_i,vm_br$redcap_var[vm_i]] %in% eval(parse(text=(vm_br$value6[vm_i])))){ # if the redcap value %in% value 6 then replace it with NA, otherwise just report 
          log_replace<-report_wrong(id = fresh_alldata[df_i,"IDDATE"], which_var = vm_br$redcap_var[vm_i], wrong_val = fresh_alldata[df_i,vm_br$redcap_var[vm_i]], which_form = formname, comments = paste("Branch1",vm_br$value4[vm_i],"=",fresh_alldata[df_i,vm_br$value4[vm_i]],"|in value6 | Replaced with NA"), report = log_replace)
          fresh_alldata[df_i,vm_br$redcap_var[vm_i]]<-NA 
        }else{log_branching<-report_wrong(id = fresh_alldata[df_i,"IDDATE"], which_var = vm_br$redcap_var[vm_i], wrong_val = fresh_alldata[df_i,vm_br$redcap_var[vm_i]], which_form = formname, comments = paste("Branch1- replaced with NA",vm_br$value4[vm_i],"=",fresh_alldata[df_i,vm_br$value4[vm_i]]), report = log_branching)}
        fresh_alldata[df_i,vm_br$redcap_var[vm_i]]<-NA
      }
      #unique(fresh_alldata$staupxtra_sep_which)#-->NA   "1"  "2"  "3"  "NA"
      #unique(fresh_nonch$staupxtra_sep_which)
    }}
  }
  vm_br<-subset(vm,fix_what2=="branch_2")
  if(is.null(vm_br[["value6"]])){vm_br$value6<-NA}
  if (nrow(vm_br)>0){ for (df_i in 1:nrow(fresh_alldata)){
    for (vm_i in 1:nrow(vm_br)){
      brlogic<-sum(with(fresh_alldata[df_i,],eval(parse(text = vm_br$value4[vm_i]))),na.rm = T)==1 # branching logic: if T then branch (i.e. have data in the redcap variable); if F or NA then NOT branch.
      if(!brlogic&!is.na(fresh_alldata[df_i,vm_br$redcap_var[vm_i]])){ # report if brlogic ==F but is.na()==F
        if(!is.na(vm_br$value6[vm_i])&fresh_alldata[df_i,vm_br$redcap_var[vm_i]]%in%eval(parse(text=(vm_br$value6[vm_i])))){ # if the redcap value %in% value 6 then replace it with NA, otherwise just report 
          log_replace<-report_wrong(id = fresh_alldata[df_i,"IDDATE"], which_var = vm_br$redcap_var[vm_i], wrong_val = fresh_alldata[df_i,vm_br$redcap_var[vm_i]], which_form = formname, comments = paste("Branch2",vm_br$value4[vm_i],"| in value6 | Replaced with NA"), report = log_replace)
          fresh_alldata[df_i,vm_br$redcap_var[vm_i]]<-NA 
        }else{log_branching<-report_wrong(id = fresh_alldata[df_i,"IDDATE"], which_var = vm_br$redcap_var[vm_i], wrong_val = fresh_alldata[df_i,vm_br$redcap_var[vm_i]], which_form = formname, comments = paste("Branch2- replaced with NA",vm_br$value4[vm_i]), report = log_branching)}
        fresh_alldata[df_i,vm_br$redcap_var[vm_i]]<-NA
      }
      #unique(fresh_alldata$staupxtra_sep_which)#-->NA   "1"  "2"  "3"  "NA"
      #unique(fresh_nonch$staupxtra_sep_which)
    }}}
  log_branching<<-log_branching
  log_replace<<-log_replace
  cat(paste("#",form_i,formname,"- STEP6 done.\n"))
}

for (form_i in 1) {STEP1();STEP2();STEP3();if(!is.null(acvar_chk)){STEP4()};STEP5()} # temperary test step1 and 4

for (form_i in 1:length(forms)) {
  #for (form_i in 9){ 
  #for (form_i in 6:length(forms)) { 
  STEP1() # get 'raw_nonch': redcap variables, 
  if(!skipotherforms|ifbl){ # skip the form if skipotherforms is T AND ifbl is F
    STEP2();STEP3()
    if(!is.null(acvar_chk)){STEP4()}
    STEP5();STEP6()
    assign(paste0("form_",formname),unique(fresh_alldata))
    write.csv(unique(fresh_alldata),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/form_",formname,"_",Sys.Date(),".csv"))
    if(formname=="A_SCIDIV"){write.csv(unique(deleted_rows),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/deleted_rows_scidiv_",Sys.Date(),".csv"))}
    #  write.csv(unique(log_comb_fm),file = paste0("~/Documents/github/UPMC/TRANSFER/log_comb_fm_",form_i,".csv"))
    #  write.csv(unique(log_out_of_range),file = paste0("~/Documents/github/UPMC/TRANSFER/log_out_of_range_",form_i,".csv"))
    #  write.csv(unique(log_replace),file = paste0("~/Documents/github/UPMC/TRANSFER/log_replace_",form_i,".csv"))
  }} 

# check double entries across forms (dbentry)
#full_join the columns of two forms by IDDATE or ID, get the rows where both cols have values
dbentry<-subset(dbentry,access_var%in%c("RATER","Q3","Q3a","Q3NEW","Q3aNEW",))

#write.csv(unique(log_branching),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/log_branching_scidiv_",Sys.Date(),".csv")) #temperary
#write.csv(unique(log_comb_fm),file = paste0("~/Documents/github/UPMC/TRANSFER/log_comb_fm.csv"))
write.csv(unique(log_out_of_range),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/log_out_of_range_",Sys.Date(),".csv"))
write.csv(unique(log_branching),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/log_branching_",Sys.Date(),".csv"))
write.csv(unique(log_replace),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/log_replace",Sys.Date(),".csv"))
#write.csv(unique(log_replace),file = paste0("~/Documents/github/UPMC/TRANSFER/log_replace_.csv"))

newdeleted<-do.call("rbind",lapply(deleted_rows[4:13],function(x){x[1:2]}))
write.csv(newdeleted,file = paste0("~/Documents/github/UPMC/TRANSFER/DeletedRows_IPDE.csv"))
#}




#####################################end of the function#########################################



