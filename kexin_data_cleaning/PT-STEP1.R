#################################### SAME #################################### 
## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
source('~/Documents/github/UPMC/startup.R')
rootdir="~/Box/skinner/data/Redcap Transfer/All protect data/"
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
forms<-with(var_map,unique(Form_name[!is.na(Form_name)])) #TEMPERARY

combine<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/combing forms.csv',stringsAsFactors = FALSE)
combine[which(combine=="",arr.ind = T)]<-NA
## verify Morgan's var_map. 
####for the col is.box. NA should mean represent unecessary variables. i.e. 
# if redcap_var and access_var both exist, is.checkbox cannot be NA
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
#var_map[[8]]<-sapply(var_map[[8]], function(x) gsub("\"", "", x))###TEMP
## TEMP so that NA in 'is.checkbox' means that 

remove_dupid = FALSE # if T, only keep duplicated id with the earliest date 
#Initialize reports 
log_out_of_range <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                               which_form=as.character(),comments=as.character(),stringsAsFactors = F) #Report out-of-range values 
log_replace <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report wrong values/datatypes, correct and report 
log_comb_fm <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 
log_comb_fm2 <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                           which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 
deleted_rows<-list()
comb_rows<-list # IDDATE absent in at least one form when combining 
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
# PREPARE variable: forms
all_formnm<-with(var_map,unique(Form_name[!is.na(Form_name)])) #get all redcap formnames  
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
#################################### SAME #################################### 

#STEP1: Select a RC form, get an integrated RC form with complete variables, right variable names, splited ordinary variables with checkbox variables. 
for (form_i in 1:length(forms)) {
  #TEMPfor (form_i in 8) {
  STEP1<-function(){
    #STEP1.1 Select a RC form. Check if multiple origianl forms need to be combined into one form 
    formname <- forms[form_i] #formname(a character)
    message(paste0("Cleaning form: ",formname," now..."))
    vm<-subset(var_map, Form_name==formname) #subset of var mapping for the current form
    acvar_nonch<-with(vm,split(access_var,is.checkbox))$'FALSE' #non-checkbox var
    acvar_chk<-with(vm,split(access_var,is.checkbox))$'TRUE' #checkbox var
    fm_dir<-unique(vm$path) #path of forms
    if (any(is.na(vm$path))){
      stop(message('At least one row in var mapping does not give the path of directory for the original forms')) # path cannot be NA
    }else{if(any(!file.exists(paste0(rootdir,fm_dir)))){stop(message('At least one row of path in var mapping does not exist.'))}}#path must be valid
    #STEP1.2 Get raw. Grab forms, remove unecessary variables, combine forms by common cols and remove some rows (eg: with different ID+date values, following combing forms.csv). If not need to combine multiple forms, jump to STEP1.3. 
    if (length(fm_dir)>1){ 
      cb<-subset(combine,Form_name==formname)
      if(!identical(cb$path,unique(vm$path))){stop(message("combining form.csv has some problems."))}
      comb_fm_list<-lapply(fm_dir, function(fm_dir){read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)});names(comb_fm_list)<-cb$path # grab forms 
      #comb_fm_list<-lapply(comb_fm_list, function(x){x[,-which(colnames(x)=='X')]}) # remove col 'X'
      # check var_mapping: access variables in Access forms and var map shoule be the same  
      w_acvar<-paste(sapply(comb_fm_list,function(x){paste(setdiff(colnames(x),vm$access_var),collapse = ", ")}),collapse = " ") #all access variesbles should be in var map
      w_rcvar<-na.omit(setdiff(vm$access_var,unlist(sapply(comb_fm_list,function(x){colnames(x)})))) # all access_var in var mapping should be in actual Access forms
      if(any(grepl("[a-zA-Z]",w_acvar))){message(paste("Warning:",w_acvar,"cannot be found in the var_map."))} # report ^
      if(any(grepl("[a-zA-Z]",w_rcvar))){stop(message(paste("Stop:",paste(w_rcvar,collapse = ", "),"in the var_map does not match any variables in the forms.")))} # report^
      comb_fm_list<-lapply(comb_fm_list, function(x){
        x<-x[,which(colnames(x)%in%c(acvar_nonch,acvar_chk))]
        x[which(x=="",arr.ind = T)]<-NA
        x["CDATE"]<-as.Date(x[["CDATE"]],format = "%m/%d/%y");x}) #remove unnecessary variables, replace "" with NA, change dtype of CDATE
      #STEP1.2.1 No NAs in ID or CDATE. Add IDCDTE col. Report or remove duplicated ID+CDATE.
      if(any(is.na(unlist(sapply(comb_fm_list,function(x){c(x[["ID"]],x[["CDATE"]])}))))){stop(message(paste0("Stop: NA exists in ID or CDATE in the form ",formname)))}
      comb_fm_list<-lapply(comb_fm_list,function(x){x<-data.frame(x,IDDATE=paste0(x[["ID"]],x[["CDATE"]]),stringsAsFactors = F)}) # add col of "IDDATE"
      temp_dup_id<-as.vector(unlist(sapply(comb_fm_list, function(x){x[which(duplicated(x[["IDDATE"]])),"IDDATE"]}))) # get duplicated IDDATE 
      if (length(temp_dup_id)>0){
        if (!as.logical(remove_dupid)){ # report duplicated ID
          log_comb_fm<-report_wrong(id=temp_dup_id,which_var = 'IDDATE',report = log_comb_fm,which_form = formname,comments = 'Duplicated IDDATE. Note: it\'s possible that they are duplicated in each form.')
          log_comb_fm<-unique(log_comb_fm)
          message('Duplicated IDs exist. Refer to log_comb_fm for more info. Forms are stored as comb_fm_list.
      Viewing details of duplicated ID...')}
        temp_chck_dupid<-lapply(comb_fm_list,function(x){x[which(x[["IDDATE"]]%in%temp_dup_id),]}); # Viewing details of duplicated IDDATE
        for (chk_i in 1:length(temp_chck_dupid)) {View(temp_chck_dupid[[chk_i]])} #Viewing details of duplicated IDDATE
        remove_dupid<-readline(prompt = 'Enter T to remove duplciated ID; F to just report: ') # to remove duplicated ID based on date 
        if(as.logical(remove_dupid)){
          message("Duplicated IDDATE Not removed.")
          remove_dupid<-F # foreced to report dup ids for the next form 
        }}
      #STEP1.2.2 My codes assume the common columns are ID, CDATE, IDDATE, MISSCODE. then check IDDATE following Morgan's .csv  
      comm_var<-Reduce(intersect,lapply(comb_fm_list,names)) # get a vector of the names of common cols.
      w_comm_var<-setdiff(comm_var,c("ID","CDATE","IDDATE","MISSCODE")) # identify common var other than ID, DATE, MISSCODE
      if (length(w_comm_var)>0){stop(message(paste0("Common variables other than ID, DATE, MISSCODE: ",paste(w_comm_var,collapse = ", "))))} # my code assumes the common cols are only ID, CDATE, MISSCODE
      for (comb_i in 1:length(cb$path)){comb_fm_list[[comb_i]]<-data.frame(comb_fm_list[[comb_i]],formpath=cb$path[comb_i])}
      # Check IDDATE following Morgan's instructions 
      if(all(grepl("=",cb$instructions))){
        commonid<-Reduce(intersect,sapply(comb_fm_list,function(x){x[["IDDATE"]]})) # get IDDATE that exist in all forms 
        if(length(commonid)==0){stop(message("Something is wrong"))
        }else{new_combrows<-lapply(comb_fm_list,function(x){subset(x,!IDDATE%in%commonid)});names(new_combrows)<-cb$path}
        newlog<-do.call("rbind",lapply(new_combrows,function(x){
          report_wrong(id=x[["IDDATE"]],which_var = x[["formpath"]],wrong_val = length(cb$path),which_form = formname,comments = "observation absent from at least one from",report = log_comb_fm,rbind = F)}))
        log_comb_fm2<-rbind(log_comb_fm2,unique(newlog))
      }else{stop(message("write codes to check IDDATE when combining forms!"))}
      comb_fm_list<-lapply(comb_fm_list,function(x){subset(x,IDDATE%in%commonid)}) #TEMPERARLY remove the rows 
      rawdata<-Reduce(f=function(x,y){dplyr::full_join(x,y,by=c("ID","CDATE","IDDATE"))},comb_fm_list)
      if("MISSCODE" %in% comm_var){colnames(rawdata)[grep("MISSCODE",colnames(rawdata))]<-paste0("MISSCODE",cb$path)}
      rawdata<-rawdata[,-grep("formpath",colnames(rawdata))] #remove formpath
      #STEP1.2.4 Remove some rows (eg:that have different name+cdate values). 
      new_comm_col<-Reduce(dplyr::inner_join,temp_comm_col_list) # innerjoin common cols
      removed_rows<-nrow(temp_comm_col_list[[1]])-nrow(new_comm_col)
    }else{#STEP1.3 get 'rawdata'-- necessary vars. ID+CDATE must be unique  
      rawdata <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F) #grab form 
      rawdata<-rawdata[,which(colnames(rawdata)%in%c(acvar_nonch,acvar_chk))] #remove unncessary var 
    }
    #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx vars to df: 'raw_chk'
    if(!is.null(acvar_chk)){
      raw_nonch<-rawdata[,-which(colnames(rawdata)%in%acvar_chk)] #keep only non-checkbx variables 
      raw_chk<-rawdata[c("ID","CDATE","IDDATE",acvar_chk)]
    }else{raw_nonch<-rawdata}
    #STEP1.5 remove calculated fields 
    cal_var<-subset(vm,fix_what=='calculated_field')$access_var
    if(length(cal_var)>0){raw_nonch<-raw_nonch[,-which(colnames(raw_nonch)%in%cal_var)]}
    #STEP1.6 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames NOTE: ONE ACVAR CAN MATCH MULTIPLE RCVAR
    VMAP<-unique(subset(vm,select=c(access_var,redcap_var),is.checkbox=='FALSE'))
    colnames(raw_nonch)<-plyr::mapvalues(colnames(raw_nonch),from = VMAP$access_var, to = VMAP$redcap_var)
    if(any(duplicated(colnames(raw_nonch)))){stop(message(paste0("Stop: ",formname,": Duplicated colnames.")))}
    if(!is.null(acvar_chk)){raw_nonch$matching_id<-1:nrow(raw)} #get non-check df a matching id if needed
    #STEP rename colname CDATE. Save NAME+CDATE to the outer environment 
    
    
    vm<<-vm
    formname<<-formname
    acvar_chk<<-acvar_chk
    rawdata<<-raw
    deleted_rows<<-deleted_rows
    if(!is.null(acvar_chk)){raw_chk<<-raw_chk}
    raw_nonch<<-raw_nonch
    log_replace<<-log_replace
    colnames(log_comb_fm2)<-c("IDDATE","path","num_forms","formname","comments")
    log_comb_fm<<-log_comb_fm
    log_comb_fm2<<-log_comb_fm2
    message(paste0(formname,": STEP1 done."))
  }
  
} # remove this 


