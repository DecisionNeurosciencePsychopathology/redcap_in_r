## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
source('~/Documents/github/UPMC/startup.R')
rootdir="~/Box/skinner/data/Redcap Transfer/All protect data/"
allsub<-read.csv(paste0(rootdir,"ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
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
comb_rows<-list() # IDDATE absent in at least one form when combining 
#####################################start of the function#########################################
# rctransfer.dataclean <- function(
# [VARIABLES]
#curdb = bsoc
protocol.cur <- ptcs$bsocial
#db = 
#bsoc<- bsrc.checkdatabase2()

forms = NULL # A vector. must be exactly the same as the a subset of the form names in the variable mapping. Case sensitive. Space sensitive. 
#range
replace_999 = TRUE # by defult, replace all 999 with NA 
remove_dupid = FALSE # if T, only keep duplicated id with the earliest date 
replace_w_na = TRUE
#) {

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

for (form_i in 1:length(forms)) {
  #for (form_i in 8:length(forms)) {
  #STEP1: Select a RC form, get an integrated RC form with complete variables, right variable names, splited ordinary variables with checkbox variables, removed calculated variables 
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
      comb_fm_list<-lapply(comb_fm_list,function(x){x<-with(x,x[which(ID%in%allsub$ID),])}) #remove people not in our study
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
      #STEP1.2.2check IDDATE following Morgan's .csv  
      comm_var<-Reduce(intersect,lapply(comb_fm_list,names)) # get a vector of the names of common cols.
      w_comm_var<-setdiff(comm_var,c("ID","CDATE","IDDATE","MISSCODE")) # identify common var other than ID, DATE, MISSCODE
      if (length(w_comm_var)>0){stop(message(paste0("Common variables other than ID, DATE, MISSCODE: ",paste(w_comm_var,collapse = ", "))))} # my code assumes the common cols are only ID, CDATE, MISSCODE
      for (comb_i in 1:length(cb$path)){comb_fm_list[[comb_i]]<-data.frame(comb_fm_list[[comb_i]],formpath=cb$path[comb_i])} #add col "formpath" to each ac form
      # Check IDDATE following Morgan's instructions 
      if(all(grepl("=",cb$instructions))){
        commonid<-Reduce(intersect,sapply(comb_fm_list,function(x){x[["IDDATE"]]})) # get IDDATE that exist in all forms 
        if(length(commonid)==0){stop(message("Something is wrong. commonid should contain at least something."))
        }else{
          new_combrows<-lapply(comb_fm_list,function(x){subset(x,!IDDATE%in%commonid)});names(new_combrows)<-cb$path # get rows with IDDATE that does not exist in every form
          comb_rows<-append(comb_rows,new_combrows);rm(new_combrows)
        }
        newlog<-do.call("rbind",lapply(new_combrows,function(x){
          report_wrong(id=x[["IDDATE"]],which_var = x[["formpath"]],wrong_val = length(cb$path),which_form = formname,comments = "observation absent from at least one from",report = log_comb_fm,rbind = F)}))
        log_comb_fm2<-rbind(log_comb_fm2,unique(newlog))
        
      }else{stop(message("write codes to check IDDATE when combining forms!"))}
      comb_fm_list<-lapply(comb_fm_list,function(x){subset(x,IDDATE%in%commonid)}) #TEMPERARLY remove the wired rows 
      rawdata<-Reduce(f=function(x,y){dplyr::full_join(x,y,by=c("ID","CDATE","IDDATE"))},comb_fm_list)
      if("MISSCODE" %in% comm_var){colnames(rawdata)[grep("MISSCODE",colnames(rawdata))]<-paste0("MISSCODE",cb$path)}
      rawdata<-rawdata[,-grep("formpath",colnames(rawdata))] #remove formpath
    }else{#STEP1.3 get 'rawdata'-- necessary vars. 
      rawdata <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F) #grab form 
      rawdata<-rawdata[,which(colnames(rawdata)%in%c(acvar_nonch,acvar_chk))] #remove unncessary var 
      #STEP 1.3.1 no NA in ID or CDATE. create IDDATE. IDDATE must be unique
      rawdata[which(rawdata=="",arr.ind = T)]<-NA
      if(any(is.na(rawdata$ID)|is.na(rawdata$CDATE))){stop(message(paste("NA in ID or CDATE of rawdata. Form:",formname)))}
      rawdata$IDDATE<-paste0(rawdata$ID,rawdata$CDATE)
      if(any(is.na(rawdata$IDDATE))){stop(message(paste("Duplicated IDDATE in",formname)))} # no duplicates in IDDATE
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
    if(any(duplicated(VMAP$access_var))){message(paste("Variable mapping... \nWarning: some access variable matches multiple redcap variabels in form",formname))} #check if one ac var matches multiple rc var 
    colnames(raw_nonch)<-plyr::mapvalues(colnames(raw_nonch),from = VMAP$access_var, to = VMAP$redcap_var)
    if(any(duplicated(colnames(raw_nonch)))){stop(message(paste0("Stop: ",formname,": Duplicated colnames.")))}
    if(!is.null(acvar_chk)){raw_nonch$matching_id<-1:nrow(raw)} #get non-check df a matching id if needed
    #STEP rename colname CDATE. Save NAME+CDATE to the outer environment 
    
    write.csv(unique(log_comb_fm2),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/",formname,"log_wrong_combingforms.csv"))
    for (del_i in 1:length(comb_rows)){
      write.csv(comb_rows[[del_i]],file = paste0("~/Documents/github/UPMC/TRANSFER/PT/",formname,"_Wrong_RowstobeCombined_",names(comb_rows)[del_i]))
      rm(del_i)
    }
  
    vm<<-vm
    formname<<-formname
    acvar_chk<<-acvar_chk
    rawdata<<-rawdata
    if(!is.null(acvar_chk)){raw_chk<<-raw_chk}
    raw_nonch<<-raw_nonch
    #deleted_rows<<-deleted_rows
    comb_rows<<-comb_rows
    #log_replace<<-log_replace
    log_comb_fm<<-log_comb_fm
    colnames(log_comb_fm2)<-c("IDDATE","path","num_forms","formname","comments")
    log_comb_fm2<<-log_comb_fm2
    message(paste0(formname,": STEP1 done."))
  }# the function is writen and editted in another script. Above is a copy of the script
  STEP1() # get 'raw_nonch': redcap variables, 
  
  ##STEP2 change data type 
  # identify all non-integer/numeric col
  #Dates (change date to date (YYYY-MM-DD))
  
  ##STEP3 get 'fresh_nonch'. Report 999 AND if replace_999=T, replace 999's with NA
  STEP3<-function(df=raw_nonch){
    if (length(which(df==999))>0){
      log_replace<-rbind(log_replace,(do.call("rbind",apply(which(df==999,arr.ind = T),1,function(indeX){ # TO BE GENERALIZED
        report_wrong(report = log_replace, id=df[indeX[1],1],which_var = colnames(df)[indeX[2]],
                     wrong_val = 999, which_form = formname, rbind = F,
                     comments = ifelse(replace_999,'Replaced with NA','Not replaced with NA yet'))
      })))) 
      if(replace_999){df[which(df==999,arr.ind = T)]<-NA}
    }else {message(paste('Form',formname,'does not have any value of 999'))}
    log_replace<<-log_replace
    return(df)
  }
  fresh_nonch<-STEP3(); message(paste0(formname,": STEP3 done."))
  ##STEP fix data with systematic issues (eg: shifted range) identified in 'var_map'
  STEP4<-function(){
    message(paste0(formname,": performning STEP4 now..."))
    #STEP4.01 range_fix: range in access is not the same as range in redcap, specifies first access variable, then redcap variable to change to
    fixmap<-unique(subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)))
    if(nrow(fixmap)>0) {for (step4_i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
      valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T)
      if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
        message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
      }else{
        fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = T)
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
          message(paste('Fixing issue "range_allowed" GOOD.:', formname,fixmap$redcap_var[step4_i],'are within the range (NA is allowed).'))
        }else{
          log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[row_i,1],which_form = formname, which_var = fixmap$redcap_var[step4_i],wrong_val = thecol[row_i,1],
                                         comments = 'range_allowed')
          fresh_nonch[row_i,fixmap$redcap_var[step4_i]]<-NA
          log_replace<-report_wrong(id=fresh_nonch[row_i,1],which_var = fixmap$redcap_var[row_i], wrong_val = thecol[row_i,1],which_form = formname,comments = 'Fixing "range_allowed": Out of the range.The value is replaced with NA',report = log_replace)
          message('Fixing issue "range_allowed": Some values are out of range. Refer to log_out_of_range for more details. The out-of-range values are replcaed with NA.')
        }}}}
    #STEP4.03 special_3: range_fix+range_allowed , 1=1, 2=2, 3=3, 4=5 (5 out of range)
    fixmap<-unique(subset(vm,fix_what=='special_3',select = c(redcap_var,instructions)))
    if(nrow(fixmap)>0) { #if there's 'special_3' problem
      #range_allowed
      thecol<-fresh_nonch[fixmap$redcap_var[1]] # the col with the problem 
      if(!is.numeric(thecol[[1]])){ # values in the col should be all numeric (or NA)
        stop(message(paste0('Form "',formname,'" has non-numeric values in column "',fixmap$redcap_var[step4_i],'" so "special_3" cannot be fixed')))
      }else{
        rg<-1:4 #get rg: range specified in var_map
        row_i<-which(!((thecol[[1]] %in% rg) | is.na(thecol[[1]]))) # report values that is not in the range. NA is acceptable 
        if (length(row_i)==0){
          message(paste('Fixing issue "special_3: range_fix+range_allowed." GOOD.:', formname,fixmap$redcap_var[step4_i],'are within the range (NA is allowed).'))
        }else{
          log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[row_i,1],which_form = formname, which_var = fixmap$redcap_var[step4_i],wrong_val = thecol[row_i,1],
                                         comments = 'speical_3')
          fresh_nonch[row_i,fixmap$redcap_var[step4_i]]<-NA
          log_replace<-report_wrong(id=fresh_nonch[row_i,1],which_var = fixmap$redcap_var[row_i], wrong_val = thecol[row_i,1],which_form = formname,comments = 'Fixing "special_3": Out of the range.The value is replaced with NA',report = log_replace)
          message('Fixing issue "special_3". Some values are out of range. Refer to log_out_of_range for more details. The out-of-range values are replcaed with NA.')
        }}
      #range_fix- copied the codes in step 4.1
      valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T)
      if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
        message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
      }else{
        fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = T)
      }}
    #STEP4.04 date: must be converted to date (YYYY-MM-DD)
    fixmap<-unique(subset(vm,fix_what=='date',select = c(redcap_var,instructions)))
    if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
      fresh_nonch[fixmap$redcap_var[step4_i]]<-as.Date(fresh_nonch[fixmap$redcap_var[step4_i]][[1]],format = fixmap$instructions[step4_i])
    }}
    #STEP 4.05 SPECIAL check_equal: These two values in access should be equal before being imported. Throw an error if they are different
    fixmap<-subset(vm,fix_what=='check_equal',select = c(access_var,instructions))
    if (nrow(fixmap)>0){ #if there's 'check_equal' problem, fix the problem one variable by one var
      fixmap$instructions<-gsub("=",",",fixmap$instructions)
      reportrow<-which(!fresh_nonch$SPD5==fresh_nonch$STPD8)
      if(length(reportrow)>0){
        log_replace<-report_wrong(id = fresh_nonch[reportrow,1], which_var = "SPD5", wrong_val = fresh_nonch$SPD5[reportrow], which_form = formname, comments = "check_equal: SPD5<>STPD8", report = log_replace) #report
        log_replace<-report_wrong(id = fresh_nonch[reportrow,1], which_var = "STPD8", wrong_val = fresh_nonch$STPD8[reportrow], which_form = formname, comments = "check_equal: SPD5<>STPD8", report = log_replace) #report
        fresh_nonch$SPD5[reportrow]<-NA #replace with NA
        fresh_nonch$STPD8[reportrow]<-NA #replace with NA
        #for (step4_i in 1:nrow(fixmap)){ # generalized codes but not replace with NA
        #  temp_check<-subset(fresh_nonch,select = eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))))
        #  if(!all(temp_check[[1]]==temp_check[[2]])){stop(message(paste0(formname,"'s ",fixmap$instructions[step4_i]," are not equal.")))}
        #  rm(temp_check)
        #}
      }}
    #STEP4.06 multi_field: One access variable goes into multiple redcap variables
    fixmap<-subset(vm,fix_what=='multi_field',select = c(access_var,instructions))
    if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
      newvar<-gsub(" ","",strsplit(fixmap$instructions[step4_i],",")[[1]]) #new rc var
      newcolnames<-append(colnames(fresh_nonch),newvar) #update the colnames to include the new rc var
      fresh_nonch<-cbind(fresh_nonch,replicate(length(newvar),fresh_nonch[fixmap$access_var[step4_i]])) #duplicate the ac col and then rbind the cols to the original df
      colnames(fresh_nonch)<-newcolnames 
    }}
    #STEP4.07 special_4: This value goes into multiple redcap values, also value needs to be changed
    fixmap<-unique(subset(vm,fix_what=='special_4',select = c(access_var,instructions,value1)))
    if(nrow(fixmap)>0) { # if there's 'special_4' problem
      # replace values (range_fix)
      valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[1],")"))),ncol = 2,byrow = T)
      if (all(is.na(fresh_nonch[[fixmap$access_var[1]]]))){ 
        message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[1],'" so no need to do value replacement for "special_4"'))
      }else{
        temp_dupcol<-plyr::mapvalues(fresh_nonch[[fixmap$access_var[1]]],from = valuemap[,1], to = valuemap[,2],warn_missing = T)
        # multi_field
        newvar<-gsub(" ","",strsplit(fixmap$value1[1],",")[[1]]) #new rc var
        newcolnames<-append(colnames(fresh_nonch),newvar)
        fresh_nonch<-cbind(fresh_nonch,replicate(length(newvar),temp_dupcol)) #duplicate the ac col and then rbind the cols to the original df
        colnames(fresh_nonch)<-newcolnames #update the colnames to include the new rc var
      }}
    #STEP4.08 special_1 needs to be changed to time (HH:SS)
    sp1var<-subset(vm,fix_what=='special_1',select = redcap_var)[[1]]
    if(length(sp1var)>1){fresh_nonch[,sp1var]<-as.data.frame(apply(fresh_nonch[,sp1var],2,function(x){gsub('1899-12-30','',x)}))}
    #STEP4.09 SPECIAL value_set: import this value for EVERYONE who we import this form for
    fixmap<-unique(subset(vm,fix_what=='value_set',select = c(redcap_var,instructions)))
    if(nrow(fixmap)>0) { # if there's 'value_set' problem
      fresh_nonch$"ipde_excludeitem"<-replicate(nrow(fresh_nonch),1)
    }
    #STEP4.10 special_5: if demo_childnum=0, give 0, otherwise give 1
    fixmap<-unique(subset(vm,fix_what=='special_5',select = c(redcap_var,instructions,value1,value2)))
    if(nrow(fixmap)>0) { # if there's 'special_5' problem
      for (df_row in 1:nrow(fresh_nonch)){
        if (is.na(fresh_nonch$demo_childnum[df_row])){fresh_nonch$demo_ynchild[df_row]<-NA
        }else{
          fresh_nonch$demo_ynchild[df_row]<-ifelse(as.numeric(fresh_nonch$demo_childnum[df_row])==0,0,1)
        }}}
    
    fresh_nonch<<-fresh_nonch
    log_out_of_range<<-log_out_of_range
    log_replace<<-log_replace
    message(paste0(formname,": STEP4 done."))
  }
  STEP4()
  
  ##STEP5 
  #Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
  STEP5<-function(){
    message(paste0(formname,": performning STEP5 now..."))
    if(!replace_999){message('Warn: 999 has not been replaced yet.')}
    for (j in 1:length(colnames(fresh_nonch))) { # get the range by col (variable) and then get the rows of out-of-range values
      if (colnames(fresh_nonch)[j]=="matching_id"){break()}
      if(colnames(fresh_nonch)[j]%in%vm$redcap_var){ #skip access var in the current form 
        rg<-bsrc.getchoicemapping(variablenames = colnames(fresh_nonch)[j],metadata = bsoc$metadata)[[1]] # get the range 
        if(is.null(rg)){log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname, id='OKAY-NO_RANGE',which_var = colnames(fresh_nonch)[j],comments = 'This variable has no range') # variable should have a range 
        } else { # check range for those variables that have a range 
          if(formname=="IPDE"){ # for "IPDE", the range is often "2"   "1"   "0"   "NaN", which is okay
            ifelse(j==107,rg<-c(1,0),rg<-c(2,1,0))
          }else{
            if(any(is.na(as.integer(rg)))){stop(message(paste('The range of variable',colnames(fresh_nonch)[j],'is not integer or contain NA. Stop the function.')))}# the range should be integer 
            rg<-as.integer(rg)}
          #get the rows of out-of-range values; replace and report out-of-range values
          i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]]))) # report values that is not in the range. NA is acceptable 
          if (length(i)==0){
            message(paste('GOOD. All values of', formname, colnames(fresh_nonch)[j],'are within the range.'))
            log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname,id='GOOD',which_var = colnames(fresh_nonch)[j],comments = 'GOOD. All values are within the range.')
          }else{
            log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[i,1],which_form = formname, which_var = colnames(fresh_nonch)[j],wrong_val = fresh_nonch[i,j],
                                           comments = paste0('Correct range: ', do.call('paste',as.list(rg)),'. Replaced with NA.'))
            log_replace<-report_wrong(id=fresh_nonch[i,1],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i,j], which_form = formname,comments = 'Step5: Out of range values.',report = log_replace)
            fresh_nonch[i,j]<-NA
            message(paste0('Some values from ',formname," ", colnames(fresh_nonch)[j], ' are out of range. Refer to log_out_of_range for more details.'))
          }
        }
      }
    }
    fresh_nonch<<-fresh_nonch
    log_out_of_range<<-log_out_of_range
    log_replace<<-log_replace
    message(paste0(formname,": STEP5 done."))
  }
  STEP5()
  
  ##STEP6 identify systematic issues based on the log by calculating the number of observations that have the same issue. 
  #If almost all of them have the same issue it may be very likely to be systematic. 
  
  ##STEP7 for checkbox -- fix issues identified in STEP7
  STEP7<-function(){
    fresh_chk<-STEP3(raw_chk) #replace 999 with NA
    vm<-subset(vm,is.checkbox=="TRUE") #subset of var_map where is.checkbox = T
    #STEP7.1
    #####need to check the values of ac var first!
    #STEP7.2 redcap checkbox
    vm_rcchk<-subset(vm,fix_what=="redcap_check") # subset of vm of redcap_check var
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
        if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA
        }else if (as.numeric(fresh_chk[df_i,acvar])==vm_rcchk$value1[vm_i]){
          fresh_chk[df_i,rcvar]<-vm_rcchk$value2[vm_i]
        }}}
    fresh_chk<<-fresh_chk
    message(paste0(formname,": STEP7 done."))
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
}

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



