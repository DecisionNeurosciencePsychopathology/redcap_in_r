## startup
rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
source('~/Documents/github/UPMC/startup.R')
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_old2.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
colnames(var_map)[7]<-'instructions' #TEMP
var_map$instructions<- gsub('=',',',var_map$instructions) #change the format of 'range_fix'
#note: here uses 'QOL interview' as the 'training form'. 

#Initialize reports 
log_out_of_range <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                               which_form=as.character(),comments=as.character(),stringsAsFactors = F) #Report out-of-range values 
log_replace <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report wrong values/datatypes, correct and report 
log_comb_fm <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 
deleted_rows<-list()
#####################################start of the function#########################################
# rctransfer.dataclean <- function(
# [VARIABLES]
#curdb = bsoc
protocol.cur <- ptcs$bsocial
#db = 
bsoc<- bsrc.checkdatabase2()
forms = NULL # A vector. must be exactly the same as the a subset of the form names in the variable mapping. Case sensitive. Space sensitive. 
#range
replace_999 = TRUE # by defult, replace all 999 with NA 
remove_dupid = FALSE # if T, only keep duplicated id with the earliest date 
replace_w_na = FALSE
#) {

## verify Morgan's var_map. 
####for the col is.box. NA should mean represent unecessary variables. i.e. 
# if redcap_var and access_var both exist, is.checkbox cannot be NA
chckmg<-subset(var_map,select = c('redcap_var','access_var'),is.na(is.checkbox))
chckmg[which(!is.na(chckmg$redcap_var)&(!is.na(chckmg$access_var))),] #shoule give us nothing
# vice versa 
chckmg<-subset(var_map,select = c('redcap_var','access_var','is.checkbox'),!is.na(is.checkbox))
which(is.na(chckmg),arr.ind = T) # should give us nothing. if yes, try run the following line of code 
sum(is.na(var_map$is.checkbox)) #of unecessary variabels (based on rows. duplicates included)
var_map$is.checkbox[which(is.na(var_map$redcap_var)&!var_map$is.checkbox)]<-NA
var_map$is.checkbox[which(is.na(var_map$access_var)&!var_map$is.checkbox)]<-NA
sum(is.na(var_map$is.checkbox)) #of unecessary variabels (based on rows. duplicates included)
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
  #STEP1: Select a RC form, get an integrated RC form with complete variables, right variable names, splited ordinary variables with checkbox variables, removed calculated variables 
  STEP1<-function(){# the function is writen and editted in another script. Below is a copy of the script
    #STEP1.1 Select a RC form. Check if multiple origianl forms need to be combined into one form 
    formname <<- forms[form_i] #formname(a character)
    vm<<-subset(var_map, Form_name==formname) #subset of var mapping for the current form
    acvar_nonch<-with(vm,split(access_var,is.checkbox))$'FALSE' #non-checkbox var
    acvar_chk<<-with(vm,split(access_var,is.checkbox))$'TRUE' #checkbox var
    fm_dir<-unique(vm$path) #path of forms
    if (any(is.na(vm$path))){
      stop(message('At least one row in var mapping does not give the path of directory for the original forms')) # path cannot be NA
    }else{if(any(!file.exists(paste0(rootdir,fm_dir)))){stop(message('At least one row of path in var mapping does not exist.'))}}#path must be valid
    if (length(fm_dir)>1){ 
      ######TEMP: end the function. function is not fully developed.############
      stop(message("TEMP: end the function. function is not fully developed."))
      ##################end of TEMP codes 
      #STEP1.2 Get raw. Grab forms, remove col 'X', combine forms by common cols and check if common cols have same values. If not need to combine multiple forms, jump to STEP1.3. 
      comb_fm_list<-lapply(fm_dir, function(fm_dir){read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)}) # grab forms 
      comb_fm_list<-lapply(comb_fm_list, function(x){x[,-which(colnames(x)=='X')]}) # remove col 'X'
      #comb_fm_list<-lapply(comb_fm_list, function(x){x<-x[,which(colnames(x)%in%c(acvar_or,acvar_chk))]}) #remove unnecessary variables
      #STEP1.2.1 Report or remove duplicated ID. No NAs in common cols
      temp_dup_id<-as.vector(unlist(sapply(comb_fm_list, function(x){x[which(duplicated(x[[1]])),1]}))) # get duplicated ID 
      if (length(temp_dup_id)>0){
        if (!as.logical(remove_dupid)){ # report duplicated ID
          log_comb_fm<-report_wrong(id=temp_dup_id,which_var = 'ID',report = log_comb_fm,which_form = formname,comments = 'Duplicated ID. Note: it\'s possible that they are duplicated in each form.')
          log_comb_fm<-unique(log_comb_fm)
          message('Duplicated IDs exist. Refer to log_comb_fm for more info. Forms are stored as comb_fm_list.
      Viewing details of duplicated ID...')}
        temp_chck_dupid<-lapply(comb_fm_list,function(x){x[which(x[[1]]%in%temp_dup_id),]}); # Viewing details of duplicated ID
        View(temp_chck_dupid[[1]]);View(temp_chck_dupid[[2]]);View(temp_chck_dupid[[3]]) #Viewing details of duplicated ID
        remove_dupid<-readline(prompt = 'Enter T to remove duplciated ID; F to just report: ') # to remove duplicated ID based on date 
        if(as.logical(remove_dupid)){
          temp_var_date<-unique(sapply(comb_fm_list, function(x){colnames(x)[2]}))
          if(length(temp_var_date)>1){stop(message('For the forms to be combined, do they have the same 2nd-colname (should be the date)?'))}
          temp_confirm<-readline(prompt = paste(
            'Will remove duplicated ID and keep IDs with the earliest completion date. Please confirm that', temp_var_date,'are the dates. 
          Enter T to continue, F to stop:'))
          if(as.logical(temp_confirm)){ #removed replicated id 
            deleted_rows<<-append(deleted_rows,lapply(comb_fm_list,function(comb_fm){
              df<-do.call('rbind',lapply(split(comb_fm,comb_fm[1]),function(rows_by_id){rows_by_id[-which.min(as.Date(rows_by_id[[2]])),]}))
              df$formname<-formname
              df$whydeleted<-'Duplicated ID'
              df}))  # save the deleted rows in the dataframe 'deleted_rows'
            comb_fm_list<-lapply(comb_fm_list,function(comb_fm){do.call('rbind',lapply(split(comb_fm,comb_fm[1]),function(rows_by_id){rows_by_id[which.min(as.Date(rows_by_id[[2]])),]}))})  # select ID with the earlist date 
            message('Duplicated ID removed.')
          }
          remove_dupid<-F # foreced to report dup ids for the next form 
        }
      }
      #STEP1.2.2 Each form should have the same number of rows
      comm_var<-Reduce(intersect,lapply(comb_fm_list,names)) # get a vector of the names of common cols.
      temp_comm_col_list<-lapply(comb_fm_list, function(x){x<-x[comm_var]}) # get the common cols for each form. all common cols are saved in one list. 
      if(!nlevels(sapply(comb_fm_list, nrow))==0){ # nrows of each AC form should be the same
        stop(message(paste('For the access forms that needs combining:', formname,'do not have the same number of rows. The forms are stored as "comb_fm_list"')))
      }else{message("Good. AC forms have the same number of rows.")}
      temp_na_in_comm_col<-sum(is.na(unlist(temp_comm_col_list)))
      if(temp_na_in_comm_col>1){stop(message(paste0('For the access forms that needs combining: ', formname,'. There are ', temp_na_in_comm_col,' NAs in the common columns. The common columns are stored as "temp_comm_col_list".')))}
      #STEP1.2.3 Check if common cols have identical values 
      ######TEMP:the cols are not identical. debugging codes######
      tempdf1<-temp_comm_col_list[[1]]
      tempdf2<-temp_comm_col_list[[2]]
      tempdf3<-temp_comm_col_list[[3]]
      ###################end of debugging codes###############
      rm(comb_fm_list,temp_comm_col_list,temp_dup_id,fm_dir,temp_chck_dupid,temp_confirm,temp_var_date,remove_dupid,temp_na_in_comm_col) #keep comm_var, form_i, formname, vm, acvar_nonch, acvar_chk
    }else{#STEP1.3 get 'raw'--all vars. IDs can be duplicated 
      raw <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F) #grab form 
    }
    #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx varsto df: 'raw_chk'
    raw_nonch<-raw[,which(colnames(raw)%in%acvar_nonch)] #keep only non-checkbx variables 
    if(!is.null(acvar_chk)){raw_chk<<-raw[,which(colnames(raw%in%acvar_chk))]}
    #STEP1.5 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames
    rcvar_nonch<-with(vm,split(redcap_var,is.checkbox))$'FALSE' #non-checkbox var
    if(!length(acvar_nonch)==length(rcvar_nonch)){
      stop(message('Something is wrong. Check the codes.'))
    }else{
      colnames(raw_nonch)<-replace(colnames(raw_nonch),match(acvar_nonch,colnames(raw_nonch)),rcvar_nonch)
    }
    #STEP1.6 remove calculated fields 
    cal_var<-subset(vm,fix_what=='calculated_field')$redcap_var
    if(length(cal_var)>0){raw_nonch<-raw_nonch[,-which(colnames(raw_nonch)%in%cal_var)]}
    
    rawdata<<-raw
    raw_nonch<<-raw_nonch
  }
  STEP1() # get 'raw_nonch': redcap variables, 
  
  ##STEP2 change data type 
  # identify all non-integer/numeric col
  #Dates (change date to date (YYYY-MM-DD))
  
  ##STEP3 get 'fresh_nonch'. Report 999 AND if replace_999=T, replace 999's with NA
  STEP3<-function(){
    if (length(which(raw_nonch==999))>0){
      log_replace<-rbind(log_replace,(do.call("rbind",apply(which(raw_nonch==999,arr.ind = T),1,function(indeX){ # TO BE GENERALIZED
        report_wrong(report = log_replace, id=raw_nonch[indeX[1],1],which_var = colnames(raw_nonch)[indeX[2]],
                     wrong_val = 999, which_form = formname, rbind = F,
                     comments = ifelse(replace_999,'Replaced with NA','Not replaced with NA yet'))
      })))) 
      if(replace_999){raw_nonch[which(raw_nonch==999,arr.ind = T)]<-NA}
    }else {message(paste('Form',formname,'does not have any value of 999'))}
    log_replace<<-log_replace
    fresh_nonch<<-raw_nonch
  }
  STEP3()
  ##STEP4 fix data with systematic issues (eg: shifted range) identified in 'var_map'
  STEP4<-function(){
    #STEP4.1 systematically shifted (eg: 1-false; 2-true) #range_fix
    fixmap<-subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)) 
    if(nrow(fixmap)>0) {for (i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
      valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[i],")"))),ncol = 2,byrow = T)
      if (all(is.na(fresh_nonch[[fixmap$redcap_var[i]]]))){
        message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[i],'" so no need to do "range_fix"'))
      }else{
        fresh_nonch[fixmap$redcap_var[i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[i]]],from = valuemap[,1], to = valuemap[,2])
      }}}
    #STEP4.2 range_allowed
    fixmap<-subset(vm,fix_what=='range_allowed',select = c(redcap_var,instructions))
    if(nrow(fixmap)>0) {for (i in 1:nrow(fixmap)){ #if there's 'range_allowed' problem, fix the problem one variable by one var
      thecol<-fresh_nonch[fixmap$redcap_var[i]] # the col with the problem 
      if(!is.numeric(thecol[[1]])){ # values in the col should be all numeric (or NA)
        stop(message(paste0('Form "',formname,'" has non-numeric values in column "',fixmap$redcap_var[i],'" so "range_allowed" cannot be fixed')))
      }else{
        eval(parse(text=paste0('rg<-seq(',fixmap$instructions[i],')'))) #get rg: range specified in var_map
        row_i<-which(!((thecol[[1]] %in% rg) | is.na(thecol[[1]]))) # report values that is not in the range. NA is acceptable 
        if (length(row_i)==0){
          message(paste('Fixing issue "range_allowed" GOOD.:', formname,fixmap$redcap_var[i],'are within the range (NA is allowed).'))
        }else{
          log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[row_i,1],which_form = formname, which_var = fixmap$redcap_var[i],wrong_val = thecol[row_i,1],
                                         comments = 'range_allowed')
          message('Fixing issue "range_allowed" GOOD.: Some values are out of range. Refer to log_out_of_range for more details.')
        }}}}
    #STEP4.3 date
    
    fresh_nonch<<-fresh_nonch
  }
  
  
  #STEP4.2 unreasonable date
  #STEP4.3 special issues (occur in only one form)
  sp1var<-subset(var_map,fix_what=='special_1',select = redcap_var)[[1]]
  QOL_fresh[,sp1var]<-as.data.frame(apply(QOL_fresh[,sp1var],2,function(x){gsub('1899-12-30','',x)}))
  
  #STEP4.4 calculated_field= don't transfer this one
  #range_allowed (redcap range is WIDER than Access range)
  
  
  
  
  ##STEP5 
  #Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
  STEP6<-function(){
    if(!replace_999){message('Warn: 999 has not been replaced yet.')}
    for (j in 1:length(colnames(fresh_nonch))) {
      rg<-bsrc.getchoicemapping(variablenames = colnames(fresh_nonch)[j],metadata = bsoc$metadata)[[1]] # get the range 
      if(is.null(rg)){log_out_of_range<-report_wrong(report = log_out_of_range,which_form = 'QOL',id='OKAY-NO_RANGE',which_var = colnames(QOL_fresh)[j],comments = 'This variable has no range') # variable should have a range 
      } else {
        if (any(is.na(as.integer(rg)))){ # the range should be integer 
          stop(message(paste('The range of variable',colnames(QOL_fresh)[j],'is not integer or contain NA. Stop the function.')))
        }else{
          rg<-as.integer(rg)
          i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]]))) # report values that is not in the range. NA is acceptable 
          if (length(i)==0){
            message(paste('GOOD. All values of', formname,'are within the range.'))
            log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname,id='GOOD',which_var = colnames(QOL_fresh)[j],comments = 'GOOD. All values are within the range.')
          }else{
            log_out_of_range<-report_wrong(report = log_out_of_range,id=QOL_fresh[i,1],which_form = 'QOL', which_var = colnames(QOL_fresh)[j],wrong_val = QOL_fresh[i,j],
                                           comments = paste('Correct range:', do.call('paste',as.list(rg))))
            message('Some values are out of range. Refer to log_out_of_range for more details.')
          }}}}
    log_out_of_range<<-log_out_of_range
  }
  
  
  
  ##STEP7 identify systematic issues based on the log by calculating the number of observations that have the same issue. 
  #If almost all of them have the same issue it may be very likely to be systematic. 
  
  
  
  ##STEP8 fix issues identified in STEP7
  
  #STEP10 checkbox (redcap_check= redcap is checkbox, access_check=access is checkbox, both_check=both are checkbox)
  #STEP10.2 match checkbox variabels with other variabels using matching_id
  
  
}
#}




#####################################end of the function#########################################












#### original codes   

# import data from access and match variables  # TO BE GENERALIZED 
QOL_raw <- read.csv(paste0(rootdir,"QOL_raw.csv"), stringsAsFactors = F) 
#rename the variables to something more reasonable (i.e. var names in redcap): 
QOL_fresh <- dplyr::select(QOL_raw, ID, #FOLOQOL, DATEQOL, 
                           TIME.BEGAN, QOLBA1:TIME.ENDED)
#get variables for qol
rd.var.map("qol")->qolvarmap
#change variable names to match redcap
names(QOL_fresh)<-qolvarmap[-c(18:23, 26, 77)]



#Range problems:
##Range problems for DT scale (1-7)
#which ones don't fit get probs=1
qol.range(range=c(1:7), c(3, 20:22, 32:35, 38, 39, 
                          44:46, 70:72, 78:80, 84:86, 88:91))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 3, 20:22, 32:35, 38, 39, 44:46, 70:72, 78:80, 84:86, 88:91)]->qolprobs
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs %>% gather(key="question", value="original",-registration_redcapid)->qolprobs
qolprobs[which(!qolprobs$original %in% c(1:7) & !is.na(qolprobs$original)),]->qolprobs
mutate(qolprobs, new=NA)->qolprobs
#Change the ones that don't fit to NA
qol.na(range=c(1:7), cols=c(3, 20:22, 32:35, 38, 39, 44:46, 70:72, 78:80, 84:86, 88:91))->QOL_fresh

##Range problems for living situations (1-16)
qol.range(range=c(1:16), c(4, 8, 10, 12, 14, 16))->QOL_fresh
#which ones don't fit (No range problems here)
QOL_fresh[which(QOL_fresh$probs==1),c(1, 4, 8, 10, 12, 14, 16)]->qolprobs2

##Range problems for YES/NO
qol.range(range=c(0:1,9), c(23:30, 47:60,65:69, 81:82))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 23:30, 47:60,65:69, 81:82)]->qolprobs3
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs3 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs3
qolprobs3[which(!qolprobs3$original %in% c(0:1) & !is.na(qolprobs3$original)),]->qolprobs3
mutate(qolprobs3, new=NA)->qolprobs3
#Change the ones that don't fit to NA
qol.na(range=c(1:7), cols=c(23:30, 47:60,65:69, 81:82))->QOL_fresh

##Range problems for 1:4 items
qol.range(range=c(1:4), c(31,64))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 31, 64)]->qolprobs4
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs4 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs4
qolprobs4[which(!qolprobs4$original %in% c(1:4) & !is.na(qolprobs4$original)),]->qolprobs4
mutate(qolprobs4, new=NA)->qolprobs4
#Change the ones that don't fit to NA
qol.na(range=c(1:4), cols=c(31,64))->QOL_fresh

##Range problems for 0:5 items
qol.range(range=c(0:5), c(36:37))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 36:37)]->qolprobs5
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs5 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs5
qolprobs5[which(!qolprobs5$original %in% c(0:5) & !is.na(qolprobs5$original)),]->qolprobs5
mutate(qolprobs5, new=NA)->qolprobs5
#Change the ones that don't fit to NA
qol.na(range=c(0:5), cols=c(36:37))->QOL_fresh

##Range problems for 1:5
qol.range(range=(1:5), c(40:43, 87))->QOL_fresh
#which ones don't fit
QOL_fresh[which(QOL_fresh$probs==1),c(1, 40:43, 87)]->qolprobs6
#Make dataframe of missing original (ID, question, original value, new value)
qolprobs6 %>% gather(key="question", value="original",-registration_redcapid)->qolprobs6
qolprobs6[which(!qolprobs6$original %in% c(1:5) & !is.na(qolprobs6$original)),]->qolprobs6
mutate(qolprobs6, new=NA)->qolprobs6
#Change the ones that don't fit to NA
qol.na(range=c(1:5), cols=c(40:43, 87))->QOL_fresh

##Range problems for 0:2- no issues
which(!QOL_fresh$qol_i_1 %in% c(1:5))


#Put all range problems together    
qol.range.probs<-rbind(qolprobs, qolprobs3, qolprobs4, qolprobs5, qolprobs6)

#Check for duplicates: in the event that the same ID has two entries within a single follow-up, just take the earliest one
any(duplicated(QOL_fresh$registration_redcapid))

#FIGURE OUT IDS LAST
bsrc.findid(QOL_fresh,idmap = idmap,id.var = "registration_redcapid")->QOL_fresh
if(any(!QOL_fresh$ifexist)){message("ERROR: NOT ALL IDS EXIST IN MASTER DEMO, PLEASE FIX. Here are their soloff ids:")
  print(QOL_fresh[which(!QOL_fresh$ifexist),"registration_redcapid"])}
#Figure out NAs    
qol.remove.na<-function(cols){for (i in 1:nrow(QOL_fresh)){
  QOL_fresh[i, cols]<-
    sapply(QOL_fresh[i, cols], function(x){
      ifelse (is.na(x), x<-999, x<-x)})}
  return(QOL_fresh)}
qol.remove.na(c(3, 4, 8, 10, 12, 14, 16, 19:60, 65:73, 78:82, 84:86, 88:91))->QOL_fresh  
as.data.frame(names(QOL_fresh))->r






