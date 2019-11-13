#################################### SAME #################################### 
## startup
rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
source('~/Documents/github/UPMC/startup.R')
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_old2.csv',stringsAsFactors = FALSE)
var_map[which(var_map=="",arr.ind = T)]<-NA
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
#var_map[[8]]<-sapply(var_map[[8]], function(x) gsub("\"", "", x))###TEMP
## TEMP so that NA in 'is.checkbox' means that 

remove_dupid = FALSE # if T, only keep duplicated id with the earliest date 
#Initialize reports 
log_comb_fm <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 
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
deleted_rows<-list()
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
    
    raw<<-raw
    raw_nonch<<-raw_nonch
  }
  
  
} # remove this 


