#################################### SAME #################################### 
## startup
rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
source('~/Documents/github/UPMC/startup.R')
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice.csv',stringsAsFactors = FALSE)
var_map[which(var_map=="",arr.ind = T)]<-NA
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
deleted_rows<-list()
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
    message(paste0("Cleaning form:",formname," now..."))
    vm<-subset(var_map, Form_name==formname) #subset of var mapping for the current form
    acvar_nonch<-with(vm,split(access_var,is.checkbox))$'FALSE' #non-checkbox var
    acvar_chk<-with(vm,split(access_var,is.checkbox))$'TRUE' #checkbox var
    fm_dir<-unique(vm$path) #path of forms
    if (any(is.na(vm$path))){
      stop(message('At least one row in var mapping does not give the path of directory for the original forms')) # path cannot be NA
    }else{if(any(!file.exists(paste0(rootdir,fm_dir)))){stop(message('At least one row of path in var mapping does not exist.'))}}#path must be valid
    #STEP1.2 Get raw. Grab forms, remove unecessary variables, combine forms by common cols and remove rows with different values in the common cols. If not need to combine multiple forms, jump to STEP1.3. 
    if (length(fm_dir)>1){ 
      comb_fm_list<-lapply(fm_dir, function(fm_dir){read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)}) # grab forms 
      #comb_fm_list<-lapply(comb_fm_list, function(x){x[,-which(colnames(x)=='X')]}) # remove col 'X'
      comb_fm_list<-lapply(comb_fm_list, function(x){x<-x[,which(colnames(x)%in%c(acvar_nonch,acvar_chk))]}) #remove unnecessary variables
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
            new_deleted_rows<-lapply(comb_fm_list,function(comb_fm){
              df<-do.call('rbind',lapply(split(comb_fm,comb_fm[1]),function(rows_by_id){rows_by_id[-which.min(as.Date(rows_by_id[[2]])),]}))
              df$formname<-formname
              df$whydeleted<-'Duplicated ID'
              df})
            names(new_deleted_rows)<-paste0(formname,"_dupID_",1:length(new_deleted_rows))
            deleted_rows<-append(deleted_rows,new_deleted_rows)  
            comb_fm_list<-lapply(comb_fm_list,function(comb_fm){do.call('rbind',lapply(split(comb_fm,comb_fm[1]),function(rows_by_id){rows_by_id[which.min(as.Date(rows_by_id[[2]])),]}))})  # select ID with the earlist date 
            message('Checking duplicated ID...')
            if(length(as.vector(unlist(sapply(comb_fm_list, function(x){x[which(duplicated(x[[1]])),1]}))))==0){
              message('Duplicated ID removed.')
            }else{stop(message('Duplicated ID not removed! Check codes.'))}
          }
          remove_dupid<-F # foreced to report dup ids for the next form 
        }
      }
      #STEP1.2.2 Get common cols. Each form should have the same number of rows
      comm_var<-Reduce(intersect,lapply(comb_fm_list,names)) # get a vector of the names of common cols.
      temp_comm_col_list<-lapply(comb_fm_list, function(x){x<-x[comm_var]}) # get the common cols for each form. all common cols are saved in one list. 
      if(!nlevels(sapply(comb_fm_list, nrow))==0){ # nrows of each AC form should be the same
        stop(message(paste('For the access forms that needs combining:', formname,'do not have the same number of rows. The forms are stored as "comb_fm_list"')))
      }else{message(paste("Good. Access forms",formname, "have the same number of rows."))}
      temp_na_in_comm_col<-sum(is.na(unlist(temp_comm_col_list))) # should have no NAs in common cols
      if(temp_na_in_comm_col>1){
        stop(message(paste0('For the access forms that needs combining: ', formname,', there are ', temp_na_in_comm_col,' NAs in the common columns. The common columns are stored as "temp_comm_col_list".')))
      }else{message(paste("Good. Access forms",formname, "do not have NAs in the common cols."))}
      if(any(unlist(sapply(comb_fm_list,function(df){duplicated(df[[1]])})))){ # should be no duplciated IDs in the common cols 
        stop(message(paste0('For the access forms that needs combining: ', formname,', there are duplicated IDs. The common columns are stored as "temp_comm_col_list".')))
      }else{message(paste("Good. Access forms",formname, "do note have duplicated IDs."))}
      temp_confirm2<-readline(prompt = paste("Enter T to confirm this variable:",comm_var[2],"refers to date: "))
      #STEP1.2.3 replace dates using dates of the first form 
      if(!as.logical(temp_confirm2)){stop()}else{
        iddate<-temp_comm_col_list[[1]][,1:2]#;iddate<-iddate[order(iddate[1]),]
        new_log_replace<-do.call("rbind",lapply(temp_comm_col_list,function(x){ #log replacement
          temp_repo<-dplyr::anti_join(x[1:2],iddate)
          if(nrow(temp_repo)>1){report_wrong(id=temp_repo[[1]],which_var = comm_var[2], wrong_val = temp_repo[[2]],which_form = formname,comments = "The date is changed when combing with other forms",report = log_replace,rbind = F)}
        })) 
        if(is.null(new_log_replace)){
          message(paste("No date data is replaced when combining forms for", formname))
        }else{message(paste("Some date data is replaced when combining forms for", formname,". Refer to log_replace for details."))}
        log_replace<-rbind(log_replace,new_log_replace)
        temp_comm_col_list<-lapply(temp_comm_col_list,function(x){x[2]<-plyr::mapvalues(x[[1]],from = iddate[[1]], to = iddate[[2]]); x}) #update dates for common cols
        for(i in 1:length(temp_comm_col_list)){comb_fm_list[[i]][comm_var]<-temp_comm_col_list[[i]]} #update dates for the combined_forms_list
      }
      #STEP1.2.4 Remove rows that have different values in the common cols. 
      new_comm_col<-Reduce(dplyr::inner_join,temp_comm_col_list) # innerjoin common cols
      removed_rows<-nrow(temp_comm_col_list[[1]])-nrow(new_comm_col)
      if(removed_rows>0){ #report removed rows 
        message(paste(removed_rows,"rows are removed when combining the forms for",formname,". 
        They have severl weird values (eg: mistype of id (7162->7165)) in the common cols but are probably usable. Refer to log_replace and deleted_rows for details"))
        removedid<-unique(unlist(sapply(temp_comm_col_list,function(x){setdiff(x[[1]],new_comm_col[[1]])})))
        new_deleted_rows<-lapply(comb_fm_list,function(comb_fm){
          df<-comb_fm[which(!comb_fm[[1]]%in%new_comm_col[[1]]),]
          df$formname<-formname
          df$whydeleted<-'Different values in the common cols across forms'
          df})
        names(new_deleted_rows)<-paste0(formname,"_CommCol_",1:length(new_deleted_rows))
        deleted_rows<-append(deleted_rows,new_deleted_rows)  
        log_replace<-report_wrong(id = removedid,which_var = "REMOVED", wrong_val = "REMOVED",which_form = formname, comments = "DELETED ROWS when importing/combining forms",report = log_replace,rbind = T)
      }
      #if(any(!sapply(temp_comm_col_list,function(x){identical(temp_comm_col_list[[1]],x)}))){stop(message(paste("Combining forms for",formname,"Common cols not identical.")))} #Check if common cols have identical values
      comb_fm_list<-lapply(comb_fm_list,function(x){x<-dplyr::inner_join(x,new_comm_col)}) #remove some rows where the common rows have different values across forms
      #STEP1.2.5 get 'raw' -- necessary vars from all multiple forms. IDs are unique. 
      raw<-comb_fm_list[[1]]
      for (comb_i in 2:length(comb_fm_list)){raw<-dplyr::left_join(raw,comb_fm_list[[comb_i]],by=comm_var)}
      if(!nrow(raw)==nrow(new_comm_col)){stop(message(paste("Some thing is wrong with",formname,"when combining forms. Check codes.")))}
      
    }else{#STEP1.3 get 'raw'-- necessary vars. IDs can be duplicated 
      raw <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F) #grab form 
      raw<-raw[,which(colnames(raw)%in%c(acvar_nonch,acvar_chk))] #remove unncessary var 
    }
    #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx varsto df: 'raw_chk'
    raw_nonch<-raw[,which(colnames(raw)%in%acvar_nonch)] #keep only non-checkbx variables 
    if(!is.null(acvar_chk)){
      raw_chk<-raw[1]
      raw_chk<-cbind(raw_chk,raw[,which(colnames(raw)%in%acvar_chk)])
      raw_chk$matching_id<-1:nrow(raw) #give checkbox df a matching id
    }
    #STEP1.5 remove calculated fields 
    cal_var<-subset(vm,fix_what=='calculated_field')$access_var
    if(length(cal_var)>0){raw_nonch<-raw_nonch[,-which(colnames(raw_nonch)%in%cal_var)]}
    #STEP1.6 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames
    VMAP<-subset(vm,select=c(access_var,redcap_var),is.checkbox=='FALSE')
    ##STEP special: for IPDE, keep some original access variable names to fix "check_equal", "multi_field", "special_2" issues later
    if(formname=="IPDE"){for (tempvar in c("APDa5","APDa6","BPD3","BPD4","SPD5","STPD8")){VMAP[which(VMAP$access_var==tempvar),2]<-tempvar}}
    colnames(raw_nonch)<-plyr::mapvalues(colnames(raw_nonch),from = VMAP$access_var, to = VMAP$redcap_var)
    if(any(duplicated(colnames(raw_nonch)))){stop(message(paste0("Stop: ",formname,": Duplicated colnames.")))}
    if(!is.null(acvar_chk)){raw_nonch$matching_id<-1:nrow(raw)} #get non-check df a matching id if needed
    
    vm<<-vm
    formname<<-formname
    acvar_chk<<-acvar_chk
    rawdata<<-raw
    deleted_rows<<-deleted_rows
    if(!is.null(acvar_chk)){raw_chk<<-raw_chk}
    raw_nonch<<-raw_nonch
    log_replace<<-log_replace
    message(paste0(formname,": STEP1 done."))
  }
  
} # remove this 


