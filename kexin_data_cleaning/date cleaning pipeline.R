## startup
rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
source('~/Documents/github/UPMC/startup.R')
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA

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

#for (form_i in 1:length(forms)) {
for (form_i in 1:7) {
  #STEP1: Select a RC form, get an integrated RC form with complete variables, right variable names, splited ordinary variables with checkbox variables, removed calculated variables 
  STEP1<-function(){
    #STEP1.1 Select a RC form. Check if multiple origianl forms need to be combined into one form 
    formname <- forms[form_i] #formname(a character)
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
        if(is.null(log_replace)){
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
    if(!is.null(acvar_chk)){acvar_chk<<-acvar_chk}
    rawdata<<-raw
    deleted_rows<<-deleted_rows
    if(!is.null(acvar_chk)){raw_chk<<-raw_chk}
    raw_nonch<<-raw_nonch
    log_replace<<-log_replace
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
  fresh_nonch<-STEP3()
  ##STEP fix data with systematic issues (eg: shifted range) identified in 'var_map'
  STEP4<-function(){
    #STEP4.1 range_fix: range in access is not the same as range in redcap, specifies first access variable, then redcap variable to change to
    fixmap<-unique(subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)))
    if(nrow(fixmap)>0) {for (step4_i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
      valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T)
      if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
        message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
      }else{
        fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2])
      }}}
    #STEP4.2 range_allowed: The range in Redcap allows more values than we accept from what should have been the range in redcap. Specifies the new range
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
          message('Fixing issue "range_allowed". Some values are out of range. Refer to log_out_of_range for more details.')
        }}}}
    #STEP4.3 date: must be converted to date (YYYY-MM-DD)
    fixmap<-unique(subset(vm,fix_what=='date',select = c(redcap_var,instructions)))
    if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
      fresh_nonch[fixmap$redcap_var[step4_i]]<-as.Date(fresh_nonch[fixmap$redcap_var[step4_i]][[1]],format = fixmap$instructions[step4_i])
    }}
    #STEP 4.4 check_equal: These two values in access should be equal before being imported. Throw an error if they are different
    fixmap<-subset(vm,fix_what=='check_equal',select = c(access_var,instructions))
    if (nrow(fixmap)>0){ #if there's 'check_equal' problem, fix the problem one variable by one var
      fixmap$instructions<-gsub("=",",",fixmap$instructions)
      for (step4_i in 1:nrow(fixmap)){
        temp_check<-subset(fresh_nonch,select = eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))))
        if(!all(temp_check[[1]]==temp_check[[2]])){stop(message(paste0(formname,"'s ",fixmap$instructions[step4_i]," are not equal.")))}
        rm(temp_check)
      }}
    #STEP4.5 One access variable goes into multiple redcap variables
    fixmap<-subset(vm,fix_what=='multi_field',select = c(access_var,instructions))
    if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
      newvar<-gsub(" ","",strsplit(fixmap$instructions[step4_i],",")[[1]]) #new rc var
      fresh_nonch<-cbind(fresh_nonch,replicate(length(newvar),fresh_nonch[fixmap$access_var[step4_i]])) #duplicate the ac col and then rbind the cols to the original df
      colnames(fresh_nonch)<-append(colnames(fresh_nonch),newvar) #update the colnames to include the new rc var
    }}
    #STEP4.6 special_3: range_fix+range_allowed , 1=1, 2=2, 3=3, 4=5 (5 out of range)
    fixmap<-unique(subset(vm,fix_what=='special_3',select = c(redcap_var,instructions)))
    if(nrow(fixmap)>0) { #if there's 'special_3' problem
      #range_allowed
      thecol<-fresh_nonch[fixmap$redcap_var[step4_i]] # the col with the problem 
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
          message('Fixing issue "special_3". Some values are out of range. Refer to log_out_of_range for more details.')
        }}
      #range_fix- copied the codes in step 4.1
      valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T)
      if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
        message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
      }else{
        fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2])
      }}
    
    #STEP4.7 special_1 needs to be changed to time (HH:SS)
    sp1var<-subset(vm,fix_what=='special_1',select = redcap_var)[[1]]
    if(length(sp1var)>1){fresh_nonch[,sp1var]<-as.data.frame(apply(fresh_nonch[,sp1var],2,function(x){gsub('1899-12-30','',x)}))}
    
    
    fresh_nonch<<-fresh_nonch
    log_out_of_range<<-log_out_of_range
  }
  STEP4()
  
  ##STEP5 
  #Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
  STEP5<-function(){
    if(!replace_999){message('Warn: 999 has not been replaced yet.')}
    for (j in 1:length(colnames(fresh_nonch))) {
      rg<-bsrc.getchoicemapping(variablenames = colnames(fresh_nonch)[j],metadata = bsoc$metadata)[[1]] # get the range 
      if(is.null(rg)){log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname, id='OKAY-NO_RANGE',which_var = colnames(fresh_nonch)[j],comments = 'This variable has no range') # variable should have a range 
      } else {
        if (any(is.na(as.integer(rg)))){ # the range should be integer 
          stop(message(paste('The range of variable',colnames(fresh_nonch)[j],'is not integer or contain NA. Stop the function.')))
        }else{
          rg<-as.integer(rg)
          i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]]))) # report values that is not in the range. NA is acceptable 
          if (length(i)==0){
            message(paste('GOOD. All values of', formname,'are within the range.'))
            log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname,id='GOOD',which_var = colnames(fresh_nonch)[j],comments = 'GOOD. All values are within the range.')
          }else{
            log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[i,1],which_form = 'QOL', which_var = colnames(fresh_nonch)[j],wrong_val = fresh_nonch[i,j],
                                           comments = paste('Correct range:', do.call('paste',as.list(rg))))
            message('Some values are out of range. Refer to log_out_of_range for more details.')
          }}}}
    log_out_of_range<<-log_out_of_range
  }
  STEP5()
  
  ##STEP6 identify systematic issues based on the log by calculating the number of observations that have the same issue. 
  #If almost all of them have the same issue it may be very likely to be systematic. 
  
  ##STEP7 fix issues identified in STEP7
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
        }else if (fresh_chk[df_i,acvar]==vm_rcchk$value1[vm_i]){
          fresh_chk[df_i,rcvar]<-vm_rcchk$value2[vm_i]
        }}}
    fresh_chk<<-fresh_chk
  }
  if(!is.null(acvar_chk)){STEP7()}
  
  
  #STEP8 match checkbox variabels with other variabels using matching_id
  STEP8<-function(){
    fresh_alldata<-dplyr::inner_join(fresh_nonch,fresh_chk[-1],by = "matching_id")
    if(!max(fresh_alldata$matching_id)==nrow(fresh_alldata)){stop(message("The last check: something is wrong."))}
    fresh_alldata<<-fresh_alldata
  }
  if(!is.null(acvar_chk)){STEP8()}
  
  assign(paste0("df_",form_i),fresh_alldata)
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






