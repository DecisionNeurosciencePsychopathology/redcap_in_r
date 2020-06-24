#startup
setwd("~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/")
source('~/Documents/github/UPMC/startup.R')
library(tidyverse)
############startuptransfer#############
rootdir="~/Box/skinner/data/Redcap Transfer/NP DATAPULL/"
logdir="~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/log/" # directory of folder to save duplicated IDs that are removed from database
#allsub<-read.csv(paste0("~/Box/skinner/data/Redcap Transfer/All protect data/ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/FINAL_np_varmap.csv',stringsAsFactors = FALSE) %>% mutate_all(~replace(.,.=="",NA)) %>% distinct()

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

all_wtar<-readxl::read_xlsx("~/Box/skinner/data/Redcap\ Transfer/NP DATAPULL/dbo_A_WTAR.xlsx")
#####################################start of the function#########################################

protocol.cur =  ptcs$protect
curdb<-bsrc.checkdatabase2(protocol = protocol.cur)

forms = c("dbo_A_EXIT","dbo_A_WTAR","dbo_A_RBANS", "A_MDRS") # A vector. must be exactly the same as the a subset of the form names in the variable mapping. Case sensitive. Space sensitive. 
skipotherforms = FALSE
remove_dupid = FALSE # if T, remove all rows that involve duplicated IDs  
replace_w_na = TRUE
#) {

######## start of the cleaning process ##########
# PREPARE variable: access forms
all_formnm<-with(var_map,gsub(".xlsx","",unique(na.omit(path)))) #get all redcap formnames  
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
if (!all(paste0(forms,".xlsx") %in% unique(var_map$path))){stop("A least one form cannot be found in the variable map.")}
var_map<-subset(var_map,path %in% paste0(forms,".xlsx"))
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


#STEP1: Select a Access form - baseline(bl) first and then other forms- match with redcap variables, splited ordinary variables with checkbox variables, removed calculated variables 
STEP1<-function(RAWDATA=NULL){
  #STEP1.1 Select a RC form. 
  formname <- forms[form_i] 
  cat(paste0("STEP1: #", form_i," Cleaning form: ",formname," now...\n"))
  fm_dir<-paste0(formname,".xlsx")
  vm<-subset(var_map, path==fm_dir) #subset of var mapping for the current form
  if(!(sum(vm$baseline)==0|sum(vm$baseline)==nrow(vm))){stop(message("check the column 'baseline' in the var map"))
  }else{ifbl<-any(vm$baseline)} #ifbl: if baseline 
  
  if(!skipotherforms|ifbl){ # skip the form if skipotherforms is T AND ifbl is F
    if (any(is.na(vm$path))){
      stop(message('At least one row in var mapping does not give the path of directory for the original forms')) # path cannot be NA
    }
    #STEP1.2 Get raw. Grab forms, remove unecessary people and variables
    if(is.null(RAWDATA)){RAWDATA <- readxl::read_xlsx(paste0(rootdir,fm_dir),sheet = 1) }#grab form 
    RAWDATA<-as.data.frame(RAWDATA) %>% bsrc.findid(idmap = idmap) %>% filter(ifexist) %>% select(-masterdemoid,-wpicid,-soloffid,-ogid,-ifexist)
    View(RAWDATA) 
    w_acvar<-setdiff(colnames(RAWDATA),vm$access_var)#all access variesbles should be in var map
    if(length(w_acvar)>0){warning(paste("step1.2","#",form_i,formname,"Warning:",paste(w_acvar,collapse = ","),"cannot be found in the var_map."))} # report ^
    w_rcvar<-setdiff(na.omit(vm$access_var),colnames(RAWDATA))# all access_var in var mapping should be in actual Access forms
    if(length(w_rcvar)>0){warning(paste("step1.2","#:",form_i,formname,"Warning:",paste(w_rcvar,collapse = ", "),"in the var_map does not match any variables in the forms."))} # report^
    RAWDATA<-RAWDATA %>% select(filter(vm,!is.na(is.checkbox)) %>% pull(access_var)) #remove unncessary var 
    #STEP1.3 no NA in ID or CDATE. create IDDATE. IDDATE must be unique
    if(any(is.na(RAWDATA$ID)|is.na(RAWDATA$CDATE))){stop(message(paste("NA in ID or CDATE of RAWDATA. Form:",formname)))}
    if("CDATE"%in%colnames(RAWDATA)){ #if the dataframe has CDATE
      RAWDATA$CDATE<-as.Date(RAWDATA$CDATE)
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
        reportdup<-reportdup[order(reportdup$IDDATE),];reportdup[which(is.na(reportdup),arr.ind = T)]<-""
        write.csv(reportdup,file = paste0(logdir,formname,"_dup_idcdate_rows.csv"))
        RAWDATA<-RAWDATA[-which(RAWDATA$IDDATE%in%dup_id),]}      #remove duplicated rows
    }
    #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx vars to df: 'raw_chk'
    raw_nonch<-RAWDATA
    #STEP1.6 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames. make sure there's a column cdate 
    VMAP<-unique(subset(vm,select=c(access_var,redcap_var),!is.na(redcap_var)))
    if(any(duplicated(na.omit(VMAP$access_var)))){warning(paste("setp1.6","#",form_i,formname,"Variable mapping... \nWarning: some non-checkbox access variable matches multiple redcap variabels in form",formname))} #check if one ac var matches multiple rc var 
    colnames(raw_nonch)<-plyr::mapvalues(colnames(raw_nonch),from = VMAP$access_var, to = VMAP$redcap_var,warn_missing = F)
    if("CDATE"%in%colnames(RAWDATA)){colnames(raw_nonch)[grep("^CDATE",colnames(raw_nonch))]<-"CDATE"} # rename cdatecopy 
    if(!all(colnames(raw_nonch)%in%c(VMAP$redcap_var,"CDATE","IDDATE","MISSCODE"))){stop(message(paste0(formname," has an error after var mapping when checking: new colnames should contain only CDATE, IDDATE, and redcap variables")))} # check: new colnames should contain only CDATE, IDDATE, and redcap variables 
    if("CDATE"%in%colnames(RAWDATA)){raw_nonch<-cbind(raw_nonch[,-which(colnames(raw_nonch)=="CDATE")],CDATE=raw_nonch$CDATE)} # keep only one col of CDATE
    if(any(duplicated(colnames(raw_nonch)))){stop(message(paste0("Stop: ",formname,": Duplicated colnames.")))}
    #STEP1.7 copy the column CDATE and rename as cdate_formname
    if("CDATE"%in%colnames(RAWDATA)){raw_nonch<-cbind(raw_nonch,newcol=raw_nonch$CDATE)
    colnames(raw_nonch)<-gsub("newcol",tolower(paste0("cdate_",formname)),colnames(raw_nonch))}
    
    cat(paste0(formname,": STEP1 done.\n"))
    vm<<-vm
    formname<<-formname 
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
        log_out_of_range<-report_wrong(report = log_out_of_range,id=ifelse(rep(ifbl,length(row_i)),fresh_nonch[row_i,1],fresh_nonch[row_i,"IDDATE"]),which_form = formname, which_var = fixmap$redcap_var[step4_i],wrong_val = thecol[row_i,1],
                                       comments = 'range_allowed')
        if(replace_w_na){ #Replace out-of-range values with NA 
          fresh_nonch[row_i,fixmap$redcap_var[step4_i]]<-NA
          log_replace<-report_wrong(id=ifelse(rep(ifbl,length(row_i)),fresh_nonch[row_i,1],fresh_nonch[row_i,"IDDATE"]),which_var = fixmap$redcap_var[row_i], wrong_val = thecol[row_i,1],which_form = formname,comments = 'Fixing "range_allowed": Out of the range.The value is replaced with NA',report = log_replace)
          message('Fixing issue "range_allowed": Some values are out of range. Refer to log_out_of_range for more details. The out-of-range values are replcaed with NA.')}
      }}}}
  
  fresh_nonch<<-fresh_nonch
  log_out_of_range<<-log_out_of_range
  log_replace<<-log_replace
  cat(paste0(formname,": STEP2 done.\n"))
}
##STEP3 Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
STEP3<-function(){
  cat(paste("#",form_i,formname,"- performning STEP3 now...\n"))
  deleted_rows<-fresh_nonch[1,];deleted_rows<-deleted_rows[-1,];deletedrownum<-c()
  if(sum(apply(fresh_nonch,2,function(x){as.character(x)==""}),na.rm = T)>0){stop(message("Stop: The df contains \"\". Check your codes!"))} # all "" should be replaced with NA
  #which(apply(fresh_nonch,2,function(x){as.character(x)==""}),arr.ind = T) #find out "" 
  for (j in 1:length(colnames(fresh_nonch))) { # get the range by col (variable) and then get the rows of out-of-range values
    if(!colnames(fresh_nonch)[j]%in%vm$redcap_var){next()} #skip access var in the current form 
    if(colnames(fresh_nonch)[j]%in%c("scid_s211","scid_xii_c168")){next()} #SPECIAL skip "scid_s211","scid_xii_c168"
    if (grepl("___",colnames(fresh_nonch)[j])){next()} # skip checkbox var brougt in during fixing "value_set2"
    rg<-bsrc.getchoicemapping(variablenames = colnames(fresh_nonch)[j],metadata = curdb$metadata)[[1]] # get the range 
    if(is.null(rg)){log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname, id='OKAY-NO_RANGE',which_var = colnames(fresh_nonch)[j],comments = 'This variable has no range');next()} # variable should have a range 
    if(("na"%in%rg&sum(is.na(as.numeric(rg)))==1)|!any(is.na(as.numeric(rg)))){rg<-as.numeric(rg)} # turn range as numeric if rg contains only "na" and numbers  
    #get the rows of out-of-range values; replace and report out-of-range values
    i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]]))) # report values that is not in the range. NA is acceptable 
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
      if (replace_w_na){
        log_replace<-report_wrong(id=fresh_nonch[i,"IDDATE"],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i,j], which_form = formname,comments = 'STEP3: Out of range values. Replaced with NA',report = log_replace)
        fresh_nonch[i,j]<-NA
      }
      message(paste0('Warn: Some values from ',formname," ", colnames(fresh_nonch)[j], ' are out of range. Refer to log_out_of_range for more details.'))
    }
    #cat(paste(j,"done."))
  }
  fresh_nonch<<-fresh_nonch
  log_out_of_range<<-log_out_of_range
  log_replace<<-log_replace
  deleted_rows<<-deleted_rows
  deletedrownum<<-unique(deletedrownum)
  cat(paste0(formname,": STEP3 done.\n"))
}

for (form_i in 1:length(forms)) {
  STEP1() # get 'raw_nonch': redcap variables, 
  if(!skipotherforms|ifbl){ # skip the form if skipotherforms is T AND ifbl is F
    STEP2();STEP3()
    assign(paste0("form_",formname),unique(fresh_nonch))
    write.csv(unique(fresh_nonch),file = paste0("~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/form_",formname,"_",Sys.Date(),".csv"))
  }} 

# WTAR and RBANS: keep data that is closest to a pt's earliest consent date 
PT_condate<-md$data %>% 
  mutate_all(~replace(.,.=="",NA)) %>% 
  select(1, starts_with("registration_ptcstat___protect"),starts_with("reg_condate_protect")) %>%
  filter_at(vars(starts_with("registration_ptcstat")),any_vars(.>0)) %>% 
  mutate_at(vars(starts_with("reg_condate")),~as.Date(.)) %>% 
  select(1,starts_with("reg_condate")) %>%
  pivot_longer(-1,names_to = "protocol",names_prefix = "reg_condate_",values_to = "condate",values_drop_na = T) %>% 
  group_by(registration_redcapid) %>% slice(which.min(condate)) %>% ungroup()

BS_condate<-md$data %>% 
  mutate_all(~replace(.,.=="",NA)) %>% 
  select(1, registration_ptcstat___bsocial,reg_condate_bsocial) %>%
  filter(registration_ptcstat___bsocial>0) %>% 
  mutate_at(vars(starts_with("reg_condate")),~as.Date(.)) %>% 
  select(1,starts_with("reg_condate")) %>%
  pivot_longer(-1,names_to = "protocol",names_prefix = "reg_condate_",values_to = "condate",values_drop_na = T) %>% 
  group_by(registration_redcapid) %>% slice(which.min(condate)) %>% ungroup()

# for WTAR, split the dataframe by BS pts and PT pts 
form_WTAR_PT<-form_dbo_A_WTAR %>% as.data.frame() %>% bsrc.findid(idmap,"registration_redcapid") %>% 
  inner_join(PT_condate,by=c("masterdemoid"="registration_redcapid")) %>% 
  mutate(wtar_date=as.Date(wtar_date)) %>% 
  group_by(registration_redcapid) %>% slice(which.min(wtar_date-condate)) %>% ungroup()

form_WTAR_BS<-form_dbo_A_WTAR %>% as.data.frame() %>% bsrc.findid(idmap,"registration_redcapid") %>% 
  inner_join(BS_condate,by=c("masterdemoid"="registration_redcapid")) %>% 
  mutate(wtar_date=as.Date(wtar_date)) %>% 
  group_by(registration_redcapid) %>% slice(which.min(wtar_date-condate)) %>% ungroup()

write.csv(form_WTAR_PT,file = paste0("~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/form_WTAR_PT_",Sys.Date(),".csv"))
write.csv(form_WTAR_BS,file = paste0("~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/form_WTAR_BS_",Sys.Date(),".csv"))


write.csv(unique(log_out_of_range),file = paste0("~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/log/log_out_of_range_",Sys.Date(),".csv"))
write.csv(unique(log_replace),file = paste0("~/Documents/github/UPMC/TRANSFER/Neuropsych transfer/log/log_replace",Sys.Date(),".csv"))

#}




#####################################end of the function#########################################



