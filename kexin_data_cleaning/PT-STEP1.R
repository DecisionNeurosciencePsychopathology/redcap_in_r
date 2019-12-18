#rm(list = ls())
#################################### SAME #################################### 
## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
#source('~/Documents/github/UPMC/startup.R')
rootdir="~/Box/skinner/data/Redcap Transfer/All protect data/"
allsub<-read.csv(paste0(rootdir,"ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt2.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
var_map$baseline<-"TRUE" #temperary
var_map$baseline<-as.logical(var_map$baseline)
colnames(var_map)[grep("path",colnames(var_map))]<-"path" #temperary
var_map_ham<-subset(var_map,Form_name=="HRSD and BPRS") # seperate ham from ther var map 
var_map<-subset(var_map,!Form_name=="HRSD and BPRS") # var map w/o form HRSD and BPRS
forms<-with(var_map,gsub(".csv","",unique(na.omit(path)))) #TEMPERARY
combine<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/combing forms.csv',stringsAsFactors = FALSE)
combine[which(combine=="",arr.ind = T)]<-NA
## verify Morgan's var_map. 
####for the col is.box. NA should mean represent unecessary variables. i.e. 
# if redcap_var and access_var both exist, is.checkbox cannot be NA
chckmg<-subset(var_map,select = c('redcap_var','access_var'),is.na(is.checkbox))
chckmg[which(!is.na(chckmg$redcap_var)&(!is.na(chckmg$access_var))),] #shoule give us nothing
#var_map<-var_map[-as.numeric(rownames(chckmg[which(!is.na(chckmg$redcap_var)&(!is.na(chckmg$access_var))),])),] #temperary
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

remove_dupid = FALSE # if T, remove all rows that involve duplicated IDs  
skipotherforms = TRUE
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

#################################### SAME #################################### 

#STEP1: Select a RC form, get an integrated RC form with complete variables, right variable names, splited ordinary variables with checkbox variables. 
for (form_i in 1:length(forms)) {
  #  STEP1<-function(){
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
      special<-read.csv(paste0(rootdir,"deleted_duplicated_id/",formname,"_special_dup_id.csv"),stringsAsFactors = F)
      special<-subset(special,ifkeep=="TRUE",select = 1:(ncol(special)-1))[-1]
      special[which(special=="",arr.ind = T)]<-NA
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
  
  
  
  
  #  }
}

#} # remove this 


