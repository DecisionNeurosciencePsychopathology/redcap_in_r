## startup
setwd("~/Documents/redcap_in_r/kexin_data_cleaning/")
source('~/Documents/github/UPMC/startup.R')
############startuptransfer#############
rootdir="~/Box/skinner/data/Redcap Transfer/All protect data/"
allsub<-read.csv(paste0(rootdir,"ALL_SUBJECTS_PT.csv"),stringsAsFactors = F)
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice_pt.csv',stringsAsFactors = FALSE) #should be list. you can choose from it is for bsocial or protect
var_map[which(var_map=="",arr.ind = T)]<-NA
#var_map$path<-gsub("\"","",var_map$path) #temperary: remove quotation marks from paths 
var_map$baseline<-"FALSE"#temperary
var_map$baseline<-as.logical(var_map$baseline)#temperary
#var_map<-subset(var_map,!is.na(path)) # temperary remove all rows without paths 
#var_map_ham<-subset(var_map,Form_name=="HRSD and BPRS") # seperate ham from ther var map 
var_map<-subset(var_map,Form_name=="HRSD and BPRS") 

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
t
#check double entries (one redcap variabels exist in multiple forms) across froms  
repeatedrcvar<-unique(na.omit(var_map$redcap_var[duplicated(var_map$redcap_var)]))
dbentry<-subset(var_map,redcap_var%in%repeatedrcvar&!is.na(redcap_var)&!access_var%in%c("ID","MISSCODE","CDATE"),select = c("Form_name","redcap_var","access_var","path"))
dbentry #will check double entry after data cleaning  
#########################################
#STEP1: Select a Access form - baseline(bl) first and then other forms- match with redcap variables, splited ordinary variables with checkbox variables, removed calculated variables 
STEP1<-function(){
  #STEP1.1 Select a RC form. Check if multiple origianl forms need to be combined into one form 
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
    MISSCODE<-RAWDATA$MISSCODE
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
        write.csv(reportdup,file = paste0("~/Documents/github/UPMC/TRANSFER/PT/dup_id/",formname,"_dup_id_rows.csv"))
        RAWDATA<-RAWDATA[-which(RAWDATA$ID%in%dup_id),]}      #remove duplicated rows 
    }else{
      dup_id<-unique(RAWDATA[which(duplicated(RAWDATA$IDDATE)),"IDDATE"])# shoule have no duplicates in IDDATE
      if(length(dup_id)>0){
        warning(paste0("Warn: ",formname," has duplicated IDDATE. Please refer to formname_dup_id_rows.csv. The rows are removed."))
        reportdup<-RAWDATA[which(RAWDATA$IDDATE%in%dup_id),]
        reportdup<-reportdup[order(reportdup$IDDATE),];reportdup[which(is.na(reportdup),arr.ind = T)]<-""
        write.csv(reportdup,file = paste0("~/Documents/github/UPMC/TRANSFER/PT/dup_id/",formname,"_dup_idcdate_rows.csv"))
        RAWDATA<-RAWDATA[-which(RAWDATA$IDDATE%in%dup_id),]}      #remove duplicated rows
    }
    #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx vars to df: 'raw_chk'
    if(!is.null(acvar_chk)){
      raw_nonch<-RAWDATA[,-which(colnames(RAWDATA)%in%acvar_chk)] #keep only non-checkbx variables 
      ifelse("CDATE"%in%colnames(RAWDATA), raw_chk<-RAWDATA[,c("ID","CDATE","IDDATE",acvar_chk)],raw_chk<-RAWDATA[,c("ID","IDDATE",acvar_chk)])
    }else{raw_nonch<-cbind(RAWDATA,MISSCODE)}
    #STEP1.6 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames. make sure there's a column cdate 
    VMAP<-unique(subset(vm,select=c(access_var,redcap_var),is.checkbox=='FALSE'&!is.na(redcap_var)))
    if(any(duplicated(na.omit(VMAP$access_var)))){warning(paste("setp1.6","#",form_i,formname,"Variable mapping... \nWarning: some non-checkbox access variable matches multiple redcap variabels in form",formname))} #check if one ac var matches multiple rc var 
    colnames(raw_nonch)<-plyr::mapvalues(colnames(raw_nonch),from = VMAP$access_var, to = VMAP$redcap_var,warn_missing = F)
    if("CDATE"%in%colnames(RAWDATA)){colnames(raw_nonch)[grep("^CDATE",colnames(raw_nonch))]<-"CDATE"} # rename cdatecopy 
    if(!all(colnames(raw_nonch)%in%c(VMAP$redcap_var,"CDATE","IDDATE","MISSCODE"))){stop(message(paste0(formname," has an error after var mapping when checking: new colnames should contain only CDATE, IDDATE,MISSCODE and redcap variables")))} # check: new colnames should contain only CDATE, IDDATE, and redcap variables 
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
##STEP5 Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
STEP5<-function(){
  cat(paste("#",form_i,formname,"- performning STEP5 now...\n"))
  fresh_nonch<-raw_nonch
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
      if(length(i0>0)){log_replace<-report_wrong(id=fresh_nonch[i0,"IDDATE"],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i0,j], which_form = formname,comments = 'Step5: Out of range values. The rows are removed.',report = log_replace)
      message(paste("col#",j,":",length(i0),"rows are removed from",formname,"because many of them have 0 as an out-of-range value. Row#",fresh_nonch$rownum[i0],"\n"))
      deleted_rows<-rbind(deleted_rows,fresh_nonch[i0,])
      deletedrownum<-append(deletedrownum,fresh_nonch$rownum[i0])
      fresh_nonch<-fresh_nonch[-i0,]
      i<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]])))}
    }
    i9<-which(!((fresh_nonch[[j]] %in% rg) | is.na(fresh_nonch[[j]])) & fresh_nonch[[j]]%in%c(9,2,3,50)) # if the wrong value is 9, replace them with NA;
    if(length(i9)>0){
      log_replace<-report_wrong(id=fresh_nonch[i9,"IDDATE"],which_var = colnames(fresh_nonch)[j], wrong_val = fresh_nonch[i9,j], which_form = formname,comments = 'Step5: Out of range values. Replaced with NA',report = log_replace)
      fresh_nonch[i9,j]<-NA
      i<-setdiff(i,i9)
    }
    if (length(i)==0){
      cat(paste('GOOD. All values of', formname, colnames(fresh_nonch)[j],'are within the range.\n'))
      log_out_of_range<-report_wrong(report = log_out_of_range,which_form = formname,id='GOOD',which_var = colnames(fresh_nonch)[j],comments = 'GOOD. All values are within the range.')
    }else{
      log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[i,1],which_form = formname, which_var = colnames(fresh_nonch)[j],wrong_val = fresh_nonch[i,j],
                                     comments = paste0('Correct range: ', do.call('paste',as.list(rg)),'. Values except 9 are not replaced with NA.'))
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
  cat(paste0(formname,": STEP5 done.\n"))
}

for (form_i in 1:3){
  STEP1();STEP5()
  assign(paste0("form_",form_i),unique(fresh_nonch))
}

#SPECIAL only one IDDATE has both HAM17 and HAM24
intersect(form_1$IDDATE,form_2$IDDATE)
form_1<-subset(form_1,!IDDATE=="2147102015-03-24") #remove "2147102015-03-24" from form_1
write.csv(unique(form_1),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/form_",forms[1],"_",Sys.Date(),".csv"))
write.csv(unique(form_2),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/form_",forms[2],"_",Sys.Date(),".csv"))
write.csv(unique(form_3),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/form_",forms[3],"_",Sys.Date(),".csv"))
