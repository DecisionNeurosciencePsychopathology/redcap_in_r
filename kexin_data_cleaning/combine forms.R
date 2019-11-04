#################################### SAME #################################### 
## startup
rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
source('~/Documents/github/UPMC/startup.R')
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice.csv',stringsAsFactors = FALSE)
var_map[which(var_map=="",arr.ind = T)]<-NA

#note: here uses 'QOL interview' as the 'training form'. 

#Initialize reports 
log_out_of_range <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                               which_form=as.character(),comments=as.character(),stringsAsFactors = F) #Report out-of-range values 
log_replace <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report wrong values/datatypes, correct and report 

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

replace_w_na = FALSE
#) {

## verify Morgan's var_map. 

# PREPARE variable: forms
all_formnm<-with(var_map,unique(Form_name[!is.na(Form_name)])) #get all redcap formnames  
if (is.null(forms)){
  forms<-all_formnm
} else {  
  # check if form names can be found in variable mapping   
  if (!is.vector(forms)){stop(message('`forms` must be a vector. Use "c("example1","example2")" or "example".'))}
  if (sum(forms %in% all_formnm)>1) {
    stop(message('One of the formnames cannot be found in the variable mapping. Please note that form names are case sensitive and space sensitive.'))
  }
  # removed duplicates and NA from `forms`
  forms<-unique(forms[!is.na(forms)])
} 

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
#?Grabs data from bsocial, everything that starts with x, minus the variable for complete
rd.var.map<-function(x){
  bsocnames<-c('id',names(bsoc$data[c(which(grepl(x,names(bsoc$data))))]))
  bsocnames[-which(grepl("complete$", bsocnames))]->bsocnames
  return(bsocnames)
}
#?Gives value of 1 if not in range # TO BE CHANGED - MAKE A REPORT INSTEAD OF A COL 
qol.range<-function(range, tar_cols){for (i in 1:nrow(QOL_fresh)){
  if (any(sapply(QOL_fresh[i, tar_cols], function(x){
    !x %in% range & !is.na(x)
  }))){
    QOL_fresh$probs4[i]<-1} # TO BE GENERALIZED
  else{QOL_fresh$probs4[i]<-0}} # TO BE GENERALIZED
  return(QOL_fresh)}
#?if not in range, changes the values to NA
qol.na<-function(range, tar_cols,df=QOL_fresh){for (i in 1:nrow(QOL_fresh)){
  QOL_fresh[i, tar_cols]<-
    sapply(QOL_fresh[i, tar_cols], function(x){
      ifelse (!x %in% range & !is.na(x) ,x<-NA,x<-x)})}
  return(QOL_fresh)}

#################################### SAME #################################### 

#STEP1: Select a form. Match variable names, checkbox variables considered.??? 
for (form_i in 1:length(forms)) {
  formname<-forms[form_i]
  
  # if multiple forms are transformed into one form in redcap, combine them by ID and check if they have the same ID  
  