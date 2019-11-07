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
chckmg<-subset(var_map,select = c('redcap_var','access_var','is.checkbox'),!is.na(is.checkbox))
which(is.na(chckmg),arr.ind = T) # should give us nothing. if yes, try run the following line of code 
var_map$is.checkbox[which(is.na(var_map$redcap_var)&!var_map$is.checkbox)]<-NA
var_map$is.checkbox[which(is.na(var_map$access_var)&!var_map$is.checkbox)]<-NA
sum(is.na(var_map$is.checkbox)) #of unecessary variabels (based on rows. duplicates included)
####remove all blank rows 
#var_map[[8]]<-sapply(var_map[[8]], function(x) gsub("\"", "", x))###TEMP
## TEMP so that NA in 'is.checkbox' means that 

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
#for (form_i in 1:length(forms)) {
for (form_i in 8) {
  #STEP1.1 Select a RC form. Check if multiple origianl forms need to be combined into one form 
  formname <- forms[form_i]
  vm<-subset(var_map, Form_name==formname)
  acvar_or<-with(vm,split(access_var,is.checkbox))$'FALSE' #non-checkbox var
  acvar_chk<-with(vm,split(access_var,is.checkbox))$'TRUE' #checkbox var
  fm_dir<-unique(vm$path)
  if (any(is.na(vm$path))){
    stop(message('At least one row in var mapping does not give the path of directory for the original forms'))
  }else if (length(fm_dir)>1){
    #STEP1.2 If not need to combine multiple forms, jump to STEP1.3. Grab forms, remove uncessary var, combine forms by common cols and check if common cols have same values. 
    df_list<-lapply(fm_dir, function(fm_dir){read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)})
    df_list<-lapply(df_list, function(x){x<-x[,which(colnames(x)%in%c(acvar_or,acvar_chk))]}) #remove unnecessary variables
    
    
    ########write a function to report duplicated ID
    temp_dup_id<-sapply(df_list, function(x){
      any(duplicated(x[[1]]))}) # check duplicated IDs
   ####here 
    
    
    
    
    
    
     comm_var<-Reduce(intersect,lapply(df_list,names)) # get a vector of the names of common cols.
    temp_comm_col_list<-lapply(df_list, function(x){x<-x[comm_var]}) # get the common cols for each form. all dataframes are saved in one list. 
    if(!nlevels(sapply(df_list, nrow))==0){ # nrows of each access form should be the same
      stop(message(paste('For the access forms that needs combining:', formname,'do not have the same number of rows. The forms are stored as "df_list"')))}
    temp_na_in_comm_col<-sum(is.na(unlist(temp_comm_col_list))) # no NAs in common cols.
    if(temp_na_in_comm_col>1){stop(message(paste0('For the access forms that needs combining: ', formname,'. There are ', temp_na_in_comm_col,' NAs in the common columns. The common columns are stored as "temp_comm_col_list".')))}
    for (temp_i in 1:length(fm_dir)) {
      #temp_df<-read.csv(paste0(rootdir,fm_dir[temp_i]), stringsAsFactors = F)
      temp_df<-df_list[[temp_i]]
      duprow<-which(duplicated(temp_df[[1]]))
      if(length(duprow)>0){log_comb_fm<-report_wrong(id=temp_df[duprow,1],which_var = 'ID',which_form = fm_dir[temp_i],report = log_comb_fm,comments = 'Duplicated ID')} # report duplicated ID
      #temp_df<-temp_df[,which(colnames(temp_df)%in%c(acvar_or,acvar_chk))] #remove unnecessary variables
      assign(paste0('temp_df',temp_i),temp_df)
      rm(df_list,temp_comm_col_list,duprow,temp_na_in_comm_col) #keep comm_var
      }
    #comm_var<-Reduce(intersect,lapply((1:length(fm_dir)),function(x){names(get(paste0('temp_df',x)))}))
    for (temp_i in 1:length(fm_dir)) {
      
    }
      
    
    
    
    
  }#STEP1.3 
  else{
    raw <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F) 
    
  }
  
  
eval(parse(text = paste0('rm(names,temp_i,form_i,fm_dir,rcvar',paste0('df',1:3),')')))

  } # remove this 


