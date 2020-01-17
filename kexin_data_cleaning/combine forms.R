#################################### SAME #################################### 
## startup
rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
source('~/Documents/github/UPMC/startup.R')
var_map<-read.csv('~/Box/skinner/data/Redcap Transfer/variable map/kexin_practice.csv',stringsAsFactors = FALSE)
var_map[which(var_map=="",arr.ind = T)]<-NA
var_map[[8]]<-sapply(var_map[[8]], function(x) gsub("\"", "", x))###TEMP

#Initialize reports 
log_comb_fm <- data.frame(id=as.character(),var_name=as.character(),wrong_val=as.character(),
                          which_form=as.character(),comments=as.character(),stringsAsFactors = F) # Report issues during combining forms 

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

#STEP1: Select a form. Match variable names, checkbox variables considered.??? 
#for (form_i in 1:length(forms)) {
for (form_i in 8) {
  formname <- forms[form_i]
  vm_col <- which(var_map$Form_name==formname)
  fm_dir<-unique(var_map$path[vm_col])
  # import data from access and match variables  # TO BE GENERALIZED 
  # IF if multiple forms are transformed into one form in redcap, combine them by ID and check if they have the same ID  
  if (sum(is.na(var_map$path[vm_col]))>0){
    stop(message('At least one row in var mapping does not give the path of directory for the original forms'))
  }else if (length(fm_dir)>1){
    names<-paste0('df',c(1:length(fm_dir)))
    tempdf<-lapply(fm_dir,funtion(x){read.csv(paste0(rootdir,x), stringsAsFactors = F)})[[1]]
  }else{
    raw <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F) 
  }
rm(names)
  } # remove this 


