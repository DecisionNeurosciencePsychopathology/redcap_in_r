#STEP4<-function(){
  #STEP4.1 range_fix: range in access is not the same as range in redcap, specifies first access variable, then redcap variable to change to
  fixmap<-subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)) 
  if(nrow(fixmap)>0) {for (i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
    valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[i],")"))),ncol = 2,byrow = T)
    if (all(is.na(fresh_nonch[[fixmap$redcap_var[i]]]))){
      message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[i],'" so no need to do "range_fix"'))
    }else{
      fresh_nonch[fixmap$redcap_var[i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[i]]],from = valuemap[,1], to = valuemap[,2])
    }}}
  #STEP4.2 range_allowed: The range in Redcap allows more values than we accept from what should have been the range in redcap. Specifies the new range
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
  #STEP4.3 date: must be converted to date (YYYY-MM-DD)
  #######TEMP - IPDE shoule be combined forms. here i use IPDE_Antisocial_raw.csv as a test form #######
  rootdir="~/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
  fm_dir="IPDE_Antisocial_raw.csv"
  raw <- read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)
  vm<-subset(var_map, path=="IPDE_Antisocial_raw.csv")
  acvar_nonch<-with(vm,split(access_var,is.checkbox))$'FALSE' #non-checkbox var
  acvar_chk<<-with(vm,split(access_var,is.checkbox))$'TRUE' #checkbox var
  #STEP1.4 save chkbx vars to 'raw_nonch' and non-chkbx varsto df: 'raw_chk'
  raw_nonch<-raw[,which(colnames(raw)%in%acvar_nonch)] #keep only non-checkbx variables 
  is.null(acvar_chk)
  #STEP1.5 get 'raw_nonch' for non-chckbx vars: rename AC var using RC varnames
  rcvar_nonch<-with(vm,split(redcap_var,is.checkbox))$'FALSE' #non-checkbox var
  length(acvar_nonch)==length(rcvar_nonch)
  colnames(raw_nonch)<-replace(colnames(raw_nonch),match(acvar_nonch,colnames(raw_nonch)),rcvar_nonch)
  #STEP1.6 remove calculated fields 
  cal_var<-subset(vm,fix_what=='calculated_field')$redcap_var
  length(cal_var)>0
  #######end of temp#########
  fixmap<-subset(vm,fix_what=='date',select = c(redcap_var,instructions))
  if(nrow(fixmap)>0) {for (i in 1:nrow(fixmap)){ # if there's 'date' problem
    message(paste0('Fixing "date" for Form "',formname,'" column "',fixmap$redcap_var[i],'" ... The process may break if the data is weird.'))
    fresh_nonch[fixmap$redcap_var[i]]<-as.Date(fresh_nonch[fixmap$redcap_var[i]][[1]],format = fixmap[i,2])
  }}
  
  
  #STEP 4.4 check_equal: These two values in access should be equal before being imported. Throw an error if they are different
  #fixmap<-subset(vm,fix_what=='check_equal',select = c(redcap_var,instructions))
  #if(nrow(fixmap)>0) {for (i in 1:nrow(fixmap)){ #if there's 'check_equal' problem, fix the problem one variable by one var
  
  #  stop(message(paste0('Fixing "check_equal" for Form "',formname,'" column "',fixmap$redcap_var[i],'". Some values are not equal. Refer to log_out_of_range for details.')))
  #}}
  #fresh_nonch[fixmap]<-as.Date(fresh_nonch$ipde_date,fixmap[1,2])
  
  
  
  #STEP4.5 
  #STEP4.2 unreasonable date
  #STEP4.3 special issues (occur in only one form)
  #sp1var<-subset(vm,fix_what=='special_1',select = redcap_var)[[1]]
  #QOL_fresh[,sp1var]<-as.data.frame(apply(QOL_fresh[,sp1var],2,function(x){gsub('1899-12-30','',x)}))
  
  #STEP4.4 calculated_field= don't transfer this one
  #range_allowed (redcap range is WIDER than Access range)
  
  #fresh_nonch<<-fresh_nonch
#}