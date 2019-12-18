fresh_nonch<-raw_nonch #temperary
#STEP4<-function(){
fresh_nonch<-raw_nonch
cat(paste("#",form_i,formname,"- performning STEP4 now...\n"))
#STEP4.01 range_fix: range in access is not the same as range in redcap, specifies first access variable, then redcap variable to change to
fixmap<-unique(subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)))
if(nrow(fixmap)>0) {for (step4_i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
  valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T) # don't use str split because we want NA rather than "NA"
  if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
    message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
  }else{
    fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = F)
  }}}
#STEP4.02 range_allowed: The range in Redcap allows more values than we accept from what should have been the range in redcap. Specifies the new range
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
      log_out_of_range<-report_wrong(report = log_out_of_range,id=ifelse(ifbl,fresh_nonch[row_i,1],fresh_nonch[row_i,"IDDATE"]),which_form = formname, which_var = fixmap$redcap_var[step4_i],wrong_val = thecol[row_i,1],
                                     comments = 'range_allowed')
      if(replace_w_na){ #Replace out-of-range values with NA 
        fresh_nonch[row_i,fixmap$redcap_var[step4_i]]<-NA
        log_replace<-report_wrong(id=ifelse(ifbl,fresh_nonch[row_i,1],fresh_nonch[row_i,"IDDATE"]),which_var = fixmap$redcap_var[row_i], wrong_val = thecol[row_i,1],which_form = formname,comments = 'Fixing "range_allowed": Out of the range.The value is replaced with NA',report = log_replace)
        message('Fixing issue "range_allowed": Some values are out of range. Refer to log_out_of_range for more details. The out-of-range values are replcaed with NA.')}
    }}}}
#STEP4.03 date: must be converted to date (YYYY-MM-DD)
fixmap<-unique(subset(vm,fix_what=='date',select = c(redcap_var,instructions)))
if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
  fresh_nonch[fixmap$redcap_var[step4_i]]<-as.Date(as.character(fresh_nonch[[fixmap$redcap_var[step4_i]]]),format = "%m/%d/%y")
  if(!all(na.omit(as.integer((fresh_nonch[[fixmap$redcap_var[step4_i]]]-Sys.Date())/365))%in%seq(-100,100))){stop(message("Something went wrong when changing the format of date"))} # check that the new date is in reasonable range: +-100years of today
}}
#STEP4.04 value_set: import this value for EVERYONE who we import this form for
fixmap<-unique(subset(vm,fix_what=='value_set',select = c(redcap_var,instructions)))
if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
  fresh_nonch<-cbind(fresh_nonch,newcolumn=fixmap$instructions[step4_i])
  colnames(fresh_nonch)<-gsub("^newcolumn",fixmap$redcap_var[step4_i],colnames(fresh_nonch))
}}
if(nrow(fixmap)>0) { # if there's 'value_set' problem
  fresh_nonch$"ipde_excludeitem"<-replicate(nrow(fresh_nonch),1)
}
#STEP4.05 value_set2: if value 1 is not null, value 2, otherwise, value 3
fixmap<-unique(subset(vm,fix_what=='value_set2',select = c(redcap_var,instructions,value1,value2,value3)))
if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
  fresh_nonch<-cbind(fresh_nonch,newcolumn=as.integer(fixmap[step4_i,c("value2","value3")[as.integer(is.na(fresh_nonch[[fixmap$value1[step4_i]]]))+1]]))
  colnames(fresh_nonch)<-gsub("newcolumn",fixmap$redcap_var[step4_i],colnames(fresh_nonch))
}}

fresh_nonch<<-fresh_nonch
log_out_of_range<<-log_out_of_range
log_replace<<-log_replace
cat(paste0(formname,": STEP4 done.\n"))
#}

