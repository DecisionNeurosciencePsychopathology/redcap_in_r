#STEP4<-function(){
cat(paste0(formname,": performning STEP4 now...\n"))
#STEP4.01 range_fix: range in access is not the same as range in redcap, specifies first access variable, then redcap variable to change to
fixmap<-unique(subset(vm,fix_what=='range_fix',select = c(redcap_var,instructions)))
if(nrow(fixmap)>0) {for (step4_i in 1:nrow(fixmap)){ # if there's 'range_fix' problem
  valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T)
  if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
    message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
  }else{
    fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = T)
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
      message(paste('Fixing issue "range_allowed" GOOD.:', formname,fixmap$redcap_var[step4_i],'are within the range (NA is allowed).'))
    }else{
      log_out_of_range<-report_wrong(report = log_out_of_range,id=fresh_nonch[row_i,1],which_form = formname, which_var = fixmap$redcap_var[step4_i],wrong_val = thecol[row_i,1],
                                     comments = 'range_allowed')
      fresh_nonch[row_i,fixmap$redcap_var[step4_i]]<-NA
      log_replace<-report_wrong(id=fresh_nonch[row_i,1],which_var = fixmap$redcap_var[row_i], wrong_val = thecol[row_i,1],which_form = formname,comments = 'Fixing "range_allowed": Out of the range.The value is replaced with NA',report = log_replace)
      message('Fixing issue "range_allowed": Some values are out of range. Refer to log_out_of_range for more details. The out-of-range values are replcaed with NA.')
    }}}}
#STEP4.03 special_3: range_fix+range_allowed , 1=1, 2=2, 3=3, 4=5 (5 out of range)
fixmap<-unique(subset(vm,fix_what=='special_3',select = c(redcap_var,instructions)))
if(nrow(fixmap)>0) { #if there's 'special_3' problem
  #range_allowed
  thecol<-fresh_nonch[fixmap$redcap_var[1]] # the col with the problem 
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
      fresh_nonch[row_i,fixmap$redcap_var[step4_i]]<-NA
      log_replace<-report_wrong(id=fresh_nonch[row_i,1],which_var = fixmap$redcap_var[row_i], wrong_val = thecol[row_i,1],which_form = formname,comments = 'Fixing "special_3": Out of the range.The value is replaced with NA',report = log_replace)
      message('Fixing issue "special_3". Some values are out of range. Refer to log_out_of_range for more details. The out-of-range values are replcaed with NA.')
    }}
  #range_fix- copied the codes in step 4.1
  valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))),ncol = 2,byrow = T)
  if (all(is.na(fresh_nonch[[fixmap$redcap_var[step4_i]]]))){
    message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[step4_i],'" so no need to do "range_fix"'))
  }else{
    fresh_nonch[fixmap$redcap_var[step4_i]]<-plyr::mapvalues(fresh_nonch[[fixmap$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = T)
  }}
#STEP4.04 date: must be converted to date (YYYY-MM-DD)
fixmap<-unique(subset(vm,fix_what=='date',select = c(redcap_var,instructions)))
if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
  fresh_nonch[fixmap$redcap_var[step4_i]]<-as.Date(fresh_nonch[fixmap$redcap_var[step4_i]][[1]],format = fixmap$instructions[step4_i])
}}
#STEP 4.05 SPECIAL check_equal: These two values in access should be equal before being imported. Throw an error if they are different
fixmap<-subset(vm,fix_what=='check_equal',select = c(access_var,instructions))
if (nrow(fixmap)>0){ #if there's 'check_equal' problem, fix the problem one variable by one var
  fixmap$instructions<-gsub("=",",",fixmap$instructions)
  reportrow<-which(!fresh_nonch$SPD5==fresh_nonch$STPD8)
  if(length(reportrow)>0){
    log_replace<-report_wrong(id = fresh_nonch[reportrow,1], which_var = "SPD5", wrong_val = fresh_nonch$SPD5[reportrow], which_form = formname, comments = "check_equal: SPD5<>STPD8", report = log_replace) #report
    log_replace<-report_wrong(id = fresh_nonch[reportrow,1], which_var = "STPD8", wrong_val = fresh_nonch$STPD8[reportrow], which_form = formname, comments = "check_equal: SPD5<>STPD8", report = log_replace) #report
    fresh_nonch$SPD5[reportrow]<-NA #replace with NA
    fresh_nonch$STPD8[reportrow]<-NA #replace with NA
    #for (step4_i in 1:nrow(fixmap)){ # generalized codes but not replace with NA
    #  temp_check<-subset(fresh_nonch,select = eval(parse(text = paste0("c(",fixmap$instructions[step4_i],")"))))
    #  if(!all(temp_check[[1]]==temp_check[[2]])){stop(message(paste0(formname,"'s ",fixmap$instructions[step4_i]," are not equal.")))}
    #  rm(temp_check)
    #}
  }}
#STEP4.06 multi_field: One access variable goes into multiple redcap variables
fixmap<-subset(vm,fix_what=='multi_field',select = c(access_var,instructions))
if (nrow(fixmap)>0){for (step4_i in 1:nrow(fixmap)){
  newvar<-gsub(" ","",strsplit(fixmap$instructions[step4_i],",")[[1]]) #new rc var
  newcolnames<-append(colnames(fresh_nonch),newvar) #update the colnames to include the new rc var
  fresh_nonch<-cbind(fresh_nonch,replicate(length(newvar),fresh_nonch[fixmap$access_var[step4_i]])) #duplicate the ac col and then rbind the cols to the original df
  colnames(fresh_nonch)<-newcolnames 
}}
#STEP4.07 special_4: This value goes into multiple redcap values, also value needs to be changed
fixmap<-unique(subset(vm,fix_what=='special_4',select = c(access_var,instructions,value1)))
if(nrow(fixmap)>0) { # if there's 'special_4' problem
  # replace values (range_fix)
  valuemap<-matrix(eval(parse(text = paste0("c(",fixmap$instructions[1],")"))),ncol = 2,byrow = T)
  if (all(is.na(fresh_nonch[[fixmap$access_var[1]]]))){ 
    message(paste0('Form "',formname,'" has only NA in column "',fixmap$redcap_var[1],'" so no need to do value replacement for "special_4"'))
  }else{
    temp_dupcol<-plyr::mapvalues(fresh_nonch[[fixmap$access_var[1]]],from = valuemap[,1], to = valuemap[,2],warn_missing = T)
    # multi_field
    newvar<-gsub(" ","",strsplit(fixmap$value1[1],",")[[1]]) #new rc var
    newcolnames<-append(colnames(fresh_nonch),newvar)
    fresh_nonch<-cbind(fresh_nonch,replicate(length(newvar),temp_dupcol)) #duplicate the ac col and then rbind the cols to the original df
    colnames(fresh_nonch)<-newcolnames #update the colnames to include the new rc var
  }}
#STEP4.08 special_1 needs to be changed to time (HH:SS)
sp1var<-subset(vm,fix_what=='special_1',select = redcap_var)[[1]]
if(length(sp1var)>1){fresh_nonch[,sp1var]<-as.data.frame(apply(fresh_nonch[,sp1var],2,function(x){gsub('1899-12-30','',x)}))}
#STEP4.09 SPECIAL value_set: import this value for EVERYONE who we import this form for
fixmap<-unique(subset(vm,fix_what=='value_set',select = c(redcap_var,instructions)))
if(nrow(fixmap)>0) { # if there's 'value_set' problem
  fresh_nonch$"ipde_excludeitem"<-replicate(nrow(fresh_nonch),1)
}
#STEP4.10 special_5: if demo_childnum=0, give 0, otherwise give 1
fixmap<-unique(subset(vm,fix_what=='special_5',select = c(redcap_var,instructions,value1,value2)))
if(nrow(fixmap)>0) { # if there's 'special_5' problem
  for (df_row in 1:nrow(fresh_nonch)){
    if (is.na(fresh_nonch$demo_childnum[df_row])){fresh_nonch$demo_ynchild[df_row]<-NA
    }else{
      fresh_nonch$demo_ynchild[df_row]<-ifelse(as.numeric(fresh_nonch$demo_childnum[df_row])==0,0,1)
    }}}

fresh_nonch<<-fresh_nonch
log_out_of_range<<-log_out_of_range
log_replace<<-log_replace
message(paste0(formname,": STEP4 done."))
#}