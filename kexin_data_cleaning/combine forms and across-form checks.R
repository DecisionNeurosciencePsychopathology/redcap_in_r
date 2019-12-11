
if (formname=="HRSD and BPRS"){ 
  cb<-subset(combine,Form_name==formname)
  if(!identical(cb$path,unique(vm$path))){stop(message("combining form.csv has some problems."))}
  comb_fm_list<-lapply(fm_dir, function(fm_dir){read.csv(paste0(rootdir,fm_dir), stringsAsFactors = F)});names(comb_fm_list)<-cb$path # grab forms 
  comb_fm_list<-lapply(comb_fm_list,function(x){x<-with(x,x[which(ID%in%allsub$ID),])}) #remove people not in our study
  #comb_fm_list<-lapply(comb_fm_list, function(x){x[,-which(colnames(x)=='X')]}) # remove col 'X'
  # check var_mapping: access variables in Access forms and var map shoule be the same  
  w_acvar<-paste(sapply(comb_fm_list,function(x){paste(setdiff(colnames(x),vm$access_var),collapse = ", ")}),collapse = " ") #all access variesbles should be in var map
  if(any(grepl("[a-zA-Z]",w_acvar))){message(paste("Warning:",w_acvar,"cannot be found in the var_map."))} # report ^
  w_rcvar<-na.omit(setdiff(vm$access_var,unlist(sapply(comb_fm_list,function(x){colnames(x)})))) # all access_var in var mapping should be in actual Access forms
  if(any(grepl("[a-zA-Z]",w_rcvar))){stop(message(paste("Stop:",paste(w_rcvar,collapse = ", "),"in the var_map does not match any variables in the forms.")))} # report^
  confm<-readline(prompt = paste0("Enter T to confirm that the CDATE",paste(sapply(comb_fm_list,function(x){x[1,"CDATE"]}),collapse = ",")," follows '%m/%d/%y':")) #confirm the format of CDATE
  if(as.logical(confm)){comb_fm_list<-lapply(comb_fm_list, function(x){
    x<-x[,which(colnames(x)%in%c(acvar_nonch,acvar_chk))]
    x[which(x=="",arr.ind = T)]<-NA
    x["CDATE"]<-as.Date(x[["CDATE"]],format = "%m/%d/%y");x})} #remove unnecessary variables, replace "" with NA, change dtype of CDATE
  #STEP1.2.1 No NAs in ID or CDATE. Add IDCDTE col. Report or remove duplicated ID+CDATE.
  if(any(is.na(unlist(sapply(comb_fm_list,function(x){c(x[["ID"]],x[["CDATE"]])}))))){stop(message(paste0("Stop: NA exists in ID or CDATE in the form ",formname)))}
  comb_fm_list<-lapply(comb_fm_list,function(x){x<-data.frame(x,IDDATE=paste0(x[["ID"]],x[["CDATE"]]),stringsAsFactors = F)}) # add col of "IDDATE"
  temp_dup_id<-as.vector(unlist(sapply(comb_fm_list, function(x){x[which(duplicated(x[["IDDATE"]])),"IDDATE"]}))) # get duplicated IDDATE 
  if (length(temp_dup_id)>0){
    if (!as.logical(remove_dupid)){ # report duplicated ID
      log_comb_fm<-report_wrong(id=temp_dup_id,which_var = 'IDDATE',report = log_comb_fm,which_form = formname,comments = 'Duplicated IDDATE. Note: it\'s possible that they are duplicated in each form.')
      log_comb_fm<-unique(log_comb_fm)
      message('Duplicated IDs exist. Refer to log_comb_fm for more info. Forms are stored as comb_fm_list.
      Viewing details of duplicated ID...')}
    temp_chck_dupid<-lapply(comb_fm_list,function(x){x[which(x[["IDDATE"]]%in%temp_dup_id),]}); # Viewing details of duplicated IDDATE
    for (chk_i in 1:length(temp_chck_dupid)) {View(temp_chck_dupid[[chk_i]])} #Viewing details of duplicated IDDATE
    remove_dupid<-readline(prompt = 'Enter T to remove duplciated ID; F to just report: ') # to remove duplicated ID based on date 
    if(as.logical(remove_dupid)){
      message("Duplicated IDDATE Not removed.")
      remove_dupid<-F # foreced to report dup ids for the next form 
    }}
  #STEP1.2.2check IDDATE following Morgan's .csv  
  comm_var<-Reduce(intersect,lapply(comb_fm_list,names)) # get a vector of the names of common cols.
  w_comm_var<-setdiff(comm_var,c("ID","CDATE","IDDATE","MISSCODE")) # identify common var other than ID, DATE, MISSCODE
  if (length(w_comm_var)>0){stop(message(paste0("Common variables other than ID, DATE, MISSCODE: ",paste(w_comm_var,collapse = ", "))))} # my code assumes the common cols are only ID, CDATE, MISSCODE
  for (comb_i in 1:length(cb$path)){comb_fm_list[[comb_i]]<-data.frame(comb_fm_list[[comb_i]],formpath=cb$path[comb_i])} #add col "formpath" to each ac form
  # Check IDDATE following Morgan's instructions 
  if(all(grepl("=",cb$instructions))){
    commonid<-Reduce(intersect,sapply(comb_fm_list,function(x){x[["IDDATE"]]})) # get IDDATE that exist in all forms 
    if(length(commonid)==0){stop(message("Something is wrong. commonid should contain at least something."))
    }else{
      new_combrows<-lapply(comb_fm_list,function(x){subset(x,!IDDATE%in%commonid)});names(new_combrows)<-cb$path # get rows with IDDATE that does not exist in every form
      comb_rows<-append(comb_rows,new_combrows)
      newlog<-do.call("rbind",lapply(new_combrows,function(x){
        report_wrong(id=x[["IDDATE"]],which_var = x[["formpath"]],wrong_val = length(cb$path),which_form = formname,comments = "observation absent from at least one from",report = log_comb_fm,rbind = F)}))
      log_comb_fm2<-rbind(log_comb_fm2,unique(newlog));rm(new_combrows)
    }
  }else{stop(message("write codes to check IDDATE when combining forms!"))}
  comb_fm_list<-lapply(comb_fm_list,function(x){subset(x,IDDATE%in%commonid)}) #TEMPERARLY remove the wired rows 
  rawdata<-Reduce(f=function(x,y){dplyr::full_join(x,y,by=c("ID","CDATE","IDDATE"))},comb_fm_list)
  if("MISSCODE" %in% comm_var){colnames(rawdata)[grep("MISSCODE",colnames(rawdata))]<-paste0("MISSCODE",cb$path)}
  rawdata<-rawdata[,-grep("formpath",colnames(rawdata))] #remove formpath
}

write.csv(unique(log_comb_fm2),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/",formname,"log_wrong_combingforms.csv"))
for (del_i in 1:length(comb_rows)){
  write.csv(comb_rows[[del_i]],file = paste0("~/Documents/github/UPMC/TRANSFER/PT/",formname,"_Wrong_RowstobeCombined_",names(comb_rows)[del_i]))
  rm(del_i)
}

#Morgan check: combining forms 
morgan<-tidyr::pivot_wider(log_comb_fm2[,1:3],values_from = wrong_val,names_from = var_name)
morgan[which(is.na(morgan),arr.ind = T)]<-""
write.csv(unique(morgan),file = paste0("~/Documents/github/UPMC/TRANSFER/PT/",formname,"wrong_combingforms.csv"))
