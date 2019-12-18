#STEP7<-function(){
cat(paste("#",form_i,formname,"- performning STEP7 now...\n"))
fresh_chk<-raw_chk
vm<-subset(vm,is.checkbox=="TRUE") #subset of var_map where is.checkbox = T

#STEP7.1
#####need to check the values of ac var first!????
#STEP7.2 redcap checkbox
vm_rcchk<-subset(vm,fix_what=="redcap_check") # subset of vm of redcap_check var
if (nrow(vm_rcchk)>0){
  #STEP7.2.1 cbind the original df with an empty dataframe containing rc col
  newcolname<-append(colnames(fresh_chk),unique(vm_rcchk$redcap_var))#get the colname for the new df
  fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_rcchk$redcap_var))))) 
  colnames(fresh_chk)<-newcolname
  #STEP7.2.2 fill in redcap cols
  #for each row of fresh_chk, if values of acvar == x1 then values of rcvar == y1
  for (df_i in 1:nrow(fresh_chk)) { # for every observation, [swtich values from access forms to coresponding values in redcap]  
    for (vm_i in 1:nrow(vm_rcchk)){ #for every row in var_map (i.e. for every pair of [accessvalue,redcapvalue]), replace access value with redcap value
      acvar<-vm_rcchk$access_var[vm_i]
      rcvar<-vm_rcchk$redcap_var[vm_i]
      if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
        iftrue<-as.numeric(fresh_chk[df_i,acvar])==vm_rcchk$value1[vm_i]
        fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_rcchk$value2[vm_i],NA)
      }}}
  cat("redcap_check done.")
}
#STEP7.3 access checkbox
vm_achk<-subset(vm,fix_what=="access_check") # subset of vm of redcap_check var
if (nrow(vm_achk)>0){
  #STEP7.3.1 cbind the original df with an empty dataframe containing rc col
  newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
  fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
  colnames(fresh_chk)<-newcolname
  #STEP7.3.2 fill in redcap cols
  #for each row of fresh_chk, if values of acvar contains x1 then values of rcvar == x2, otherwise rcvar == x3
  for (df_i in 1:nrow(fresh_chk)){
    for (vm_i in 1:nrow(vm_achk)){
      acvar<-vm_achk$access_var[vm_i]
      rcvar<-vm_achk$redcap_var[vm_i]
      if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
        iftrue<-grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
        fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],vm_achk$value3[vm_i])
      }}}
  cat("access_check done.")
}
#STEP7.4 both_check1 both access and redcap are checkboxes, not case sensitive, if not in value1, then value 2 
vm_achk<-subset(vm,fix_what=="both_check1") # subset of vm of redcap_check var
if (nrow(vm_achk)>0){
  #STEP7.4.1 cbind the original df with an empty dataframe containing rc col
  newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
  fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
  colnames(fresh_chk)<-newcolname
  #STEP7.4.2 fill in redcap cols
  #for each row of fresh_chk, if not contain value1, then value 2 
  for (df_i in 1:nrow(fresh_chk)){
    for (vm_i in 1:nrow(vm_achk)){
      acvar<-vm_achk$access_var[vm_i]
      rcvar<-vm_achk$redcap_var[vm_i]
      if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
        iftrue<-!grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
        fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],NA)
      }}}
  cat("both_check done.")
}
#STEP7.5 both_check2 both access and redcap are checkboxes, not case sensitive, if in value1, then value 2 
vm_achk<-subset(vm,fix_what=="both_check2") # subset of vm of redcap_check var
if (nrow(vm_achk)>0){
  #STEP7.5.1 cbind the original df with an empty dataframe containing rc col
  newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
  fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
  colnames(fresh_chk)<-newcolname
  #STEP7.5.2 fill in redcap cols
  #for each row of fresh_chk, if contain value1, then value 2 
  for (df_i in 1:nrow(fresh_chk)){
    for (vm_i in 1:nrow(vm_achk)){
      acvar<-vm_achk$access_var[vm_i]
      rcvar<-vm_achk$redcap_var[vm_i]
      if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
        iftrue<-grepl(vm_achk$value1[vm_i],fresh_chk[df_i,acvar],ignore.case = T)
        fresh_chk[df_i,rcvar]<-ifelse(iftrue,vm_achk$value2[vm_i],NA)
      }}}
  cat("both_check done.")
}
#STEP7.6 condition if value1=value2, match here, otherwise assign value 3
vm_achk<-subset(vm,fix_what=="condition") # subset of vm of redcap_check var
if (nrow(vm_achk)>0){
  #STEP7.6.1 cbind the original df with an empty dataframe containing rc col
  newcolname<-append(colnames(fresh_chk),unique(vm_achk$redcap_var))#get the colname for the new df
  fresh_chk<-cbind(fresh_chk,data.frame(matrix(NA, nrow = nrow(fresh_chk), ncol = length(unique(vm_achk$redcap_var))))) 
  colnames(fresh_chk)<-newcolname
  #STEP7.6.2 fill in redcap cols
  #for each row of fresh_chk, if value1=value2, match here, otherwise assign value 3
  for (df_i in 1:nrow(fresh_chk)){
    for (vm_i in 1:nrow(vm_achk)){
      #for (vm_i in 1:12){
      acvar0<-vm_achk$value1[vm_i]
      acvar<-vm_achk$access_var[vm_i]
      rcvar<-vm_achk$redcap_var[vm_i]
      if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA} else {
        iftrue<-fresh_chk[df_i,acvar0]==vm_achk$value2[vm_i]
        fresh_chk[df_i,rcvar]<-ifelse(iftrue,fresh_chk[df_i,acvar],vm_achk$value3[vm_i])
      }}}
  cat("condition done.")
}
#STEP7.7 SPECIAL special_6 range fix and make copies of this variable
vm_achk<-subset(vm,fix_what=="special_6") # subset of vm of redcap_check var
if (nrow(vm_achk)>0){
  #STEP7.7.1 cbind the original df with an empty dataframe containing rc col
  newcolname<-append(colnames(fresh_chk),vm_achk$redcap_var)#get the colname for the new df
  fresh_chk<-cbind(fresh_chk,fresh_chk$EATYP,fresh_chk$EATYP,fresh_chk$EATYP) 
  colnames(fresh_chk)<-newcolname
  #STEP7.7.1 range fix 
  for (step4_i in 1:nrow(vm_achk)){
    valuemap<-matrix(eval(parse(text = paste0("c(",vm_achk$instructions[step4_i],")"))),ncol = 2,byrow = T)
    fresh_chk[vm_achk$redcap_var[step4_i]]<-plyr::mapvalues(fresh_chk[[vm_achk$redcap_var[step4_i]]],from = valuemap[,1], to = valuemap[,2],warn_missing = F)
  }
}

fresh_chk<<-fresh_chk
cat(paste("\n#",form_i,formname,"- STEP7 done.\n"))
#}