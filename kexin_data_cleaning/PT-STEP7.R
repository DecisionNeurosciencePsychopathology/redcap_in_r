#STEP7<-function(){
fresh_chk<-STEP3(raw_chk) #replace 999 with NA
vm<-subset(vm,is.checkbox=="TRUE") #subset of var_map where is.checkbox = T

#STEP7.1
#####need to check the values of ac var first!
#STEP7.2 redcap checkbox
vm_rcchk<-subset(vm,fix_what=="redcap_check") # subset of vm of redcap_check var
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
    if (is.na(fresh_chk[df_i,acvar])){fresh_chk[df_i,rcvar]<-NA
    }else if (as.numeric(fresh_chk[df_i,acvar])==vm_rcchk$value1[vm_i]){
      fresh_chk[df_i,rcvar]<-vm_rcchk$value2[vm_i]
    }}}
fresh_chk<<-fresh_chk
#}