############################
###### Project RETRO #######
############################

###In this project, Jiazhou attempt to replicate functions used to process data in Mathlab to R 
##This project essentially uselss but to provide pratice for Jiazhou to farmilar with the functions
#Unless somehow it actually got finished where the data is available 


library(pracma)
library(rprime)
library(tidyr)
library(stringdist)
library(R.matlab)
#Pick the location of the file 



dnpl.whereisfile<-function(path){
  #Formalate the plan to get the data:
  if(missing(path)){stop("Please include path that contains folders of each participants which consist their data")}
  
  
  getvar.bandit<-c()
  singlebandit<-dnpl.eprimeread()
  
}




##########Bad
if (FALSE) {
getvar<-c("showstim.RESP","showstim.RT","showstim.ACC","Procedure")
newvar<-strsplit(getvar,".",fixed = T)
spiltvar<-newvar[which(sapply(newvar,function(x) {length(x)})>1)]
nonspiltvar<-unlist(newvar[which(sapply(newvar,function(x) {length(x)})<=1)])
onevar<-unique(sapply(spiltvar,'[[',1))
twovar<-unique(sapply(spiltvar,'[[',2))

#Get variable name:
onefilter<-names(testdf)[grep(onevar,names(testdf))]
onefilter.s<-strsplit(onefilter,".",fixed = T)
onefilter.p<-sapply(onefilter.s,'[[',2)
onefilter.x<-onefilter[which(onefilter.x %in% twovar)]

testdf2<-testdf[,c(onefilter.x,"Procedure","Eprime.FrameNumber")]

# We are stuck here because we have no idea what/how to rename the variable so they don't ....
for (i in 1:length(unique(testdf2$Procedure))){
  targ<-unique(testdf2$Procedure)[i]
  tardf<-testdf2[which(testdf2$Procedure==targ),]
  tardf <- tardf[,colSums(is.na(tardf))<nrow(tardf)]
  
  if (i==1) {enddf<-tardf}
  
  merge(tardf,enddf,all = T)
}


#######Let's try to just reshape the data; let's not, cuz it ....
#for loop each:
testdf->testdf.backup
testdf.x<-testdf
for (i in 1:length(dfname.tu)) {
  vartoga<-dfname.go[which(dfname.t==dfname.tu[i])]
  toeval<-paste("testdf.x<-gather(testdf.x, Firstvar,", dfname.tu[i],", vartoga)")
  eval(parse(text = toeval))
}

reshape(testdf.x,varying = dfname.go, timevar = "Firstorder",v.names = dfname.tu , times = dfname.ou, direction = "long")


# That creates problems for getting it later, so nah
testdf.y<-testdf.x %>% 
  gather(key = "First", value = "value",dfname.go) %>%
  separate(First, into = c("procname", "valuetype"), sep = "\\.")

testdf.y$procname[agrep(paste(dfname.ou,collapse = "|"),testdf.y$procname)]

lix<-strsplit(find[grep(paste(findt, collapse = "|"),find,fixed = F)],split = ".",fixed = T)
sapply(lix,'[[',2) %in% ftnewvar
findt[which(sapply(lix,'[[',2) %in% ftnewvar)]
find2[which(sapply(lix,'[[',2) %in% ftnewvar)]
find[which(sapply(lix,'[[',2) %in% ftnewvar)]
find[grep(paste(findt, collapse = "|"),find,fixed = F)][which(sapply(lix,'[[',2) %in% ftnewvar)]
c(find[grep(paste(findt, collapse = "|"),find,fixed = F)][which(sapply(lix,'[[',2) %in% ftnewvar)],"Procedure")
testdf[,c(find[grep(paste(findt, collapse = "|"),find,fixed = F)][which(sapply(lix,'[[',2) %in% ftnewvar)],"Procedure")]
testdf2<-testdf[,c(find[grep(paste(findt, collapse = "|"),find,fixed = F)][which(sapply(lix,'[[',2) %in% ftnewvar)],"Procedure")]
View(testdf2)
testdf2$Procedure
names(testdf2)
grep(".RT",names(testdf2))
testdf2[,grep(".RT",names(testdf2))]
apply(testdf2[,grep(".RT",names(testdf2))],1, function(x) {which(!is.na(x))})
unlist(apply(testdf2[,grep(".RT",names(testdf2))],1, function(x) {which(!is.na(x))}))
testdf2[,grep(".RT",names(testdf2))][unlist(apply(testdf2[,grep(".RT",names(testdf2))],1, function(x) {which(!is.na(x))}))]
apply(testdf2[,grep(".RT",names(testdf2))],1, function(x) {x[which(!is.na(x))]})

rt<-apply(testdf2[,grep(".RT",names(testdf2))],1, function(x) {x[which(!is.na(x))]})
acc<-apply(testdf2[,grep(".ACC",names(testdf2))],1, function(x) {x[which(!is.na(x))]})
resp<-apply(testdf2[,grep(".RESP",names(testdf2))],1, function(x) {x[which(!is.na(x))]})

as.array()
?as.array
as.data.frame(rt,acc,resp)
data.frame(rt,acc,resp)
sapply(rt, ']]',1)
sapply(rt, '[[',1)
sapply(rt, '[[',0)
unlist(rt)
as.character(unlist(rt))
as.character(rt)
}