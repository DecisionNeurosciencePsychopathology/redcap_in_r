########Try to Process EPRIME Data:
##;_; Good Luck

##!!! Need to take the EPrime Frame Number because it  makes no sense hahaahahahhahahaahahahahahahahah ;_;

library(pracma)
library(rprime)
library(tidyr)
library(stringdist)
#Pick the location of the file 

bsrc.eprimeread<-function(fpath,getvar=NULL){
if (missing(fpath)){
  fpath<-file.choose()}

if (is.null(getvar)){stop("No varible input")}
if (is.list(getvar)){stop("Not a list, go away")}
  
#Read the data; most of the heavy part are done by someone else..,
test1<-rprime::read_eprime(fpath)
test2<-rprime::extract_chunks(test1) #chuck it
tdata<-FrameList(test1) #tdata now is a list
preview_levels(tdata) 
onlytwo<-keep_levels(tdata,2)
testdf<-to_data_frame(onlytwo)

###################ABOVE IS GOOD######
################
#######Let's try setting up a reference df
dfname.s<-strsplit(names(testdf),".",fixed = T)
dfname.g<-dfname.s[which(sapply(dfname.s,function(x) {length(x)})>1)]
dfname.go<-names(testdf)[which(sapply(dfname.s,function(x) {length(x)})>1)]
dfname.t<-sapply(dfname.g,'[[',2)
dfname.tu<-unique(sapply(dfname.g,'[[',2))
dfname.o<-sapply(dfname.g,'[[',1)
dfname.ou<-unique(sapply(dfname.g,'[[',1))

getvar.s<-strsplit(getvar,".",fixed = T)
getvar.ns<-unlist(getvar.s[which(sapply(getvar.s,function(x) {length(x)})<=1)])
getvar.so<-getvar.s[which(sapply(getvar.s,function(x) {length(x)})>1)]
getvar.o<-sapply(getvar.so,'[[',1)
getvar.ou<-unique(sapply(getvar.so,'[[',1))
getvar.t<-sapply(getvar.so,'[[',2)
getvar.tu<-unique(sapply(getvar.so,'[[',2))

matchds<-data.frame(sapply(getvar.so,'[[',1),sapply(getvar.so,'[[',2))
names(matchds)<-c("first","second")
matchkx<-NULL
for (i in 1:length(getvar.ou)) {
  print(i)
  matchkx.tmp<-data.frame(dfname.ou[agrep(getvar.ou[i],dfname.ou)])
  matchkx.tmp$first<-getvar.ou[i]
  names(matchkx.tmp)<-c("dfvari","first")
  if (i==1) {matchkx.tmp->matchkx}else {
  matchkx<-merge(matchkx,matchkx.tmp,all=T)}  
}
matchfx<-merge(matchkx,matchds,all = T)
matchfx$type<-sub(paste(paste("(.*?)",matchfx$first,"(.*?)",sep = ""),collapse = "|"), "\\1", matchfx$dfvari)
matchfx$type[which(matchfx$type=="")]<-matchfx$first[which(matchfx$type=="")]
matchfx$dfname<-paste(matchfx$dfvari,matchfx$second,sep = ".")
matchfx$newname<-paste(matchfx$first,matchfx$second,sep = ".")

#Set for final df
always<-c("Eprime.FrameNumber")
getvar.ns.x<-c(getvar.ns,always)
tdf.pre<-testdf[,c(na.omit(match(matchfx$dfname,names(testdf))),na.omit(match(getvar.ns.x,names(testdf))))]
varylist<-names(tdf.pre)[names(tdf.pre) %in% matchfx$dfname]

tdf.proc<-tdf.pre %>% 
  gather(key = "temp", value = "value",varylist) %>%
  separate(temp, into = c("dfvari", "second"), sep = "\\.")

matchfx.xxc<-unique(subset(matchfx,select = c("dfvari","first","type")))
tdf.prz<-merge(tdf.proc,matchfx.xxc,all.x=T)
tdf.prz$dfvari<-NULL
#Give filtering an argument
if (TRUE){
tdf.prz->tdf.prz.backup
tdf.prz.f<-tdf.prz[which(!is.na(tdf.prz$value)),]
tdf.prz.f->tdf.prz
}
tdf.prz$newname<-paste(tdf.prz$first,tdf.prz$second,sep = ".")
tdf.finale<-reshape(tdf.prz,idvar = "Eprime.FrameNumber",timevar = "newname",direction = "wide", v.names = c("value"), drop = c("first","second"))
names(tdf.finale)<-sub("value\\.(.*?)", "\\1", names(tdf.finale))
#I think I did it;_;yay?
return(list(data=tdf.finale,varimatch=matchfx))
}



#
####agrep() is a good solution, to form a set of variables to get; 




##########Bad
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
sapply(lix,'[[',2)
sapply(lix,'[[',2) %in% ftnewvar)
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
testdf2[,grep(".RT",names(testdf2))][unlist(apply(testdf2[,grep(".RT",names(testdf2))],1, function(x) {x[which(!is.na(x))}]))]
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
as.character(rt)]=
as.character(rt)

