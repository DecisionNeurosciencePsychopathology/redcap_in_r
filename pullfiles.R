#Snake Pull:
sdpath<-"~/Box/skinner/data/Snake game"
masterdemo<-bsrc.conredcap2(protocol = ptcs$masterdemo,batch_size = 1000L,output = T)
protect<-bsrc.checkdatabase2(protocol = ptcs$protect,batch_size = 1000L,output = T)
IDPath<-list.files(sdpath,full.names = T,recursive = F,include.dirs = F)
gra<-do.call(rbind,lapply(IDPath,function(IDPathx){
  ftnote<-NA;
  IDx<-tools::file_path_sans_ext(basename(IDPathx))
  if(grepl("_",IDx)){IDy<-strsplit(IDx,"_")[[1]];IDx<-IDy[1];ftnote<-IDy[2]}
  ExMas<-IDx %in% masterdemo$data$registration_redcapid
  nocandi<-F
  possible_candidate<-list(NA)
  if(!ExMas){
    possible_candidate<-list(masterdemo$data$registration_redcapid[agrep(IDx,masterdemo$data$registration_redcapid)])
    if(length(possible_candidate)<1){nocandi<-T}
  } 
  asx<-data.frame(ID=IDx,ftnote,ExMaster=ExMas,nocandi,path=IDPathx,stringsAsFactors = F)
  asx$possible_candidate<-possible_candidate
  return(asx)
}))

toinvest<-gra[!gra$ExMaster,]; existsMs<-gra[gra$ExMaster,]
print(toinvest) 
print("By figuring out the correct ID pairing, additional clinical data will be loaded;")
#Get the dataset:
demo_sub<-masterdemo$data[which(masterdemo$data$registration_redcapid %in% existsMs$ID),]
existsMs$behavDate<-as.Date(file.info(existsMs$path)$mtime)

clinical_datalist<-lapply(c("bpni","ipipds","ffni","ham_and_bprs"),bsrc.getform,protocol = ptcs$protect) 
names(clinical_datalist)<-c("bpni","ipipds","ffni","ham_and_bprs")

clinical_datalist<-lapply(clinical_datalist,function(xa){xa[xa$registration_redcapid %in% existsMs$ID,]})

# 
# lapply(clinical_datalist,function(xl){
#   xla<-xl[which(xl$registration_redcapid %in% existsMs$ID),]
#   if(any(duplicated(xla$registration_redcapid))){
#     xla_sp<-split(xla,xla$registration_redcapid)
#     
#   } else {return(list(data=xla,nodataIDs=existsMs$ID[which(!existsMs$ID %in% xl$registration_redcapid)]))}
#   
# })
hab_sp<-split(clinical_datalist$ham_and_bprs,clinical_datalist$ham_and_bprs$registration_redcapid)
clinical_datalist$ham_and_bprs<-do.call(rbind,lapply(hab_sp,function(xra){
  if(any(existsMs$ID %in% unique(xra$registration_redcapid))){
    behavDate<-as.Date(existsMs$behavDate[which(existsMs$ID %in% unique(xra$registration_redcapid))])
    return(xra[which.min(abs(as.Date(xra$ham_date) - behavDate)),])
  } else {
    NULL
  }
}))




save(demo_sub,clinical_datalist,file = "snake_pull_demo_ham_bpni_ipip_ffni.rdata")
