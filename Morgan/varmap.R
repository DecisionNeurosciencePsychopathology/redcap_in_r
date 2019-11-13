#Where to find data
  rootdir="C:/Users/buerkem/Box/skinner/projects_analyses/suicide_trajectories/data/soloff_csv_new/"
#Functions found elsewhere
  startup()
  md<-bsrc.checkdatabase2(ptcs$masterdemo, batch_size=200L)
  idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
  names(idmap)<-c("masterdemoid","wpicid","soloffid")
#New functions
  #Grabs data from bsocial, everything that starts with x, minus the variable for complete
  rd.var.map<-function(x){
    names(bsoc$data[c("registration_redcapid",names(bsoc$data)[which(grepl(x,names(bsoc$data)))])])->bsocnames
    bsocnames[-which(grepl("complete$", bsocnames))]->bsocnames
    return(bsocnames)
  }
#QOL var map  
  QOL_raw <- read.csv(paste0(rootdir,"QOL_raw.csv"))
  #rename the variables to something more reasonable:
  QOL_fresh <- select(QOL_raw, ID, #FOLOQOL, DATEQOL, 
                      TIME.BEGAN, QOLBA1:TIME.ENDED)
  #get redcap names for each form
  bsoc<-bsrc.checkdatabase2(ptcs$bsocial, batch_size=200L)
  #get variables for qol
  rd.var.map("qol")->qolvarmap
  #change variable names to match redcap
  names(QOL_fresh)<-qolvarmap[-c(18:23, 26, 77)]
#OASM var map
oasm <- read.csv(paste0(rootdir,"OAS.M_raw.csv"))
  as.data.frame(names(oasm))->x
  rd.var.map("oasm")->oasmvarmap
  as.data.frame(oasmvarmap)->x

#MMPI var map
  mmpi<-read.csv(paste0(rootdir,"MMPI_raw.csv"))
    as.data.frame(names(mmpi))->x
  rd.var.map("mmpi")->mmpivarmap
  as.data.frame(mmpivarmap)->x

#RFL var map
  rfl<-read.csv(paste0(rootdir,"RFL_raw.csv"))
as.data.frame(names(rfl))->x
  rd.var.map("rfl")->rflvarmap
  as.data.frame(rflvarmap)->x

#CTQ var map
  ctq<-read.csv(paste0(rootdir,"CTQ_raw.csv"))
as.data.frame(names(ctq))->x
  rd.var.map("ctq")->ctqvarmap
  as.data.frame(ctqvarmap)->x

#ALS var map
  als<-read.csv(paste0(rootdir,"ALS.Table_raw.csv"))
as.data.frame(names(als))->x
  rd.var.map("^als")->alsvarmap
  as.data.frame(alsvarmap)->x

#SCID var map
  scid4<-read.csv(paste0(rootdir,"SCID_DSM4_raw.csv"))
  as.data.frame(names(scid4))->x
  rd.var.map("^scid")->scidvarmap
  as.data.frame(scidvarmap)->x

#BL questions var map
  blq1<-read.csv(paste0(rootdir,"Bdemo_PG1_raw.csv"))
  as.data.frame(names(blq1))->x
  blq2<-read.csv(paste0(rootdir,"Bdemo_PG2_raw.csv"))
  as.data.frame(names(blq2))->x
  blq3<-  read.csv(paste0(rootdir,"Bdemo_PG3_raw.csv"))
  as.data.frame(names(blq3))->x
  rd.var.map("demo")->demovarmap
  as.data.frame(demovarmap)->x

#IPDE var map
  ipde4dx<-read.csv(paste0(rootdir,"IPDE_DSM4_raw.csv"))
  as.data.frame(names(ipde4dx))->x
  names(bsoc$data[c("registration_redcapid",
            names(bsoc$data)[which(grepl("ipde_dx",names(bsoc$data)))])])->ipdedxvarmap
  as.data.frame(ipdedxvarmap)->x
  rd.var.map("ipde")->ipdevarmap
  as.data.frame(ipdevarmap)->x
  ipdeas<-read.csv(paste0(rootdir,"IPDE_Antisocial_raw.csv"))
  as.data.frame(names(ipdeas))->x
  ipdeav<-read.csv(paste0(rootdir,"IPDE_Avoidant_raw.csv"))
  as.data.frame(names(ipdeav))->x
  ipdebpd<-read.csv(paste0(rootdir,"IPDE_Borderline_raw.csv"))
  as.data.frame(names(ipdebpd))->x
  ipdedep<-read.csv(paste0(rootdir,"IPDE_Dependent_raw.csv"))
  as.data.frame(names(ipdedep))->x
  ipdehist<-read.csv(paste0(rootdir,"IPDE_Histrionic_raw.csv"))
  as.data.frame(names(ipdehist))->x
  ipdenar<-read.csv(paste0(rootdir,"IPDE_Narcissistic_raw.csv"))
  as.data.frame(names(ipdenar))->x
  ipdeoc<-read.csv(paste0(rootdir,"IPDE_OCPD_raw.csv"))
  as.data.frame(names(ipdeoc))->x
  ipdepar<-read.csv(paste0(rootdir,"IPDE_Paranoid_raw.csv"))
  as.data.frame(names(ipdepar))->x
  ipdeschizoid<-read.csv(paste0(rootdir,"IPDE_Schizoid_raw.csv"))
  as.data.frame(names(ipdeschizoid))->x
  ipdeschtyp<-read.csv(paste0(rootdir,"IPDE_Schizotypal_raw.csv"))
  as.data.frame(names(ipdeschtyp))->x
  