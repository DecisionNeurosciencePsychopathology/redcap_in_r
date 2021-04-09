#Protect Scoring
bsrc.score<-function(df=NULL,formname=NULL,...){
  library(dplyr)
  possible_forms<-c("athf","ham","cirsg","exit","drs","wtar","mmse",
                    "bis","isel","iip","neo","paibor","spsi","ssd","uppsp")
  if(is.null(formname)){
    message("No form name supplied, choose one of these options:")
    print(possible_forms)
    stop()
  }else if(tolower(formname)=="wtar"){
    stop("WTAR is already scored (see wtar_s_adj)")
  }else if(tolower(formname)=="athf"){
    stop("ATHF is already scored (see athf_maxnum)")
  }else if(tolower(formname)=="mmse"){
    stop("MMSE is already scored (see mmse_s_adj)")}
  argu <- list(...)
  argu$df = df
  score_func<-get(paste0("score.",tolower(formname)),envir = loadNamespace("bsrc"))
  return(do.call(score_func,argu))
}
#Clinical
  #SIS scoring
    score.sis<-function(df=NULL){
    #Need to be numeric to score, change to numeric
    df<-df %>% mutate_at(vars(paste0("sis_max_",c(1:18)),paste0("sis_recent_",c(1:18))),as.numeric)
    #Reverse score Q8
    df<-df %>% mutate(
      sis_max_8r=2-sis_max_8, sis_recent_8r=2-sis_recent_8
    )
    #Score
    df<-df %>% mutate(
    sis_max_plan=ifelse(rowSums(is.na(df[paste0("sis_max_",c(1:7,'8r'))]))==0,
                      rowSums(df[paste0("sis_max_",c(1:7,'8r'))]),NA),
    sis_recent_plan=ifelse(rowSums(is.na(df[paste0("sis_recent_",c(1:7,'8r'))]))==0,
                      rowSums(df[paste0("sis_recent_",c(1:7,'8r'))]),NA),
    sis_max_total=ifelse(rowSums(is.na(df[paste0("sis_max_",c(1:7,'8r',9:15))]))==0,
                      rowSums(df[paste0("sis_max_",c(1:7,'8r',9:15))]),ifelse(
                        rowSums(is.na(df[paste0("sis_max_",c(1:7,'8r',9:15))]))==1,
                      round(rowSums(df[paste0("sis_max_",c(1:7,'8r',9:15))],na.rm=T)*15/14),NA)),
    sis_recent_total=ifelse(rowSums(is.na(df[paste0("sis_recent_",c(1:7,'8r',9:15))]))==0,
                      rowSums(df[paste0("sis_recent_",c(1:7,'8r',9:15))]),ifelse(
                        rowSums(is.na(df[paste0("sis_recent_",c(1:7,'8r',9:15))]))==1,
                      round(rowSums(df[paste0("sis_recent_",c(1:7,'8r',9:15))],na.rm=T)*15/14),NA))
    )
    return(df)
  }
  
  #HAM scoring
    score.ham<-function(df=NULL){
    #Change names to ham_1 through ham_24
    paste0("ham_",1:24)->names(df)[names(df) %in% c("ham_1_dm","ham_2_gf","ham_3_su","ham_4_ii","ham_5_im","ham_6_di","ham_7_wi","ham_8_re",
    "ham_9_ag","ham_10_psya","ham_11_soma","ham_12_gi","ham_12_gi","ham_13_gs","ham_14_sex","ham_15_hd","ham_16_li",
    "ham_17_weight","ham_18_rt","ham_19_dp","ham_20_prsx","ham_21_ocsx","ham_22_xhelp","ham_23_xhope","ham_24_xworth")]
    #Need to be numeric to score, change to numeric
    df<-df %>% mutate_at(vars(paste0("ham_",c(1:24))),as.numeric)
    #Score
    df<-df %>% mutate(
    ham_17_sui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:17))]))==0,
                      rowSums(df[paste0("ham_",c(1:17))]),ifelse(
                        rowSums(is.na(df[paste0("ham_",c(1:17))]))==1,
                      round(rowSums(df[paste0("ham_",c(1:17))],na.rm=T)*17/16),NA)),
    ham_17_nosui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:2,4:17))]))==0,
                      rowSums(df[paste0("ham_",c(1:2,4:17))]),ifelse(
                        rowSums(is.na(df[paste0("ham_",c(1:2,4:17))]))==1,
                      round(rowSums(df[paste0("ham_",c(1:2,4:17))],na.rm=T)*16/15),NA)),
    ham_24_sui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:24))]))==0,
                      rowSums(df[paste0("ham_",c(1:24))]),ifelse(
                        rowSums(is.na(df[paste0("ham_",c(1:24))]))==1,
                      round(rowSums(df[paste0("ham_",c(1:24))],na.rm=T)*24/23),ifelse(
                      rowSums(is.na(df[paste0("ham_",c(1:24))]))==2,
                      round(rowSums(df[paste0("ham_",c(1:24))],na.rm=T)*24/22),NA))),
    ham_24_nosui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:2,4:24))]))==0,
                      rowSums(df[paste0("ham_",c(1:2,4:24))]),ifelse(
                        rowSums(is.na(df[paste0("ham_",c(1:2,4:24))]))==1,
                      round(rowSums(df[paste0("ham_",c(1:2,4:24))],na.rm=T)*24/23),ifelse(
                      rowSums(is.na(df[paste0("ham_",c(1:2,4:24))]))==2,
                      round(rowSums(df[paste0("ham_",c(1:2,4:24))],na.rm=T)*24/22),NA)))
    )
    return(df)
  }

  #CIRS-G scoring
    score.cirsg<-function(df=NULL){
    #Need to be numeric to score, change to numeric
    df<-df %>% mutate_at(vars(paste("cirsg_",c(1:13),"_s",sep="")),as.numeric)
    #Score
    df<-df %>% mutate(
    cirs_total=ifelse(rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==0,
                      rowSums(df[paste("cirsg_",c(1:13),"_s",sep="")]),ifelse(
                        rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==1,
                      round(rowSums(df[paste("cirsg_",c(1:13),"_s",sep="")],na.rm=T)*13/12),NA)),
    cirs_3or4=ifelse(rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==0,
                      rowSums(cirs[paste("cirsg_",c(1:13),"_s",sep="")]>2),ifelse(
                        rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==1,
                      round(rowSums(cirs[paste("cirsg_",c(1:13),"_s",sep="")]>2,na.rm=T)*13/12),NA))
    )
    return(df)
  }

#Neuropsych
  #EXIT scoring
  score.exit<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("exit_",c(1:25))),as.numeric)
    df<-df %>% mutate(
    exit_total=ifelse(rowSums(is.na(df[paste0("exit_",c(1:25))]))==0,
                      rowSums(df[paste0("exit_",c(1:25))]),ifelse(
                        rowSums(is.na(df[paste0("exit_",c(1:25))]))==1,
                      round(rowSums(df[paste0("exit_",c(1:25))],na.rm=T)*25/24),ifelse(
                      rowSums(is.na(df[paste0("exit_",c(1:25))]))==2,
                      round(rowSums(df[paste0("exit_",c(1:25))],na.rm=T)*25/23),NA)))
  )
    return(df)
  }
  
  #DRS scoring
  score.drs<-function(df=NULL){
    drs_vars<-paste0("drs_",c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
                                              'p','q','r','s','t','u','v','w','x','y','z','ab',
                                              'ac','ad','ae','af','ag','ah','ai','aj','ak'))
    df<-df %>% mutate_at(vars(drs_vars),as.numeric)
    df<-df %>% mutate(
    drs_attention=ifelse(rowSums(is.na(df[paste0("drs_",c('a','b','c','d'))]))==0,
                         rowSums(df[paste0("drs_",c('a','b','c','d'))]),NA),
    drs_initandpers=ifelse(rowSums(is.na(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))]))==0,
                         rowSums(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))]),ifelse(
                          rowSums(is.na(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))]))==1,
                          round(rowSums(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))],na.rm=T)*11/10),NA)),
    drs_construction=ifelse(rowSums(is.na(df[paste0("drs_",c('p','q','r','s','t','u'))]))==0,
                         rowSums(df[paste0("drs_",c('p','q','r','s','t','u'))]),NA),
    drs_conceptualization=ifelse(rowSums(is.na(df[paste0("drs_",c('v','w','x','y','z','ab'))]))==0,
                         rowSums(df[paste0("drs_",c('v','w','x','y','z','ab'))]),NA),
    drs_memory=ifelse(rowSums(is.na(df[paste0("drs_",c('ac','ad','ae','af','ag','ah','ai','aj','ak'))]))==0,
                         rowSums(df[paste0("drs_",c('ac','ad','ae','af','ag','ah','ai','aj','ak'))]),NA),
    drs_total=ifelse(rowSums(is.na(df))==0, rowSums(df[drs_vars]),ifelse(
      rowSums(is.na(df))==1, round(rowSums(df[drs_vars],na.rm=T)*36/35),ifelse(
        rowSums(is.na(df))==2,round(rowSums(df[drs_vars],na.rm=T)*36/34),ifelse(
          rowSums(is.na(df))==3,round(rowSums(df[drs_vars],na.rm=T)*36/33),NA))))
    )
    return(df)
  }

#Self reports
  #BIS-36 Scoring
  score.bis<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("bis36_",1:36)),as.numeric)
    #Make reverse scores
    df<-df %>% mutate(
    bis36_1r=5-bis36_1, bis36_5r=5-bis36_5,bis36_6r=5-bis36_6,
    bis36_7r=5-bis36_7, bis36_8r=5-bis36_8,bis36_10r=5-bis36_10,
    bis36_11r=5-bis36_11,bis36_13r=5-bis36_13,bis36_19r=5-bis36_19,
    bis36_30r=5-bis36_30,bis36_35r=5-bis36_35
  ) 
  df<-df %>% mutate(
    bis_attention=ifelse(rowSums(is.na(df[paste0("bis36_",c(32,'7r',36,'19r',29))]))==0,
                         rowSums(df[paste0("bis36_",c(32,'7r',36,'19r',29))]),NA),
    bis_cognitive_instability=ifelse(rowSums(is.na(df[paste0("bis36_",c(4,34,27))]))==0,
                         rowSums(df[paste0("bis36_",c(4,34,27))]),NA),
    bis_motor=ifelse(rowSums(is.na(df[paste0("bis36_",c(2,31,3,15,18,21,25))]))==0,
                         rowSums(df[paste0("bis36_",c(2,31,3,15,18,21,25))]),NA),
    bis_perseverance=ifelse(rowSums(is.na(df[paste0("bis36_",c(14, 20, 33, '30r'))]))==0,
                         rowSums(df[paste0("bis36_",c(14, 20, 33,'30r'))]),NA),
    bis_selfcontrol=ifelse(rowSums(is.na(df[paste0("bis36_",c('1r','5r','6r','10r','11r',12))]))==0,
                         rowSums(df[paste0("bis36_",c('1r','5r','6r','10r','11r',12))]),NA),
    bis_cognitive_complexity=ifelse(rowSums(is.na(df[paste0("bis36_",c('8r', '13r', 16, 28, '35r'))]))==0,
                         rowSums(df[paste0("bis36_",c('8r', '13r', 16, 28, '35r'))]),NA),
    bis_s_attentional=ifelse(rowSums(is.na(df[paste0("bis36_",c(4,34,27,32,'7r',36,'19r',29))]))==0,
                         rowSums(df[paste0("bis36_",c(4,34,27,32,'7r',36,'19r',29))]),NA),
    bis_s_motor=ifelse(rowSums(is.na(df[paste0("bis36_",c(2,31,3,15,18,21,25,14, 20, 33, '30r'))]))==0,
                         rowSums(df[paste0("bis36_",c(2,31,3,15,18,21,25,14, 20, 33, '30r'))]),ifelse(
                           rowSums(is.na(df[paste0("bis36_",c(2,31,3,15,18,21,25,14, 20, 33, '30r'))]))==1,
                             round(rowSums(df[paste0("bis36_",c(2,31,3,15,18,21,25,14, 20, 33, '30r'))],na.rm=T)*11/10), NA)),
    bis_s_nonplanning=ifelse(rowSums(is.na(df[paste0("bis36_",c('1r','5r','6r','10r','11r',12,'8r', '13r', 16, 28, '35r'))]))==0,
                         rowSums(df[paste0("bis36_",c('1r','5r','6r','10r','11r',12,'8r', '13r', 16, 28, '35r'))]),ifelse(
                           rowSums(is.na(df[paste0("bis36_",c('1r','5r','6r','10r','11r',12,'8r', '13r', 16, 28, '35r'))]))==1,
                             round(rowSums(df[paste0("bis36_",c('1r','5r','6r','10r','11r',12,'8r', '13r', 16, 28, '35r'))],na.rm=T)*11/10), NA))
  )
    return(df)
  }
  
  #ISEL scoring
  score.isel<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("isel_",1:16)),as.numeric)
  df<-df %>% mutate(
      isel_selfesteem=ifelse(rowSums(is.na(df[paste0("isel_",c(1,10,13,16))]))==0,
                             1+isel_1+isel_10-isel_13+isel_16,NA),
      isel_belonging=ifelse(rowSums(is.na(df[paste0("isel_",c(2:4,9))]))==0,
                            6-isel_2-isel_3+isel_4+isel_9,NA),
      isel_appraisal=ifelse(rowSums(is.na(df[paste0("isel_",c(5,8,12,15))]))==0,
                            6+isel_5-isel_8+isel_12-isel_15,NA),
      isel_tangible=ifelse(rowSums(is.na(df[paste0("isel_",c(6,7,11,14))]))==0,
                            1+isel_6-isel_7+isel_11+isel_14,NA)
    )
  return(df)
  }
  
  #IIP scoring
  score.iip<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("iip15_",1:15)),as.numeric)
    df<-df %>% mutate(
      iip_interpersonal_sensitivity=ifelse(rowSums(is.na(df[paste0("iip15_",c(5,8,10,12,13))]))==0,
                                           rowSums(df[paste0("iip15_",c(5,8,10,12,13))]),NA),
      iip_interpersonal_ambivalence=ifelse(rowSums(is.na(df[paste0("iip15_",c(1:4,6))]))==0,
                                           rowSums(df[paste0("iip15_",c(1:4,6))]),NA),
      iip_agression=ifelse(rowSums(is.na(df[paste0("iip15_",c(7,9,11,14,15))]))==0,
                                           rowSums(df[paste0("iip15_",c(7,9,11,14,15))]),NA)
    )
    return(df)
  }
  
  #NEO scoring
  score.neo<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("neoffi_",1:60)),as.numeric)
    df<-df %>% mutate(
      neoffi_1r=6-neoffi_1,neoffi_3r=6-neoffi_3,neoffi_8r=6-neoffi_8, neoffi_9r=6-neoffi_9,
      neoffi_12r=6-neoffi_12, neoffi_14r=6-neoffi_14, neoffi_15r=6-neoffi_15,
      neoffi_16r=6-neoffi_16, neoffi_18r=6-neoffi_18, neoffi_23r=6-neoffi_23,
      neoffi_24r=6-neoffi_24, neoffi_27r=6-neoffi_27, neoffi_29r=6-neoffi_29,
      neoffi_30r=6-neoffi_30, neoffi_31r=6-neoffi_31, neoffi_38r=6-neoffi_38,
      neoffi_39r=6-neoffi_39, neoffi_42r=6-neoffi_42, neoffi_44r=6-neoffi_44,
      neoffi_45r=6-neoffi_45, neoffi_46r=6-neoffi_46, neoffi_48r=6-neoffi_48,
      neoffi_54r=6-neoffi_54, neoffi_55r=6-neoffi_55, neoffi_57r=6-neoffi_57,
      neoffi_59r=6-neoffi_59) #Changed all scoring to 6 minus
    #Scoring
    df<-df %>% mutate(
      neo_neuroticism=ifelse(rowSums(is.na(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))]))==0,
                             rowSums(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))]),ifelse(
                               rowSums(is.na(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))]))==1,
                              round(rowSums(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))],na.rm=T)*12/11),NA)),
      neo_extraversion=ifelse(rowSums(is.na(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))]))==0,
                             rowSums(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))]),ifelse(
                               rowSums(is.na(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))]))==1,
                              round(rowSums(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))],na.rm=T)*12/11),NA)),
      neo_openness=ifelse(rowSums(is.na(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))]))==0,
                             rowSums(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))]),ifelse(
                               rowSums(is.na(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))]))==1,
                              round(rowSums(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))],na.rm=T)*10/9),NA)),
      neo_agreeableness=ifelse(rowSums(is.na(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))]))==0,
                             rowSums(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))]),ifelse(
                               rowSums(is.na(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))]))==1,
                              round(rowSums(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))],na.rm=T)*12/11),NA)),
      neo_conscientiousness=ifelse(rowSums(is.na(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))]))==0,
                             rowSums(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))]),ifelse(
                               rowSums(is.na(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))]))==1,
                              round(rowSums(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))],na.rm=T)*12/11),NA))
    )
    return(df)
  }
  
  #PAIBOR scoring
  score.paibor<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("paibor_",1:24)),as.numeric)
    #Reverse scoring
    df<-df %>% mutate(
      paibor_7r=3-paibor_7, paibor_12r=3-paibor_12, paibor_14r=3-paibor_14,
      paibor_19r=3-paibor_19, paibor_20r=3-paibor_20, paibor_24r=3-paibor_24
    )
    df<-df %>% mutate(
      paibor_identity_problems=ifelse(rowSums(is.na(df[paste0("paibor_",c(2, 5, 8, 11, 15, '19r'))]))==0,
                                      rowSums(df[paste0("paibor_",c(2, 5, 8, 11, 15, '19r'))]),NA),
      paibor_neg_relation=ifelse(rowSums(is.na(df[paste0("paibor_",c(3, 6, 9, '12r', 16, '20r'))]))==0,
                                      rowSums(df[paste0("paibor_",c(3, 6, 9, '12r', 16, '20r'))]),NA),
      paibor_affective_instability=ifelse(rowSums(is.na(df[paste0("paibor_",c(1,4,'7r',10,'14r',18))]))==0,
                                        rowSums(df[paste0("paibor_",c(1,4,'7r',10,'14r',18))]),NA),
      paibor_selfharm=ifelse(rowSums(is.na(df[paste0("paibor_",c(13,17,21,22,23,'24r'))]))==0,
                                        rowSums(df[paste0("paibor_",c(13,17,21,22,23,'24r'))]),NA),
      paibor_total=ifelse(rowSums(is.na(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]))==0,
                                        rowSums(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]),ifelse(
                                        rowSums(is.na(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]))==1,
                                        round(rowSums(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))],na.rm=T)*24/23),ifelse(
                                        rowSums(is.na(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]))==2,
                                        round(rowSums(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))],na.rm=T)*24/22),NA)))
    )
    return(df)
  }
  
  #PB scoring
  score.pb<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("pb_",c('6a','6b','6c','6d','6e','6f'))),as.numeric)
    df<-df %>% mutate(
      pb_6fr=(2-pb_6f)
      )
  
    df<-df %>% mutate(
      pb_total=ifelse(rowSums(is.na(df[c("pb_6a","pb_6b","pb_6c","pb_6d","pb_6e","pb_6f")]))==0,
                  rowSums(df[c("pb_6a","pb_6b","pb_6c","pb_6d","pb_6e","pb_6fr")]),NA)
      )
  return(df)
  }
  
  #SPSI scoring
  score.spsi<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("spsi",1:25),as.numeric))  
    df<-df %>% mutate(
    #Subscores (positive and rational are positive, negative, impulsecare and avoid are negative)
    spsi_pos_problemorient=ifelse(rowSums(is.na(SPSI[paste0("spsi_",c(4,5,9,13,15))]))==0,
                                      rowSums(SPSI[paste0("spsi_",c(4,5,9,13,15))]),NA),
    spsi_neg_problemorient=ifelse(rowSums(is.na(SPSI[paste0("spsi_",c(1,3,7,8,11))]))==0,
                                      rowSums(SPSI[paste0("spsi_",c(1,3,7,8,11))]),NA),
    spsi_rational_problemsolve=ifelse(rowSums(is.na(SPSI[paste0("spsi_",c(16,19,12,21,23))]))==0,
                                      rowSums(SPSI[paste0("spsi_",c(16,19,12,21,23))]),NA),
    spsi_impulsecareless=ifelse(rowSums(is.na(SPSI[paste0("spsi_",c(2,14,20,24,25))]))==0,
                                rowSums(SPSI[paste0("spsi_",c(2,14,20,24,25))]),NA),
    spsi_avoidance=ifelse(rowSums(is.na(SPSI[paste0("spsi_",c(10,18,6,17,22))]))==0,
                                rowSums(SPSI[paste0("spsi_",c(10,18,6,17,22))]),NA),
    #Reverse items in negative, impulsecare, and avoidance for total calculation
    spsi_1r=4-spsi_1, spsi_3r=4-spsi_3, spsi_7r=4-spsi_7, spsi_8r=4-spsi_8, spsi_11r=4-spsi_11,
    spsi_2r=4-spsi_2, spsi_14r=4-spsi_14, spsi_20r=4-spsi_20, spsi_24r=4-spsi_24, spsi_25r=4-spsi_25,
    spsi_10r=4-spsi_10, spsi_18r=4-spsi_18, spsi_6r=4-spsi_6, spsi_17r=4-spsi_17, spsi_22r=4-spsi_17)
    
    #Total
    df<-df %>% mutate(
    spsi_total=ifelse(rowSums(is.na(SPSI[paste0("spsi_",c(1:25))]))==0,
          rowSums(SPSI[paste0("spsi_",c(4,5,9,13,15,16,19,12,21,23,'1r','3r','7r','8r','11r','2r','14r','20r','24r','25r','10r','18r','6r','17r','22r'))]),ifelse(
            rowSums(is.na(SPSI[paste0("spsi_",c(1:25))]))==1,round(rowSums(SPSI[paste0("spsi_",c(4,5,9,13,15,16,19,12,21,23,'1r','3r','7r','8r','11r','2r','14r','20r','24r','25r','10r','18r','6r','17r','22r'))],na.rm=T)*25/24),ifelse(
                rowSums(is.na(SPSI[paste0("spsi_",c(1:25))]))==2,round(rowSums(SPSI[paste0("spsi_",c(4,5,9,13,15,16,19,12,21,23,'1r','3r','7r','8r','11r','2r','14r','20r','24r','25r','10r','18r','6r','17r','22r'))],na.rm=T)*25/23),NA)))
  )
    return(df)
  }
  
  #SSD Scoring
  score.ssd<-function(df=NULL){
  #Recode variables (ND means for network diversity, ppl= Number of people)
    df<-df %>% mutate_at(vars(paste0("ssd_",c(1:12,'2a','3a','4a','5a','6a','7a','8a','10a','12a','12b'))),as.numeric)
    df<-df %>% mutate(
      #network diversity
      ssd_1nd=ifelse(ssd_1==1,1,0),ssd_2nd=ifelse(ssd_2a>0,1,0),
      ssd_3nd=ifelse(ssd_3a>0,1,0),ssd_4nd=ifelse(ssd_4a>0,1,0),
      ssd_5nd=ifelse(ssd_5a>0,1,0),ssd_6nd=ifelse(ssd_6a>0,1,0),
      ssd_7nd=ifelse(ssd_7a>0,1,0),ssd_8nd=ifelse(ssd_8a>0,1,0),
      ssd_12nd=ifelse(ssd_12a>0 | ssd_12b>0,1,0),ssd_9nd=ifelse(ssd_9>0,1,0),
      ssd_10nd=ifelse(ssd_10a>0,1,0),ssd_11nd=ifelse(ssd_11==1,1,0),
      #number of people in network
      ssd_1ppl=ifelse(ssd_1==1,1,0), ssd_2ppl=ssd_2a, ssd_3ppl=dplyr::recode(ssd_3a,'0'=0,'1'=1,'2'=1,'3'=2),
      ssd_4ppl=dplyr::recode(ssd_4a,'0'=0,'1'=1,'2'=1,'3'=2,'9'=0), ssd_5ppl=ssd_5a,
      ssd_6ppl=ssd_6a, ssd_7ppl=ssd_7a, ssd_8ppl=ssd_8a, ssd_12ppl=ssd_12a+ssd_12b,
      ssd_9ppl=ssd_9, ssd_10ppl=ssd_10,
      #embedded networks
      ssd_1en=ifelse(ssd_1==1,1,0), ssd_2en=ifelse(ssd_2a>0,1,0),
      ssd_3en=ifelse(ssd_3a>0,1,0), ssd_4en=ifelse(ssd_4a>0 & ssd_4a!=9,1,0),
      ssd_5en=ifelse(ssd_5a>0,1,0))
    #Embedded Networks family score
    df<-df %>% mutate(
      ssd_famen1=rowSums(df[c("ssd_1en", "ssd_2en", "ssd_3en", "ssd_4en", "ssd_5en")],na.rm=T),
      ssd_famen2=rowSums(df[c("ssd_1ppl","ssd_2ppl", "ssd_3ppl", "ssd_4ppl", "ssd_5ppl")],na.rm=T))
    #Score
    df<-df %>% mutate(
      ssd_network_diversity=rowSums(df[names(df) %in% paste("ssd_",c(1:12),"nd",sep="")],na.rm=T),
      ssd_num_of_people=rowSums(df[names(df) %in% paste("ssd_",c(1:10,12),"ppl",sep="")],na.rm=T)
    )
    for (i in 1:nrow(df)){
       df[i,"ssd_embedded_network"]<-
         ifelse(!is.na(df[i,"ssd_famen1"]) & df[i,"ssd_famen1"]>2 & !is.na(df[i,"ssd_famen2"]) & df[i,"ssd_famen2"]>3,1,0)+
         ifelse(!is.na(df[i,"ssd_6a"]) & df[i,"ssd_6a"]>3,1,0)+
         ifelse(!is.na(df[i, "ssd_7a"])& df[i, "ssd_7a"]>3,1,0)+
         ifelse(!is.na(df[i, "ssd_8a"])& df[i, "ssd_8a"]>3,1,0)+
         ifelse(!is.na(df[i, "ssd_12a"])& !is.na(df[i, "ssd_12b"]) & df[i, "ssd_12a"]+df[i, "ssd_12b"]>3,1,0)+
         ifelse(!is.na(df[i, "ssd_9"]) & df[i, "ssd_9"]>3,1,0)+
         ifelse(!is.na(df[i, "ssd_10a"])& df[i, "ssd_10a"]>3,1,0)
      }
    return(df)
  }
  
  #UPPSP scoring
  score.uppsp<-function(df=NULL){
    df<-df %>% mutate_at(vars(paste0("uppsp_",1:47)),as.numeric)
    #Reverse Scoring
    df<-df %>% mutate(
      uppsp_2r=5-uppsp_2, uppsp_6r=5-uppsp_6, uppsp_10r=5-uppsp_10,
      uppsp_14r=5-uppsp_14, uppsp_18r=5-uppsp_18, uppsp_23r=5-uppsp_23,
      uppsp_27r=5-uppsp_27, uppsp_31r=5-uppsp_31, uppsp_35r=5-uppsp_35,
      uppsp_40r=5-uppsp_40, uppsp_46r=5-uppsp_46, uppsp_7r=5-uppsp_7,
      uppsp_37r=5-uppsp_37, uppsp_4r=5-uppsp_4, uppsp_8r=5-uppsp_8,
      uppsp_12r=5-uppsp_12, uppsp_16r=5-uppsp_16, uppsp_20r=5-uppsp_20,
      uppsp_24r=5-uppsp_24, uppsp_28r=5-uppsp_28, uppsp_32r=5-uppsp_32,
      uppsp_36r=5-uppsp_36, uppsp_39r=5-uppsp_39,uppsp_41r=5-uppsp_41, 
      uppsp_43r=5-uppsp_43, uppsp_45r=5-uppsp_45, uppsp_47r=5-uppsp_47
    )
    df<-df %>% mutate(
      uppsp_neg_urgency=ifelse(rowSums(is.na(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                              '23r','27r','31r','35r','40r',42,'46r'))]))==0,
                                      rowSums(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                              '23r','27r','31r','35r','40r',42,'46r'))]),ifelse(
                                      rowSums(is.na(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                              '23r','27r','31r','35r','40r',42,'46r'))]))==1,
                                      round(rowSums(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                              '23r','27r','31r','35r','40r',42,'46r'))],na.rm=T)*12/11),NA)),
      uppsp_lackof_premed=ifelse(rowSums(is.na(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))]))==0,
                                      rowSums(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))]),ifelse(
                                      rowSums(is.na(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))]))==1,
                                      round(rowSums(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))],
                                                    na.rm=T)*11/10),NA)),
      uppsp_lackof_perserve=ifelse(rowSums(is.na(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))]))==0,
                                      rowSums(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))]),ifelse(
                                      rowSums(is.na(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))]))==1,
                                      round(rowSums(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))],
                                                    na.rm=T)*10/9),NA)),
      uppsp_pos_urgency=ifelse(rowSums(is.na(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                                  '36r','39r','41r','43r','45r','47r'))]))==0,
                                      rowSums(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                                  '36r','39r','41r','43r','45r','47r'))]),ifelse(
                                      rowSums(is.na(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                                  '36r','39r','41r','43r','45r','47r'))]))==1,
                                      round(rowSums(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                                  '36r','39r','41r','43r','45r','47r'))],
                                                    na.rm=T)*14/13),NA))
      
      )
    return(df)
  }