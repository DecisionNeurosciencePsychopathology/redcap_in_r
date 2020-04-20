
#Lacking basic info:
all_exit <- readxl::read_xlsx("~/Box/skinner/data/Redcap\ Transfer/All\ protect\ data/neuropsych/PROTECT_EXIT.xlsx")
all_exit_sp <- split(all_exit, all_exit$ID)
proc_neuropsy_s <- function(dfx){
  stoppoint = 6
  vari_index <- list(
    cdate_varis = names(dfx)[1:stoppoint],
    de_varis = names(dfx)[(stoppoint+1):length(dfx)]
  )

  indx_dt<-lapply(vari_index,function(x){
    unique(dfx[x])
  })

  de_uq <- as.Date(indx_dt$de_varis$DE)
  index_df_match<-data.frame(cdate = as.Date(indx_dt$cdate_varis$CDATE),de=as.Date(NA),stringsAsFactors = F)

  for (i in 1:nrow(index_df_match)){
    diff_q <- de_uq - index_df_match$cdate[i]
    if(length(which(diff_q > 0))>0){
      pos_m <- which(diff_q == min(diff_q[diff_q > 0]))
      if(length(pos_m)>1) {
        pos_m <- pos_m[1]
      }
      index_df_match$de[i] <- de_uq[pos_m]
      de_uq <- de_uq[-pos_m]
    }
  }

  if(length(de_uq)>0){
    index_df <- rbind(index_df_match,data.frame(cdate=as.Date(NA),de=as.Date(de_uq),stringsAsFactors = F))
  } else {
    index_df <- index_df_match
  }
  index_df <- index_df[order(apply(index_df,1,min,na.rm=T)),]
  index_df$Matched <- !is.na(index_df$cdate) & !is.na(index_df$de)
  rownames(index_df) <- NULL
  index_df$ID <- unique(dfx$ID)
  if (!any(is.na(apply(index_df,1,min)))) {
    index_df$code = "all matched"
  } else if (!any(!is.na(apply(index_df,1,min)))) {
    index_df$code = "no matched"
  } else if (length(na.omit(index_df$cdate)) > length(na.omit(index_df$de))) {
    index_df$code = "less data"
  } else {
    index_df$code = "more data"
  }

  message("ID: ",unique(indx_dt$cdate_varis$ID)," has status: ",unique(index_df$code))
  #return(index_df)
  if(length(which(index_df$Matched))>0) {
    index_df_sub <- index_df[which(index_df$Matched),]
    output_df<-cbind(indx_dt$cdate_varis[match(index_df_sub$cdate,as.Date(indx_dt$cdate_varis$CDATE)),],
    indx_dt$de_varis[match(index_df_sub$de,as.Date(indx_dt$de_varis$DE)),])

  } else {
    output_df = NULL
  }
  return(list(output_df=output_df,index_df=index_df))
}
output_ls<-lapply(all_exit_sp,proc_neuropsy_s)
all_exit_proc <- do.call(rbind,lapply(output_ls,`[[`,"output_df"))
info_df <- do.call(rbind,lapply(output_ls,`[[`,"index_df"))

if(length(which(info_df$de - info_df$cdate < 0))){
  info_df[which(info_df$de - info_df$cdate < 0),]
  stop("There are matches resulted in DE early than CDATE")
}

message("The following have DE / CDATE difference more than 100 days: ")
info_df[which(info_df$de - info_df$cdate > 100),]
