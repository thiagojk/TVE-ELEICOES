fit_gev_groups <- function(blocks,cfg) {
  rows <- list(); fits <- list(); groups <- unique(blocks[,.(REGIAO,candidato)])
  for(i in seq_len(nrow(groups))) {
    r<-groups$REGIAO[i]; c<-groups$candidato[i]; b<-blocks[REGIAO==r & candidato==c]
    ratio <- as.numeric(quantile(b$n_secoes,.9)/quantile(b$n_secoes,.1))
    reasons <- c(if(nrow(b)<cfg$min_blocks)"poucos_blocos", if(median(b$n_secoes)<cfg$min_block_median)"blocos_pequenos",
                 if(ratio>cfg$max_block_ratio)"tamanhos_heterogeneos",if(any(b$maximo==1))"massa_no_endpoint")
    if(length(reasons)) {
      rows[[i]] <- data.table(REGIAO=r,candidato=c,modelo="GEV",n=nrow(b),status=paste(reasons,collapse=";")); next
    }
    z<-safe_fit(b$maximo,"GEV"); rows[[i]]<-fit_row(z,r,c,"GEV",nrow(b))
    if(valid_fit(z)) fits[[paste(r,c,sep="|")]]<-list(z=z,region=r,candidate=c)
  }
  out<-rbindlist(rows,fill=TRUE); write_table(out,"gev"); saveRDS(fits,"data/processed/gev_fits.rds")
  list(fits=fits,table=out)
}
