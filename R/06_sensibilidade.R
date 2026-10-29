cluster_bootstrap <- function(a,model,cfg,u=NULL) {
  # Reamostra zonas inteiras dentro de região/candidato; threshold permanece fixo.
  chunks<-split(if(model=="GPD")a$p else a$maximo,a$zona)
  if(length(chunks)<30 || cfg$bootstrap<20)return(NULL)
  rbindlist(lapply(seq_len(cfg$bootstrap),function(b) {
    x<-unlist(chunks[sample.int(length(chunks),length(chunks),replace=TRUE)],use.names=FALSE)
    z<-safe_fit(x,model,u,uncertainty=FALSE)
    if(!valid_fit(z))return(NULL)
    e<-z$fit$estimate
    ep<-if(e["shape"]<0)(if(model=="GPD")u else e["loc"])-e["scale"]/e["shape"] else Inf
    row<-data.table(replica=b,shape=unname(e["shape"]),scale=unname(e["scale"]),endpoint=unname(ep))
    if(model=="GEV")row[,loc:=unname(e["loc"])]
    row
  }),fill=TRUE)
}
sensitivity_analysis <- function(d,blocks,gpd,gev,cfg) {
  sens<-list(); ci<-list(); draws<-list()
  for(model in c("GPD","GEV")) {
    fits<-if(model=="GPD")gpd$fits else gev$fits
    for(key in names(fits)) {
      f<-fits[[key]]; a<-if(model=="GPD")d[REGIAO==f$region & candidato==f$candidate] else blocks[REGIAO==f$region & candidato==f$candidate]
      boot<-cluster_bootstrap(a,model,cfg,if(model=="GPD")f$u else NULL)
      if(!is.null(boot) && nrow(boot)) {
        boot[, `:=`(REGIAO=f$region,candidato=f$candidate,modelo=model)]
        draws[[paste(model,key)]]<-boot
        enough<-nrow(boot)>=.8*cfg$bootstrap
        regular<-unname(f$z$fit$estimate["shape"])>-.5
        row<-data.table(REGIAO=f$region,candidato=f$candidate,modelo=model,n_sucessos=nrow(boot),B=cfg$bootstrap,
                         bootstrap_valido=enough && regular,convergencia_suficiente=enough,regularidade=regular,se=sd(boot$shape))
        for(v in intersect(c("shape","scale","loc"),names(boot))) {
          q<-if(enough && regular)quantile(boot[[v]],c(.025,.975),na.rm=TRUE) else c(NA_real_,NA_real_)
          row[,(paste0(v,"_lo_cluster")):=q[1]];row[,(paste0(v,"_hi_cluster")):=q[2]]
        }
        # Endpoint só recebe IC se todas as réplicas válidas têm xi negativo.
        if(enough && regular && all(is.finite(boot$endpoint))) {
          q<-quantile(boot$endpoint,c(.025,.975));row[,`:=`(endpoint_lo=q[1],endpoint_hi=q[2])]
        } else row[,`:=`(endpoint_lo=NA_real_,endpoint_hi=NA_real_)]
        ci[[paste(model,key)]]<-row
      }
      if(model=="GPD") {
        u0<-f$u; uvec<-sort(unique(gpd$grid[REGIAO==f$region & candidato==f$candidate,u]))
        j<-which.min(abs(uvec-u0)); us<-uvec[unique(pmax(1,pmin(length(uvec),c(j-1,j,j+1))))]
        for(u1 in us) {
          z<-safe_fit(a$p,"GPD",u1); rr<-fit_row(z,f$region,f$candidate,model,nrow(a),u1,sum(a$p>u1));rr[,cenario:="threshold_vizinho"]
          sens[[length(sens)+1]]<-rr
        }
        aa<-a[total_validos>=cfg$small_section]
        if(sum(aa$p>u0)>=cfg$min_exceed){z<-safe_fit(aa$p,"GPD",u0);rr<-fit_row(z,f$region,f$candidate,model,nrow(aa),u0,sum(aa$p>u0));rr[,cenario:="sem_secoes_pequenas"];sens[[length(sens)+1]]<-rr}
      } else {
        aa<-a[n_secoes>=cfg$small_block]
        if(nrow(aa)>=cfg$min_blocks){z<-safe_fit(aa$maximo,"GEV");rr<-fit_row(z,f$region,f$candidate,model,nrow(aa));rr[,cenario:="sem_blocos_pequenos"];sens[[length(sens)+1]]<-rr}
      }
    }
  }
  cis<-rbindlist(ci,fill=TRUE); write_table(cis,"intervalos_cluster")
  write_table(rbindlist(draws,fill=TRUE),"bootstrap_replicas")
  write_table(rbindlist(sens,fill=TRUE),"sensibilidade")
  comp<-rbindlist(list(gpd$table,gev$table),fill=TRUE)
  if(nrow(cis))comp<-merge(comp,cis,by=c("REGIAO","candidato","modelo"),all.x=TRUE)
  write_table(comp,"comparacao_forma")
  if(all(c("shape","shape_lo_cluster","shape_hi_cluster")%in%names(comp)))
    save_plot(ggplot(comp,aes(modelo,shape,colour=modelo))+geom_point(na.rm=TRUE)+
      geom_errorbar(aes(ymin=shape_lo_cluster,ymax=shape_hi_cluster),width=.15,na.rm=TRUE)+
      facet_grid(REGIAO~candidato)+geom_hline(yintercept=0,linetype=2)+labs(y="Xi e IC bootstrap por zona",x=NULL),"comparacao_forma",height=10)
}

missing_zero_scenario <- function(d,cfg) {
  missing <- fread("tables/secoes_incompletas.csv")
  if(!nrow(missing))return(invisible(NULL))
  for(c in cfg$candidates)set(missing,which(is.na(missing[[c]])),c,0)
  missing[,total_validos:=rowSums(.SD),.SDcols=cfg$candidates]
  missing<-merge(missing,unique(d[,.(SG_UF,REGIAO)]),by="SG_UF")
  extra<-melt(missing[total_validos>0],id.vars=c("REGIAO","total_validos"),measure.vars=cfg$candidates,
              variable.name="candidato",value.name="votos")
  extra[,p:=votos/total_validos]
  summarize<-function(a,label)a[,.(cenario=label,n=.N,uns=sum(p==1),zeros=sum(p==0),
    acima_99=sum(p>.99),maximo=max(p)),by=.(REGIAO,candidato)]
  both<-rbindlist(list(d[,.(REGIAO,candidato,p)],extra[,.(REGIAO,candidato=as.character(candidato),p)]))
  write_table(rbindlist(list(summarize(d,"registros_completos"),summarize(both,"HIPOTESE_ausencia_igual_zero"))),"sensibilidade_ausencia_zero")
}
