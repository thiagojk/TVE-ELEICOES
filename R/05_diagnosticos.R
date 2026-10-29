diagnose_models <- function(d, blocks, gpd, gev, cfg) {
  g <- gpd$grid
  for(v in intersect(c("mean_excess","shape","scale_mod","excedencias"),names(g))) {
    fig <- ggplot(g,aes(x=u,y=.data[[v]],colour=candidato)) + geom_line(na.rm=TRUE) +
      geom_point(na.rm=TRUE) + facet_wrap(~REGIAO,scales="free_y") + scale_candidate_colour() + labs(y=v,x="Threshold u")
    if(v %in% c("shape","mean_excess")) {
      se <- if(v=="shape") "shape_se_iid" else "mean_excess_se"
      fig <- fig + geom_errorbar(aes(ymin=.data[[v]]-1.96*.data[[se]],ymax=.data[[v]]+1.96*.data[[se]]),alpha=.25,na.rm=TRUE)
    }
    save_plot(fig,paste0("threshold_",v))
  }
  levels <- list(); quality <- list()
  for(model in c("GPD","GEV")) {
    fits <- if(model=="GPD")gpd$fits else gev$fits
    for(key in names(fits)) {
      f<-fits[[key]]; e<-f$z$fit$estimate
      candidate_colour <- unname(candidate_colours[f$candidate])
      if(model=="GPD") {
        x<-d[REGIAO==f$region & candidato==f$candidate,p]; y<-sort(x[x>f$u]-f$u)
        theo<-evd::qgpd(ppoints(length(y)),scale=e["scale"],shape=e["shape"])
        cdf<-evd::pgpd(y,scale=e["scale"],shape=e["shape"])
        T<-cfg$return_T[cfg$return_T<=length(x) & cfg$return_T*mean(x>f$u)>1]
        ret<-return_gpd(T,f$u,mean(x>f$u),e["scale"],e["shape"])
        endpoint<-if(e["shape"]<0)f$u-e["scale"]/e["shape"] else Inf
        unit<-"seções"
      } else {
        x<-blocks[REGIAO==f$region & candidato==f$candidate,maximo]; y<-sort(x)
        theo<-evd::qgev(ppoints(length(y)),loc=e["loc"],scale=e["scale"],shape=e["shape"])
        cdf<-evd::pgev(y,loc=e["loc"],scale=e["scale"],shape=e["shape"])
        T<-c(10,20,50,100,500); T<-T[T<=length(x)]
        ret<-evd::qgev(1-1/T,loc=e["loc"],scale=e["scale"],shape=e["shape"])
        endpoint<-if(e["shape"]<0)e["loc"]-e["scale"]/e["shape"] else Inf
        unit<-"zonas"
      }
      quality[[paste(model,key)]]<-data.table(REGIAO=f$region,candidato=f$candidate,modelo=model,
        distancia_cdf=max(abs((seq_along(y)-.5)/length(y)-cdf)),
        endpoint=as.numeric(endpoint),endpoint_acima_um=endpoint>1,
        endpoint_abaixo_observado=endpoint<max(x),
        erro_quantil_99=as.numeric(quantile(theo,.99)-quantile(y,.99)))
      prefix<-paste0(tolower(model),"_",gsub("[^a-zA-Z0-9]","_",key))
      diag<-data.table(observado=y,teorico=theo,empirico=ppoints(length(y)),cdf=cdf)
      save_plot(ggplot(diag,aes(teorico,observado))+geom_point(alpha=.3,colour=candidate_colour)+geom_abline(slope=1,intercept=0)+labs(title=key),paste0(prefix,"_qq"))
      save_plot(ggplot(diag,aes(empirico,cdf))+geom_point(alpha=.3,colour=candidate_colour)+geom_abline(slope=1,intercept=0)+labs(title=key),paste0(prefix,"_pp"))
      save_plot(ggplot(diag,aes(observado))+stat_ecdf(aes(linetype="Empírica"),colour=candidate_colour)+geom_line(aes(y=cdf,linetype="Ajustada"),colour=candidate_colour)+
        scale_linetype_manual(values=c("Empírica"="solid","Ajustada"="dashed"),name=NULL)+
        labs(y="Distribuição acumulada",title=key),paste0(prefix,"_cdf"))
      densityfun<-if(model=="GPD")function(v)evd::dgpd(v,scale=e["scale"],shape=e["shape"]) else
        function(v)evd::dgev(v,loc=e["loc"],scale=e["scale"],shape=e["shape"])
      save_plot(ggplot(diag,aes(observado))+geom_histogram(aes(y=after_stat(density)),bins=30,fill=candidate_colour,alpha=.6)+
        stat_function(fun=densityfun,colour=candidate_colour)+labs(title=key,y="Densidade"),paste0(prefix,"_density"))
      rt<-data.table(REGIAO=f$region,candidato=f$candidate,modelo=model,T=T,nivel=ret,unidade=unit,
        endpoint=as.numeric(endpoint),fora_suporte=ret>1 | endpoint<max(x),massa_um=mean(x==1))
      levels[[paste(model,key)]]<-rt
      if(nrow(rt))save_plot(ggplot(rt,aes(T,nivel))+geom_line(colour=candidate_colour)+geom_point(colour=candidate_colour)+scale_x_log10()+
        geom_hline(yintercept=1,linetype=2)+labs(x=paste("Uma ocorrência a cada T",unit),y="Nível estimado",title=key),paste0(prefix,"_retorno"))
    }
  }
  write_table(rbindlist(levels,fill=TRUE),"niveis_retorno")
  write_table(rbindlist(quality,fill=TRUE),"diagnosticos_quantitativos")
}
