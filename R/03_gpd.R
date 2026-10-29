threshold_grid <- function(x, cfg) {
  us <- unique(as.numeric(quantile(x, cfg$threshold_probs)))
  rbindlist(lapply(us, function(u) {
    exc <- x[x > u] - u
    z <- if(length(exc) >= cfg$min_exceed && length(unique(exc)) >= 10) safe_fit(x,"GPD",u) else
      list(fit = structure(list(error="Excedências insuficientes ou muito discretas"),class="fit_error"), warnings="")
    row <- fit_row(z,"","","GPD",length(x),u,length(exc))
    row[, `:=`(mean_excess = if(length(exc))mean(exc) else NA_real_,
               mean_excess_se = if(length(exc)>1)sd(exc)/sqrt(length(exc)) else NA_real_)]
    if(valid_fit(z)) row[, scale_mod := scale - shape*u]
    row
  }), fill=TRUE)
}
select_threshold <- function(g) {
  # Primeiro trecho de três limiares com xi e escala modificada estáveis.
  # Regra exploratória pré-especificada; os gráficos devem ser revisados.
  if(!all(c("shape","shape_se_iid","scale_mod") %in% names(g))) return(NA_real_)
  for(i in seq_len(max(0,nrow(g)-2))) {
    w <- g[i:(i+2)]
    if(any(!is.finite(w$shape)) || any(w$status != "ajuste_exploratorio") || any(!is.finite(w$shape_se_iid))) next
    overlap <- max(w$shape_lo_iid) <= min(w$shape_hi_iid)
    stable <- diff(range(w$scale_mod)) <= .2*max(abs(w$scale_mod),1e-6)
    if(overlap && stable) return(w$u[1])
  }
  NA_real_
}
fit_gpd_groups <- function(d,cfg) {
  groups <- unique(d[,.(REGIAO,candidato)])
  fits <- list(); rows <- list(); grids <- list()
  for(i in seq_len(nrow(groups))) {
    r <- groups$REGIAO[i]; c <- groups$candidato[i]; x <- d[REGIAO==r & candidato==c,p]
    g <- threshold_grid(x,cfg); g[, `:=`(REGIAO=r,candidato=c)]
    # Usar variável externa sem ambiguidade com a coluna u.
    chosen <- select_threshold(g); g[, escolhido := if(is.na(chosen)) FALSE else u==chosen]
    grids[[i]] <- g
    if(is.na(chosen)) {
      rows[[i]] <- data.table(REGIAO=r,candidato=c,modelo="GPD",status="sem_threshold_estavel"); next
    }
    z <- safe_fit(x,"GPD",chosen)
    row <- fit_row(z,r,c,"GPD",length(x),chosen,sum(x>chosen))
    row[, massa_um := mean(x==1)]
    if(any(x==1)) row[, status := "massa_no_endpoint_requer_modelo_discreto"]
    if(valid_fit(z)) fits[[paste(r,c,sep="|")]] <- list(z=z,u=chosen,region=r,candidate=c)
    rows[[i]] <- row
  }
  grid <- rbindlist(grids,fill=TRUE); out <- rbindlist(rows,fill=TRUE)
  write_table(grid,"threshold_grid"); write_table(out,"gpd")
  saveRDS(fits,"data/processed/gpd_fits.rds")
  list(fits=fits,table=out,grid=grid)
}
