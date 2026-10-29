describe_data <- function(d, cfg) {
  stats <- function(p) as.list(c(n = length(p), media = mean(p), dp = sd(p), minimo = min(p),
    setNames(quantile(p, c(.25,.5,.75,.8,.9,.95,.975,.99)), c("Q1","mediana","Q3","P80","P90","P95","P975","P99")), maximo = max(p)))
  write_table(d[, stats(p), by = .(REGIAO, candidato)], "descritiva")
  write_table(d[, stats(total_validos), by = .(REGIAO, candidato)], "tamanho_secoes")
  save_plot(ggplot(d, aes(p, fill = candidato)) + geom_histogram(binwidth = .02, position = "identity", alpha = .7) + scale_candidate_fill() +
              labs(x = "Proporção de votos válidos", y = "Seções"), "histograma_nacional")
  save_plot(ggplot(d, aes(p, fill = candidato)) + geom_histogram(binwidth = .02) + scale_candidate_fill() + facet_grid(REGIAO ~ candidato) +
              labs(x = "Proporção", y = "Seções"), "histogramas_regionais", height = 10)
  save_plot(ggplot(d, aes(p, colour = REGIAO)) + geom_density(bounds = c(0,1)) + facet_wrap(~candidato) +
              labs(x = "Proporção", y = "Densidade descritiva"), "densidades")
  save_plot(ggplot(d, aes(REGIAO, p, fill = candidato)) + geom_boxplot(outlier.shape = NA) + scale_candidate_fill() +
              labs(x = NULL, y = "Proporção"), "boxplots")
  save_plot(ggplot(unique(d, by = c("SG_UF","CD_MUNICIPIO","NR_ZONA","NR_SECAO")), aes(total_validos)) +
              geom_histogram(bins = 70) + labs(x = "Votos válidos por seção", y = "Seções"), "tamanho_secoes")
  save_plot(ggplot(d, aes(total_validos, p)) + geom_bin_2d(bins = 60) + facet_wrap(~candidato) +
              labs(x = "Votos válidos por seção", y = "Proporção"), "tamanho_proporcao")
  q <- d[, .(prob = c(.8,.9,.95,.975,.99), quantil = quantile(p,c(.8,.9,.95,.975,.99))), by = .(REGIAO,candidato)]
  write_table(q, "quantis_superiores")
  save_plot(ggplot(q, aes(prob, quantil, colour = REGIAO)) + geom_line() + geom_point() + facet_wrap(~candidato), "quantis_superiores")
  blocks <- d[, .(n_secoes = .N, maximo = max(p), media = mean(p)), by = .(REGIAO,candidato,zona)]
  write_table(blocks, "blocos")
  write_table(blocks[, .(n_blocos = .N, unitarios = sum(n_secoes == 1), min = min(n_secoes),
    mediana = as.numeric(median(n_secoes)), q10 = quantile(n_secoes,.1), q90 = quantile(n_secoes,.9), max = max(n_secoes),
    rho_tamanho_max = suppressWarnings(cor(n_secoes,maximo,method="spearman"))), by = .(REGIAO,candidato)], "estrutura_blocos")
  save_plot(ggplot(blocks[candidato==cfg$candidates[1]], aes(n_secoes)) + geom_histogram(bins = 50) + facet_wrap(~REGIAO, scales = "free_y") +
              labs(x = "Seções por zona", y = "Blocos (um candidato)"), "tamanho_blocos")
  save_plot(ggplot(blocks, aes(n_secoes,maximo)) + geom_point(alpha = .15) + facet_grid(REGIAO~candidato), "blocos_maximos")
  # Decomposição descritiva da variância; não é teste temporal nem prova de independência.
  dep <- rbindlist(lapply(intersect(c("zona","municipio","local"),names(d)), function(id)
    d[, {m <- tapply(p, get(id), mean); k <- table(get(id)); v <- sum((p-mean(p))^2)
      .(agrupamento=id, grupos=length(m), fracao_entre=if(v>0)sum(k[names(m)]*(m-mean(p))^2)/v else NA_real_)
    }, by=.(REGIAO,candidato)]))
  write_table(dep, "agrupamento_descritivo")
  blocks
}
