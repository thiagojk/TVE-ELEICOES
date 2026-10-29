prepare_data <- function(cfg) {
  if (!file.exists(cfg$input)) stop("Base ausente: coloque o CSV em data/raw/ ou defina TVE_DATA.")
  # Somente as colunas necessárias: reduz a memória sem alterar o arquivo bruto.
  header <- names(fread(cfg$input, nrows = 0, sep = cfg$sep, encoding = cfg$encoding, integer64 = "character"))
  required <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "DS_CARGO",
                "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS", "NR_TURNO", "ANO_ELEICAO")
  if (length(setdiff(required, header))) stop("Colunas ausentes: ", paste(setdiff(required, header), collapse = ", "))
  keep <- intersect(c(required, "NM_MUNICIPIO", "NR_LOCAL_VOTACAO", "DT_ELEICAO", "DS_ELEICAO"), header)
  x <- fread(cfg$input, select = keep, colClasses = "character", sep = cfg$sep,
             encoding = cfg$encoding, na.strings = c("", "NA"))
  x[, (names(x)) := lapply(.SD, enc2utf8)]
  write_table(data.table(coluna = names(x), ausentes = vapply(x, function(z) sum(is.na(z)), integer(1))), "auditoria_ausentes")
  write_table(x[, .N, by = .(ANO_ELEICAO, NR_TURNO, DS_CARGO, NM_VOTAVEL, NR_VOTAVEL)], "categorias_observadas")
  observed_names <- normalize_name(x$NM_VOTAVEL)
  x[, nome := names(cfg$candidate_names)[match(observed_names, cfg$candidate_names)]]
  x <- x[ANO_ELEICAO == as.character(cfg$year) & NR_TURNO == as.character(cfg$round) &
           normalize_name(DS_CARGO) == "PRESIDENTE"]
  if (!nrow(x)) stop("Nenhum registro de presidente/ano/turno configurados. Consulte categorias_observadas.csv.")
  found <- unique(x[nome %in% cfg$candidates, nome])
  if (!setequal(found, cfg$candidates)) stop("Candidatos esperados não encontrados: ", paste(setdiff(cfg$candidates, found), collapse = ", "))
  x <- x[nome %in% cfg$candidates]
  ids <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
  if (anyNA(x[, ..ids])) stop("Identificação de seção ausente.")
  x[, votos := suppressWarnings(as.numeric(QT_VOTOS))]
  bad <- x[!is.finite(votos) | votos < 0 | votos != floor(votos)]
  write_table(bad, "votos_invalidos")
  if (nrow(bad)) stop("QT_VOTOS inválido; consulte votos_invalidos.csv.")
  dup <- x[, .N, by = c(ids, "nome")][N > 1]
  write_table(dup, "duplicidades")
  if (nrow(dup)) stop("Duplicidades por seção/candidato: requer investigação; não somadas automaticamente.")
  write_table(x[, .(n_numeros = uniqueN(NR_VOTAVEL)), by = nome], "codigos_candidatos")
  wide <- dcast(x, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~ nome, value.var = "votos")
  wide[, n_candidatos := rowSums(!is.na(.SD)), .SDcols = cfg$candidates]
  write_table(wide[, .N, by = n_candidatos], "candidatos_por_secao")
  # Ausência de linha NÃO implica zero votos. Seções incompletas ficam em quarentena.
  write_table(wide[n_candidatos != 2], "secoes_incompletas")
  wide <- wide[n_candidatos == 2]
  wide[, total_validos := rowSums(.SD), .SDcols = cfg$candidates]
  write_table(wide[total_validos == 0], "secoes_sem_votos")
  wide <- wide[total_validos > 0]
  regions <- list(Norte = c("AC","AP","AM","PA","RO","RR","TO"),
    Nordeste = c("AL","BA","CE","MA","PB","PE","PI","RN","SE"),
    `Centro-Oeste` = c("DF","GO","MT","MS"), Sudeste = c("ES","MG","RJ","SP"), Sul = c("PR","RS","SC"))
  map <- rbindlist(lapply(names(regions), function(r) data.table(SG_UF = regions[[r]], REGIAO = r)))
  write_table(wide[SG_UF == "ZZ", .(secoes = .N)], "exterior")
  wide <- merge(wide[SG_UF != "ZZ"], map, by = "SG_UF", all.x = TRUE)
  if (anyNA(wide$REGIAO)) stop("UF sem região conhecida.")
  d <- melt(wide, id.vars = c(ids, "REGIAO", "total_validos", "n_candidatos"),
            measure.vars = cfg$candidates, variable.name = "candidato", value.name = "votos")
  d[, candidato := as.character(candidato)]
  d[, `:=`(p = votos / total_validos, zona = paste(SG_UF, NR_ZONA, sep = ":"),
            municipio = paste(SG_UF, CD_MUNICIPIO, sep = ":"))]
  if ("NR_LOCAL_VOTACAO" %in% names(x)) {
    meta <- unique(x[, c(ids, "NR_LOCAL_VOTACAO"), with = FALSE])
    if (nrow(meta) != uniqueN(meta, by = ids)) stop("Local inconsistente por seção.")
    d <- merge(d, meta, by = ids, all.x = TRUE)
    d[, local := paste(SG_UF, NR_ZONA, NR_LOCAL_VOTACAO, sep = ":")]
  }
  d[, percentual := 100 * p]
  stopifnot(all(d$p >= 0 & d$p <= 1))
  write_table(d[, .(n = .N, zeros = sum(p == 0), uns = sum(p == 1), pequenas = sum(total_validos < cfg$small_section)),
                by = .(REGIAO, candidato)], "auditoria_analitica")
  saveRDS(d, "data/processed/analitica.rds")
  write_table(data.table(arquivo = basename(cfg$input), bytes = file.info(cfg$input)$size,
                         md5 = unname(tools::md5sum(cfg$input)), data_execucao = as.character(Sys.time()),
                         encoding = cfg$encoding), "proveniencia")
  d
}
