cfg <- list(
  input = Sys.getenv("TVE_DATA", "data/raw/votacao_secao_2026_BR.csv"),
  encoding = Sys.getenv("TVE_ENCODING", "Latin-1"), sep = ";",
  year = 2026L, round = 2L,
  candidates = c("LULA", "FLAVIO BOLSONARO"),
  candidate_names = c(LULA = "LUIZ INACIO LULA DA SILVA", `FLAVIO BOLSONARO` = "FLAVIO NANTES BOLSONARO"),
  threshold_probs = seq(.80, .99, .01), min_exceed = 100L,
  min_blocks = 50L, min_block_median = 20L, max_block_ratio = 4,
  small_section = 30L, small_block = 10L,
  bootstrap = as.integer(Sys.getenv("TVE_BOOTSTRAP", "199")),
  return_T = c(100, 500, 1000, 5000, 10000), seed = 20261029L
)
