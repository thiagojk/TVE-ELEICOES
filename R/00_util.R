suppressPackageStartupMessages({library(data.table); library(ggplot2)})
options(stringsAsFactors = FALSE)
normalize_name <- function(x) toupper(trimws(iconv(x, to = "ASCII//TRANSLIT")))
write_table <- function(x, name) fwrite(x, file.path("tables", paste0(name, ".csv")))
save_plot <- function(g, name, width = 10, height = 6) {
  ggsave(file.path("figures", paste0(name, ".png")), g, width = width, height = height, dpi = 160, bg = "white")
}
theme_set(theme_minimal(base_size = 12) + theme(legend.position = "bottom"))
safe_fit <- function(x, model, u = NULL, uncertainty = TRUE) {
  warnings <- character()
  fit <- tryCatch(withCallingHandlers(
    if (model == "GPD") evd::fpot(x, u, model = "gpd", std.err = FALSE) else evd::fgev(x, std.err = FALSE),
    warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")}),
    error = function(e) structure(list(error = conditionMessage(e)), class = "fit_error"))
  if (!inherits(fit, "fit_error") && uncertainty) {
    e <- fit$estimate
    fit$std.err <- setNames(rep(NA_real_, length(e)), names(e))
    if (e["shape"] <= -.5) warnings <- c(warnings, "xi <= -0.5: IC de Wald não regular, omitido") else {
      # Diferenciação em log-escala: evita os erros numéricos do Hessiano padrão
      # quando a escala é pequena na unidade de proporções.
      j <- which(names(e)=="scale"); theta <- e; theta[j] <- log(e[j])
      sample_x <- if(model=="GPD") x[x>u]-u else x
      nll <- function(t) {
        par <- t; par[j] <- exp(t[j])
        ll <- if(model=="GPD") evd::dgpd(sample_x,scale=par["scale"],shape=par["shape"],log=TRUE) else
          evd::dgev(x,loc=par["loc"],scale=par["scale"],shape=par["shape"],log=TRUE)
        if(any(!is.finite(ll)))return(1e30)
        -sum(ll)
      }
      covar <- function(step) {
        h <- numDeriv::hessian(nll,theta,method.args=list(d=step))
        if(any(!is.finite(h)) || min(eigen(h,symmetric=TRUE,only.values=TRUE)$values)<=0)stop("Hessiano não positivo")
        jac <- diag(length(e)); jac[j,j] <- e[j]
        jac %*% solve(h) %*% jac
      }
      v <- tryCatch({v1<-covar(1e-4);v2<-covar(5e-5)
        if(any(abs(diag(v1)-diag(v2))/pmax(diag(v1),1e-12)>.1))stop("curvatura instável")
        v2},error=function(err)NULL)
      if(is.null(v)) warnings<-c(warnings,"Curvatura instável: IC de Wald omitido") else {
        fit$var.cov<-v;fit$std.err<-setNames(sqrt(diag(v)),names(e))
      }
    }
  }
  list(fit = fit, warnings = paste(unique(warnings), collapse = " | "))
}
valid_fit <- function(z) !inherits(z$fit, "fit_error") &&
  identical(z$fit$convergence, "successful") && all(is.finite(z$fit$estimate))
fit_row <- function(z, region, candidate, model, n, u = NA_real_, ne = NA_integer_) {
  row <- data.table(REGIAO = region, candidato = candidate, modelo = model, n = n, u = u,
                    excedencias = ne, status = "falha", avisos = z$warnings)
  if (!valid_fit(z)) {
    row[, avisos := paste(avisos, z$fit$error, z$fit$message)]; return(row)
  }
  est <- z$fit$estimate
  for (v in names(est)) {
    se <- if (is.null(z$fit$std.err)) NA_real_ else unname(z$fit$std.err[v])
    row[, (v) := unname(est[v])]; row[, (paste0(v, "_se_iid")) := se]
    row[, (paste0(v, "_lo_iid")) := unname(est[v]) - 1.96 * se]
    row[, (paste0(v, "_hi_iid")) := unname(est[v]) + 1.96 * se]
  }
  row[, status := if (est["shape"] <= -.5) "nao_regular" else if (!is.finite(shape_se_iid)) "curvatura_instavel" else "ajuste_exploratorio"]
  row
}
return_gpd <- function(T, u, rate, sigma, xi) {
  a <- T * rate
  ifelse(a > 1, u + if (abs(xi) < 1e-7) sigma * log(a) else sigma / xi * (a^xi - 1), NA_real_)
}
