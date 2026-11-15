# Optional post-fit meta-analytic diagnostics; errors isolated by test.
ara_ipd_extra <- function(x, options = c("prediction", "egger", "trimfill")) {
  if (is.null(x$meta)) stop("Model Two-Stage IPD belum tersedia.")
  out <- list(); states <- list()
  attempt <- function(name, fun) tryCatch({
      out[[name]] <<- fun()
      states[[length(states) + 1L]] <<- data.frame(Uji = name, Status = "OK", Pesan = "Berhasil")
    }, error = function(e) {
      states[[length(states) + 1L]] <<- data.frame(Uji = name, Status = "GAGAL", Pesan = conditionMessage(e))
    })
  if ("prediction" %in% options) attempt("Prediction interval", function() {
    p <- stats::predict(x$meta)
    data.frame(Estimate = as.numeric(p$pred), CI_low = as.numeric(p$ci.lb),
      CI_high = as.numeric(p$ci.ub), PI_low = as.numeric(p$pi.lb),
      PI_high = as.numeric(p$pi.ub), k = x$meta$k)
  })
  if ("egger" %in% options) attempt("Small study effects (Egger)", function() {
    if (x$meta$k < 10L) stop("Memerlukan minimal 10 estimasi klaster untuk interpretasi uji Egger.")
    e <- metafor::regtest(x$meta, model = "rma", predictor = "sei")
    data.frame(Statistic = as.numeric(e$zval), P_value = as.numeric(e$pval),
      Method = "Egger-style meta-regression", k = x$meta$k)
  })
  if ("trimfill" %in% options) attempt("Trim and fill", function() {
    # Iterative procedure; requested explicitly, never run implicitly.

    if (x$meta$k < 10L) stop("Memerlukan minimal 10 estimasi klaster.")
    tf <- metafor::trimfill(x$meta)
    data.frame(Estimate_Adjusted = as.numeric(tf$b), CI_low = tf$ci.lb,
      CI_high = tf$ci.ub, Studies_Imputed = as.numeric(tf$k0), k = x$meta$k)
  })
  out$Status <- if (length(states)) do.call(rbind, states) else
    data.frame(Uji = character(), Status = character(), Pesan = character())
  out
}
