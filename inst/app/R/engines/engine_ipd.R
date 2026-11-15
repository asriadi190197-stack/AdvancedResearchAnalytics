safe_model_frame <- function(data, vars) {
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  missing <- setdiff(vars, names(data))
  if (length(missing)) stop("Variabel tidak ditemukan: ", paste(missing, collapse = ", "))

  dat <- data[, vars, drop = FALSE]

  for (v in names(dat)) {
    x <- dat[[v]]
    if (inherits(x, "haven_labelled")) x <- haven::as_factor(x)
    if (is.character(x)) x <- factor(x)
    if (is.factor(x)) x <- droplevels(x)
    dat[[v]] <- x
  }

  dat
}

fit_cluster_regression <- function(dat, outcome, predictor, covariates = NULL) {
  usable_cov <- character(0)

  for (v in covariates %||% character(0)) {
    x <- dat[[v]]
    if (is.factor(x)) x <- droplevels(x)
    if (length(unique(x[!is.na(x)])) >= 2) usable_cov <- c(usable_cov, v)
  }

  rhs_vars <- c(predictor, usable_cov)
  f <- stats::reformulate(rhs_vars, response = outcome)

  # model.frame/model.matrix preserve factor handling, lm.fit avoids the heavier lm() object.
  mf <- stats::model.frame(f, data = dat, na.action = stats::na.omit)
  if (nrow(mf) <= length(rhs_vars) + 2) return(NULL)

  y <- stats::model.response(mf)
  X <- stats::model.matrix(stats::terms(mf), data = mf)

  pred_col <- match(predictor, colnames(X))
  if (is.na(pred_col)) return(NULL)

  fit <- stats::lm.fit(x = X, y = y)

  if (fit$rank < 1 || fit$df.residual <= 0) return(NULL)

  beta <- fit$coefficients[pred_col]
  if (!is.finite(beta)) return(NULL)

  rss <- sum(fit$residuals^2)
  sigma2 <- rss / fit$df.residual

  # Standard error using QR decomposition.
  R <- qr.R(fit$qr)
  Rinv <- tryCatch(chol2inv(R[seq_len(fit$rank), seq_len(fit$rank), drop = FALSE]),
                   error = function(e) NULL)
  if (is.null(Rinv)) return(NULL)

  pivot <- fit$qr$pivot[seq_len(fit$rank)]
  pos <- match(pred_col, pivot)
  if (is.na(pos)) return(NULL)

  se <- sqrt(sigma2 * Rinv[pos, pos])
  if (!is.finite(se) || se <= 0) return(NULL)

  tval <- beta / se
  pval <- 2 * stats::pt(abs(tval), df = fit$df.residual, lower.tail = FALSE)

  data.frame(
    beta = unname(beta),
    se = unname(se),
    p = unname(pval),
    n = nrow(mf),
    stringsAsFactors = FALSE
  )
}

run_two_stage_ipd <- function(data, outcome, predictor, cluster,
                              covariates = NULL, method = "REML",
                              min_cluster_n = 10, moderator = NULL,
                              progress_callback = NULL,
                              compute_diagnostics = FALSE,
                              max_clusters = 2000) {
  needed <- unique(c(outcome, predictor, cluster, covariates, moderator))
  dat <- safe_model_frame(data, needed)

  if (!is.numeric(dat[[outcome]])) stop("Outcome harus numerik.")
  if (!is.numeric(dat[[predictor]])) stop("Prediktor utama harus numerik.")

  ccvars <- unique(c(outcome, predictor, cluster, covariates))
  dat <- dat[stats::complete.cases(dat[, ccvars, drop = FALSE]), , drop = FALSE]

  if (nrow(dat) < 10) stop("Data lengkap terlalu sedikit untuk Two-Stage IPD.")
  if (length(unique(dat[[cluster]])) < 2) stop("Minimal dua cluster diperlukan.")

  diag <- ipd_cluster_diagnostics(dat, cluster, min_cluster_n)
  if (diag$clusters > max_clusters) {
    stop(
      paste0(
        "Two-Stage IPD dibatalkan: terdapat ", format(diag$clusters, big.mark=","),
        " cluster. Mode interaktif dibatasi maksimal ", format(max_clusters, big.mark=","),
        " cluster agar aplikasi tetap responsif. Pilih unit agregasi/studi yang lebih tinggi."
      )
    )
  }
  if (diag$valid_clusters < 2) {
    stop(
      paste0(
        "Two-Stage IPD tidak dapat dijalankan: hanya ", diag$valid_clusters,
        " cluster yang memiliki minimal ", min_cluster_n, " observasi."
      )
    )
  }

  # split only the columns needed by Stage-1 to reduce copying.
  stage_vars <- unique(c(outcome, predictor, cluster, covariates))
  stage_dat <- dat[, stage_vars, drop = FALSE]
  spl <- split(stage_dat, stage_dat[[cluster]], drop = TRUE)
  cluster_names <- names(spl)
  total <- length(cluster_names)

  rows <- vector("list", total)

  for (i in seq_along(cluster_names)) {
    g <- cluster_names[[i]]
    d <- spl[[i]]

    if (!is.null(progress_callback) && (i == 1 || i %% max(1, floor(total / 50)) == 0 || i == total)) {
      progress_callback(i, total, g)
    }

    if (nrow(d) < min_cluster_n) next
    if (length(unique(d[[predictor]])) < 2) next

    z <- tryCatch(
      fit_cluster_regression(d, outcome, predictor, covariates),
      error = function(e) NULL
    )

    if (is.null(z) || !is.finite(z$beta) || !is.finite(z$se) || z$se <= 0) next

    z$cluster <- g
    rows[[i]] <- z
  }

  rows <- Filter(Negate(is.null), rows)
  stage1 <- if (length(rows)) do.call(rbind, rows) else NULL

  if (is.null(stage1) || nrow(stage1) < 2)
    stop("Minimal dua cluster dengan estimasi Stage-1 valid diperlukan.")

  stage1$ci_low <- stage1$beta - 1.96 * stage1$se
  stage1$ci_high <- stage1$beta + 1.96 * stage1$se

  if (!is.null(progress_callback)) progress_callback(total, total, "Pooling")

  meta <- metafor::rma(yi = beta, sei = se, data = stage1, method = method)
  stage1$statistic <- stage1$beta / stage1$se
  stage1$weight_pct <- tryCatch(as.numeric(metafor::weights(meta)), error = function(e) rep(NA_real_, nrow(stage1)))

  # Heavy diagnostics are intentionally lazy.
  leave1out <- NULL
  influence <- NULL
  if (isTRUE(compute_diagnostics)) {
    # Compatibility argument for callers outside the UI: only fast LOO by default.
    leave1out <- ara_ipd_leave1out_fixed_tau(list(meta = meta, stage1 = stage1))
  }

  # Meta-regression is run on demand from Stage-1 estimates, without rerunning
  # all cluster regressions when the user selects/changes a moderator.
  meta_reg <- NULL

  list(
    stage1 = stage1,
    meta = meta,
    leave1out = leave1out,
    influence = influence,
    meta_reg = meta_reg,
    diagnostics_computed = isTRUE(compute_diagnostics),
    diagnostics_mode = if (isTRUE(compute_diagnostics)) "fast" else NULL,
    diagnostics_note = if (isTRUE(compute_diagnostics))
      "LOO cepat memakai tau² model penuh tetap; bukan refit REML." else NULL,
    k = meta$k,
    pooled = as.numeric(meta$b),
    se = meta$se,
    ci_low = meta$ci.lb,
    ci_high = meta$ci.ub,
    p = meta$pval,
    Q = meta$QE,
    Q_p = meta$QEp,
    I2 = meta$I2,
    tau2 = meta$tau2,
    outcome = outcome, predictor = predictor, cluster_var = cluster,
    method = method
  )
}

ipd_summary_table <- function(result) {
  data.frame(
    Statistic = c(
      "Clusters (k)", "Pooled effect", "SE",
      "95% CI lower", "95% CI upper", "p",
      "Q", "Q p-value", "I² (%)", "τ²"
    ),
    Value = c(
      result$k,
      round(result$pooled, 2),
      round(result$se, 2),
      round(result$ci_low, 2),
      round(result$ci_high, 2),
      ifelse(result$p < .001, "< .001", round(result$p, 2)),
      round(result$Q, 2),
      ifelse(result$Q_p < .001, "< .001", round(result$Q_p, 2)),
      round(result$I2, 2),
      round(result$tau2, 2)
    ),
    stringsAsFactors = FALSE
  )
}

run_one_stage_ipd <- function(data, outcome, predictor, cluster,
                              covariates = NULL,
                              random_slope = FALSE,
                              reml = TRUE) {
  dat <- safe_model_frame(data, unique(c(outcome, predictor, cluster, covariates)))

  if (!is.numeric(dat[[outcome]])) stop("Outcome harus numerik.")
  if (!is.numeric(dat[[predictor]])) stop("Prediktor utama harus numerik.")

  # Skip covariates with no usable variation.
  prep <- prepare_model_predictors(dat, covariates)
  dat <- prep$data
  covariates <- prep$valid

  keep <- unique(c(outcome, predictor, cluster, covariates))
  dat <- dat[, keep, drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]

  if (nrow(dat) < 10) stop("Data lengkap terlalu sedikit untuk One-Stage IPD.")

  dat[[cluster]] <- droplevels(factor(dat[[cluster]]))
  if (nlevels(dat[[cluster]]) < 2) stop("Minimal dua cluster diperlukan.")
  if (length(unique(dat[[predictor]])) < 2) stop("Prediktor utama tidak memiliki variasi.")

  fixed <- c(predictor, covariates)
  fixed_part <- paste(fixed, collapse = " + ")

  # Random slope needs within-cluster predictor variation in multiple clusters.
  use_slope <- FALSE
  if (isTRUE(random_slope)) {
    varying <- tapply(dat[[predictor]], dat[[cluster]], function(z) length(unique(z)) >= 2)
    use_slope <- sum(varying, na.rm = TRUE) >= 2
  }

  random_part <- if (use_slope) {
    sprintf("(1 + %s | %s)", predictor, cluster)
  } else {
    sprintf("(1 | %s)", cluster)
  }

  f <- stats::as.formula(sprintf("%s ~ %s + %s", outcome, fixed_part, random_part))

  ctrl <- lme4::lmerControl(
    optimizer = "nloptwrap",
    calc.derivs = FALSE
  )

  model <- lmerTest::lmer(f, data = dat, REML = reml, control = ctrl)

  co <- as.data.frame(summary(model)$coefficients)
  co$Term <- rownames(co)
  rownames(co) <- NULL

  if ("Pr(>|t|)" %in% names(co)) {
    names(co)[names(co) == "Pr(>|t|)"] <- "p"
  } else {
    co$p <- NA_real_
  }

  co$CI_low <- co$Estimate - 1.96 * co$`Std. Error`
  co$CI_high <- co$Estimate + 1.96 * co$`Std. Error`

  r2 <- tryCatch(performance::r2_nakagawa(model), error = function(e) NULL)

  list(
    model = model,
    formula = f,
    coefficients = co,
    n = nrow(dat),
    clusters = nlevels(dat[[cluster]]),
    aic = stats::AIC(model),
    bic = stats::BIC(model),
    logLik = as.numeric(stats::logLik(model)),
    r2_marginal = if (!is.null(r2)) unname(r2$R2_marginal) else NA_real_,
    r2_conditional = if (!is.null(r2)) unname(r2$R2_conditional) else NA_real_,
    singular = lme4::isSingular(model, tol = 1e-4),
    random_slope_applied = use_slope,
    skipped_covariates = prep$skipped
  )
}


# Fit an optional moderator using the already calculated Stage-1 effects.
# No per-cluster lm.fit calls are needed again.
fit_ipd_meta_regression <- function(stage1, data, cluster, moderator,
                                    method = "REML") {
  if (is.null(stage1) || nrow(stage1) < 3L)
    stop("Minimal tiga estimasi Stage-1 untuk meta-regression.")
  if (!moderator %in% names(data) || !cluster %in% names(data))
    stop("Moderator atau variabel cluster tidak ditemukan.")
  x <- data[[moderator]]
  if (!is.numeric(x)) stop("Moderator harus numerik untuk mode ini.")
  g <- as.character(data[[cluster]])
  keep <- is.finite(x) & !is.na(g)
  if (sum(keep) < 3L) stop("Moderator tidak memiliki cukup data lengkap.")
  # Compact group means, without passing the full analysis dataset.
  agg <- stats::aggregate(x[keep], by = list(cluster = g[keep]), FUN = mean)
  names(agg)[2] <- "moderator"
  matched <- match(as.character(stage1$cluster), agg$cluster)
  s <- stage1
  s$moderator <- agg$moderator[matched]
  s <- s[is.finite(s$moderator) & is.finite(s$beta) & is.finite(s$se) &
           s$se > 0, , drop = FALSE]
  if (nrow(s) < 3L || length(unique(s$moderator)) < 2L)
    stop("Minimal tiga klaster dan dua nilai moderator yang berbeda diperlukan.")
  model <- metafor::rma(yi = beta, sei = se, mods = ~ moderator,
                        data = s, method = method)
  list(model = model, k = nrow(s), moderator = moderator,
       method = method)
}


# O(k) leave-one-out screening with tau² held fixed at the full model estimate.
# This is deliberately NOT the same as leave1out.rma(), which re-estimates tau² k times.
ara_ipd_leave1out_fixed_tau <- function(result) {
  s <- result$stage1
  if (is.null(s) || nrow(s) < 3L) stop("Minimal tiga estimasi klaster untuk leave-one-out.")
  if (!all(is.finite(s$beta) & is.finite(s$se) & s$se > 0))
    stop("Estimasi tahap pertama atau SE tidak valid.")
  tau2 <- as.numeric(result$meta$tau2)[1]
  if (!is.finite(tau2) || tau2 < 0) stop("Tau² model penuh tidak valid.")
  w <- 1 / (s$se^2 + tau2)
  W <- sum(w)
  WY <- sum(w * s$beta)
  W_minus <- W - w
  if (any(W_minus <= 0)) stop("Tidak cukup bobot setelah penghapusan klaster.")
  est <- (WY - w * s$beta) / W_minus
  se <- sqrt(1 / W_minus)
  z <- est / se
  data.frame(
    `Cluster_Dihapus` = as.character(s$cluster),
    `Estimate` = as.numeric(est),
    `SE` = as.numeric(se),
    `CI_lower` = as.numeric(est - stats::qnorm(.975) * se),
    `CI_upper` = as.numeric(est + stats::qnorm(.975) * se),
    `z_value` = as.numeric(z),
    `p_value` = as.numeric(2 * stats::pnorm(abs(z), lower.tail = FALSE)),
    `Tau2_Ditetapkan` = rep(tau2, nrow(s)),
    `Metode` = rep("Cepat (tau² tetap; tidak refit REML)", nrow(s)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Each diagnostic runs only on request. The full fit remains untouched on error.
compute_ipd_diagnostics <- function(result, progress_callback = NULL,
                                    mode = c("fast", "exact"),
                                    compute_influence = FALSE) {
  if (is.null(result) || is.null(result$meta)) stop("Model Two-Stage belum tersedia.")
  mode <- match.arg(mode)
  if (!is.null(progress_callback)) progress_callback("Leave-one-out")
  result$leave1out <- if (identical(mode, "fast")) {
    ara_ipd_leave1out_fixed_tau(result)
  } else {
    metafor::leave1out(result$meta)
  }
  result$diagnostics_mode <- mode
  result$diagnostics_note <- if (identical(mode, "fast")) {
    "LOO cepat memakai bobot inverse-variance dengan tau² model penuh tetap; bukan hasil refit REML per klaster."
  } else {
    "LOO penuh mengestimasi ulang model setelah menghapus setiap klaster."
  }
  if (isTRUE(compute_influence) && is.null(result$influence)) {
    if (!is.null(progress_callback)) progress_callback("Influence diagnostics")
    result$influence <- metafor::influence(result$meta)
  }
  result$diagnostics_computed <- TRUE
  result
}


ipd_sig <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

ipd_p_text <- function(p) {
  if (is.na(p)) return("-")
  if (p < .001) return(paste0("< .001", ipd_sig(p)))
  paste0(sprintf("%.2f", p), ipd_sig(p))
}

ipd_pooled_publication_table <- function(result) {
  z <- result$pooled / result$se
  data.frame(
    `Parameter Model` = paste0("Efek Sintesis (", result$predictor, " -> ", result$outcome, ")"),
    `Cluster (k)` = as.integer(result$k),
    `Pooled Slope (B)` = as.numeric(result$pooled),
    `Std. Error (SE)` = as.numeric(result$se),
    `z-value` = as.numeric(z),
    `p-value` = as.numeric(result$p),
    `95% CI Lower` = as.numeric(result$ci_low),
    `95% CI Upper` = as.numeric(result$ci_high),
    `I2 Heterogenitas (%)` = as.numeric(result$I2),
    `Tau2 (Between Var)` = as.numeric(result$tau2),
    `Cochran Q` = as.numeric(result$Q),
    `Cochran Q df` = as.integer(result$k - 1),
    `Cochran Q p-value` = as.numeric(result$Q_p),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

ipd_stage1_publication_table <- function(result) {
  s <- result$stage1
  if (anyNA(s$cluster) || anyDuplicated(as.character(s$cluster)))
    stop("Integritas IPD Stage-1 gagal: ID cluster kosong atau duplikat")
  data.frame(
    `Klaster / Wilayah` = as.character(s$cluster),
    `N Sampel` = as.integer(s$n),
    `Koefisien (B)` = as.numeric(s$beta),
    `Std. Error (SE)` = as.numeric(s$se),
    `95% CI Lower` = as.numeric(s$ci_low),
    `95% CI Upper` = as.numeric(s$ci_high),
    `z / t` = as.numeric(s$statistic),
    `p-value` = as.numeric(s$p),
    `Bobot Model (%)` = as.numeric(s$weight_pct),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}


ipd_cluster_diagnostics <- function(data, cluster, min_cluster_n = 10) {
  if (!cluster %in% names(data)) stop("Variabel cluster tidak ditemukan.")

  g <- data[[cluster]]
  g <- g[!is.na(g)]
  tab <- table(g)

  valid_clusters <- sum(tab >= min_cluster_n)

  list(
    clusters = length(tab),
    mean_n = if (length(tab)) mean(as.numeric(tab)) else NA_real_,
    median_n = if (length(tab)) stats::median(as.numeric(tab)) else NA_real_,
    min_n = if (length(tab)) min(tab) else NA_integer_,
    max_n = if (length(tab)) max(tab) else NA_integer_,
    valid_clusters = valid_clusters,
    pct_valid = if (length(tab)) 100 * valid_clusters / length(tab) else 0,
    suitable = length(tab) >= 2 && valid_clusters >= 2 && length(tab) <= 2000
  )
}
