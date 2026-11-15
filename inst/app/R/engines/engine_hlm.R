build_hlm_formula <- function(outcome, predictors = NULL, cluster,
                              random_slope = NULL,
                              interaction_l1 = NULL,
                              interaction_l2 = NULL) {
  fixed_terms <- predictors %||% character(0)

  if (!is.null(interaction_l1) && !is.null(interaction_l2) &&
      nzchar(interaction_l1) && nzchar(interaction_l2)) {
    fixed_terms <- unique(c(
      fixed_terms, interaction_l1, interaction_l2,
      paste(interaction_l1, interaction_l2, sep = ":")
    ))
  }

  fixed_part <- if (!length(fixed_terms)) "1" else paste(fixed_terms, collapse = " + ")
  random_part <- if (!is.null(random_slope) && nzchar(random_slope)) {
    sprintf("(1 + %s | %s)", random_slope, cluster)
  } else {
    sprintf("(1 | %s)", cluster)
  }

  stats::as.formula(sprintf("%s ~ %s + %s", outcome, fixed_part, random_part))
}

hlm_control <- function() {
  lme4::lmerControl(
    optimizer = "nloptwrap",
    calc.derivs = FALSE,
    check.conv.grad = lme4::.makeCC(action = "warning", tol = 2e-3),
    check.conv.singular = lme4::.makeCC(action = "message", tol = 1e-4)
  )
}

extract_fixed_table <- function(model) {
  out <- as.data.frame(summary(model)$coefficients)
  out$Term <- rownames(out)
  rownames(out) <- NULL

  if ("Pr(>|t|)" %in% names(out)) names(out)[names(out) == "Pr(>|t|)"] <- "p"
  if (!("p" %in% names(out))) out$p <- NA_real_

  # Fast Wald CI: avoids an additional confint() pass on very large models.
  out$CI_low <- out$Estimate - 1.96 * out$`Std. Error`
  out$CI_high <- out$Estimate + 1.96 * out$`Std. Error`
  out
}

fast_r2_nakagawa <- function(model) {
  # Use performance when available; failure must never abort the HLM analysis.
  tryCatch({
    r <- performance::r2_nakagawa(model)
    c(marginal = unname(r$R2_marginal), conditional = unname(r$R2_conditional))
  }, error = function(e) c(marginal = NA_real_, conditional = NA_real_))
}

extract_fit <- function(model, label, compute_r2 = FALSE) {
  r2 <- if (isTRUE(compute_r2)) fast_r2_nakagawa(model) else c(marginal=NA_real_, conditional=NA_real_)
  data.frame(
    Model = label,
    AIC = stats::AIC(model),
    BIC = stats::BIC(model),
    LogLik = as.numeric(stats::logLik(model)),
    Marginal_R2 = unname(r2["marginal"]),
    Conditional_R2 = unname(r2["conditional"]),
    Singular = lme4::isSingular(model, tol = 1e-4),
    stringsAsFactors = FALSE
  )
}

fit_hlm_models <- function(data, outcome, cluster, l1 = NULL, l2 = NULL,
                           random_slope = NULL, reml = TRUE,
                           interaction_l1 = NULL, interaction_l2 = NULL,
                           progress = NULL) {

  tick <- function(value, detail = NULL) {
    if (is.function(progress)) progress(value, detail)
  }

  required <- unique(c(outcome, cluster, l1, l2, random_slope, interaction_l1, interaction_l2))
  required <- required[!is.na(required) & nzchar(required)]
  missing_vars <- setdiff(required, names(data))
  if (length(missing_vars)) stop("Variabel tidak ditemukan: ", paste(missing_vars, collapse = ", "))

  dat <- data[, required, drop = FALSE]
  tick(.05, "Memeriksa variabel...")

  if (!is.numeric(dat[[outcome]])) stop("Outcome HLM harus berupa variabel numerik.")

  if (inherits(dat[[cluster]], "haven_labelled")) dat[[cluster]] <- haven::as_factor(dat[[cluster]])
  dat[[cluster]] <- droplevels(factor(dat[[cluster]]))
  if (nlevels(dat[[cluster]]) < 2) stop("Cluster harus memiliki minimal dua kelompok.")

  prep0 <- prepare_model_predictors(dat, c(l1, l2))
  dat <- prep0$data
  skipped_predictors <- prep0$skipped
  l1 <- intersect(l1 %||% character(0), prep0$valid)
  l2 <- intersect(l2 %||% character(0), prep0$valid)

  if (!is.null(random_slope) && !random_slope %in% prep0$valid) random_slope <- NULL
  if (!is.null(interaction_l1) && !interaction_l1 %in% prep0$valid) interaction_l1 <- NULL
  if (!is.null(interaction_l2) && !interaction_l2 %in% prep0$valid) interaction_l2 <- NULL

  keep <- unique(c(outcome, cluster, l1, l2, random_slope, interaction_l1, interaction_l2))
  keep <- keep[!is.na(keep) & nzchar(keep)]
  dat <- dat[, keep, drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (nrow(dat) < 10) stop("Terlalu sedikit complete cases untuk HLM.")

  prep1 <- prepare_model_predictors(dat, c(l1, l2))
  dat <- prep1$data
  if (nrow(prep1$skipped)) skipped_predictors <- unique(rbind(skipped_predictors, prep1$skipped))
  l1 <- intersect(l1, prep1$valid)
  l2 <- intersect(l2, prep1$valid)
  dat[[cluster]] <- droplevels(factor(dat[[cluster]]))

  if (nlevels(dat[[cluster]]) < 2) stop("Setelah penyaringan data, cluster hanya memiliki satu kelompok.")

  # Random slope requires at least a little within-cluster variation.
  if (!is.null(random_slope) && nzchar(random_slope)) {
    slope_ok <- random_slope %in% names(dat) && length(unique(dat[[random_slope]])) > 1
    if (!slope_ok) random_slope <- NULL
  }

  tick(.12, "Menjalankan null model...")
  formulas <- list()
  models <- list()
  ctrl <- hlm_control()

  formulas[["Model 0: Null"]] <- stats::as.formula(sprintf("%s ~ 1 + (1 | %s)", outcome, cluster))
  models[["Model 0: Null"]] <- lmerTest::lmer(formulas[["Model 0: Null"]], data = dat, REML = reml, control = ctrl)

  if (length(l1)) {
    tick(.30, "Menjalankan model Level-1...")
    formulas[["Model 1: Level-1"]] <- build_hlm_formula(outcome, l1, cluster)
    models[["Model 1: Level-1"]] <- lmerTest::lmer(formulas[["Model 1: Level-1"]], data = dat, REML = reml, control = ctrl)
  }

  if (length(l2)) {
    tick(.48, "Menjalankan model Level-2...")
    formulas[["Model 2: Level-2"]] <- build_hlm_formula(outcome, l2, cluster)
    models[["Model 2: Level-2"]] <- lmerTest::lmer(formulas[["Model 2: Level-2"]], data = dat, REML = reml, control = ctrl)
  }

  tick(.65, "Menjalankan full model...")
  formulas[["Model 3: Full"]] <- build_hlm_formula(
    outcome, c(l1,l2), cluster, random_slope, interaction_l1, interaction_l2
  )
  models[["Model 3: Full"]] <- lmerTest::lmer(formulas[["Model 3: Full"]], data = dat, REML = reml, control = ctrl)

  m0 <- models[["Model 0: Null"]]
  mf <- models[["Model 3: Full"]]
  tick(.78, "Menghitung statistik model...")

  vc0 <- as.data.frame(lme4::VarCorr(m0))
  between <- vc0$vcov[vc0$grp == cluster][1]
  within <- vc0$vcov[vc0$grp == "Residual"][1]
  icc <- between / (between + within)

  fit_rows <- lapply(seq_along(models), function(i) {
    extract_fit(models[[i]], names(models)[i], compute_r2 = identical(names(models)[i], "Model 3: Full"))
  })
  fit_tbl <- do.call(rbind, fit_rows)
  rownames(fit_tbl) <- NULL

  # IMPORTANT: do not refit all models under ML automatically on large data.
  # If REML=FALSE, the fitted models are already comparable and ANOVA/LRT is cheap.
  comparison <- NULL
  comparison_note <- NULL
  if (length(models) > 1 && !isTRUE(reml)) {
    # Level-1-only and Level-2-only models are typically non-nested.
    # Report a null-vs-full LRT only if random structures are identical.
    same_random <- identical(lme4::findbars(stats::formula(m0)),
                             lme4::findbars(stats::formula(mf)))
    if (same_random && length(setdiff(names(lme4::fixef(m0)), names(lme4::fixef(mf)))) == 0L) {
      comparison <- tryCatch({
        x <- stats::anova(m0, mf, refit = FALSE)
        x$Model <- rownames(x); rownames(x) <- NULL; x
      }, error = function(e) NULL)
    } else comparison_note <- "Uji LR otomatis tidak dihitung: struktur efek acak berbeda."
  } else if (length(models) > 1 && isTRUE(reml)) {
    comparison_note <- "REML aktif: aplikasi tidak melakukan refit ML otomatis agar analisis data besar tetap responsif. Gunakan Kecocokan Model atau nonaktifkan REML jika membutuhkan LRT antar fixed-effects models."
  }

  tick(.90, "Menyiapkan output...")
  re_full <- lme4::ranef(mf, condVar = TRUE)[[cluster]]
  post_var <- attr(re_full, "postVar")
  blup <- as.data.frame(re_full)
  blup$Cluster <- rownames(re_full)
  rownames(blup) <- NULL
  names(blup)[1] <- "Random_Intercept"
  if (!is.null(post_var) && length(dim(post_var)) == 3) {
    blup$Random_Intercept_SE <- sqrt(post_var[1, 1, ])
  } else {
    blup$Random_Intercept_SE <- ara_hlm_conditional_sd_ri(mf, cluster, blup$Cluster)
  }

  fixed <- extract_fixed_table(mf)
  random <- as.data.frame(lme4::VarCorr(mf))
  singular <- lme4::isSingular(mf, tol = 1e-4)
  tick(1, "Selesai")

  list(
    data = dat, models = models, formulas = formulas,
    null_model = m0, full_model = mf, full_formula = formulas[["Model 3: Full"]],
    icc = icc, between = between, within = within,
    fixed = fixed, random = random, fit = fit_tbl,
    comparison = comparison, comparison_note = comparison_note,
    blup = blup, n = nrow(dat), clusters = nlevels(dat[[cluster]]),
    skipped_predictors = skipped_predictors, singular = singular,
    outcome = outcome, cluster_var = cluster,
    l1_vars = l1, l2_vars = l2,
    reml = reml
  )
}

hlm_apa_table <- function(result) {
  x <- result$fixed
  data.frame(
    Predictor = x$Term,
    B = as.numeric(x$Estimate),
    SE = as.numeric(x$`Std. Error`),
    df = if ("df" %in% names(x)) as.numeric(x$df) else NA_real_,
    t = if ("t value" %in% names(x)) as.numeric(x$`t value`) else NA_real_,
    p = as.numeric(x$p),
    CI_lower = as.numeric(x$CI_low),
    CI_upper = as.numeric(x$CI_high),
    stringsAsFactors = FALSE
  )
}


hlm_sig <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

hlm_p_text <- function(p) {
  if (is.na(p)) return("-")
  stars <- hlm_sig(p)
  if (p < .001) return(paste0("< .001", stars))
  paste0(sprintf("%.2f", p), stars)
}

hlm_model_variance <- function(model, cluster_var) {
  vc <- as.data.frame(lme4::VarCorr(model))
  between <- vc$vcov[vc$grp == cluster_var & vc$var1 == "(Intercept)"][1]
  within <- vc$vcov[vc$grp == "Residual"][1]
  c(between = between, within = within,
    icc = between / (between + within))
}

hlm_model_fixed <- function(model) {
  x <- extract_fixed_table(model)
  x
}

hlm_term_level <- function(term, l1, l2) {
  if (identical(term, "(Intercept)")) return("Intercept")
  if (length(l1) && any(vapply(l1, function(v) startsWith(term, v), logical(1)))) return("Level 1")
  if (length(l2) && any(vapply(l2, function(v) startsWith(term, v), logical(1)))) return("Level 2")
  if (grepl(":", term, fixed = TRUE)) return("Interaksi")
  "Prediktor"
}

hlm_model_comparison_publication <- function(result) {
  slots <- list(
    `Model 1 (Null)` = result$models[["Model 0: Null"]],
    `Model 2 (Level-1)` = result$models[["Model 1: Level-1"]],
    `Model 3 (Level-2)` = result$models[["Model 2: Level-2"]],
    `Model 4 (Model Penuh)` = result$models[["Model 3: Full"]]
  )

  # model terms appearing anywhere
  terms <- unique(unlist(lapply(slots, function(m) {
    if (is.null(m)) return(character(0))
    setdiff(hlm_model_fixed(m)$Term, "(Intercept)")
  })))

  rows <- c("Intercept (pada nilai 0 / kategori referensi)")
  if (length(terms)) {
    labels <- vapply(terms, function(term) {
      lev <- hlm_term_level(term, result$l1_vars, result$l2_vars)
      tag <- if (lev == "Level 1") "[L1]" else if (lev == "Level 2") "[L2]" else if (lev == "Interaksi") "[INT]" else "[P]"
      paste(tag, term)
    }, character(1))
    rows <- c(rows, labels)
  }
  rows <- c(
    rows,
    "Varians Antar-Cluster (Between)",
    "Varians Dalam-Cluster (Within)",
    "ICC (random intercept / residual; lihat catatan random slope)",
    "Reduksi Varians Level-1 (R²_L1)",
    "Reduksi Varians Level-2 (R²_L2)",
    if (isTRUE(result$reml)) "-2 restricted logLik (REML)" else "Deviance (-2LL, ML)",
    "Akaike Information Criterion (AIC)",
    "Bayesian Information Criterion (BIC)",
    "Uji LR vs Null (hanya ML dan model nested)"
  )

  out <- data.frame(`Parameter / Komponen Model` = rows, check.names = FALSE, stringsAsFactors = FALSE)
  null_var <- hlm_model_variance(slots[[1]], result$cluster_var)
  ll0 <- as.numeric(stats::logLik(slots[[1]]))
  df0 <- attr(stats::logLik(slots[[1]]), "df")

  for (j in seq_along(slots)) {
    m <- slots[[j]]

    if (is.null(m) && isTRUE(result$fast_mode) && j %in% c(2, 3)) {
      vals <- rep("Belum dihitung", length(rows))
      vals[length(vals)] <- "Mode Cepat"
    } else {
      vals <- rep("-", length(rows))
    }

    if (!is.null(m)) {
      fx <- hlm_model_fixed(m)
      int <- fx[fx$Term == "(Intercept)", , drop=FALSE]
      if (nrow(int)) vals[1] <- paste0(sprintf("%.6g (%.6g)", int$Estimate[1], int$`Std. Error`[1]), hlm_sig(int$p[1]))

      if (length(terms)) {
        for (k in seq_along(terms)) {
          rr <- fx[fx$Term == terms[k], , drop=FALSE]
          if (nrow(rr)) vals[1+k] <- paste0(sprintf("%.6g (%.6g)", rr$Estimate[1], rr$`Std. Error`[1]), hlm_sig(rr$p[1]))
        }
      }

      v <- hlm_model_variance(m, result$cluster_var)
      base <- 1 + length(terms)
      vals[base+1] <- sprintf("%.2f", v["between"])
      vals[base+2] <- sprintf("%.2f", v["within"])
      vals[base+3] <- sprintf("%.2f%%", 100*v["icc"])
      vals[base+4] <- if (j == 1) "-" else sprintf("%.2f%%", 100*(null_var["within"]-v["within"])/null_var["within"])
      vals[base+5] <- if (j == 1) "-" else sprintf("%.2f%%", 100*(null_var["between"]-v["between"])/null_var["between"])
      ll <- as.numeric(stats::logLik(m))
      vals[base+6] <- sprintf("%.2f", -2*ll)
      vals[base+7] <- sprintf("%.2f", stats::AIC(m))
      vals[base+8] <- sprintf("%.2f", stats::BIC(m))

      if (j == 1) {
        vals[base+9] <- "Model Acuan (Baseline)"
      } else {
        delta <- (-2*ll0) - (-2*ll)
        dfd <- attr(stats::logLik(m), "df") - df0
        nested <- all(names(lme4::fixef(slots[[1]])) %in% names(lme4::fixef(m))) &&
          identical(lme4::findbars(stats::formula(slots[[1]])), lme4::findbars(stats::formula(m)))
        if (!isTRUE(result$reml) && isTRUE(nested) && is.finite(delta) && delta >= 0 && dfd > 0) {
          pp <- stats::pchisq(delta, df=dfd, lower.tail=FALSE)
          vals[base+9] <- paste0("Δ = ", sprintf("%.2f", delta), " (p = ", hlm_p_text(pp), ")")
        } else {
          vals[base+9] <- "Tidak dihitung (REML / model tidak nested)"
        }
      }
    }
    out[[names(slots)[j]]] <- vals
  }
  out
}

hlm_full_coefficients_publication <- function(result) {
  x <- result$fixed
  mm <- tryCatch(stats::model.matrix(result$full_model), error=function(e) NULL)
  y <- result$data[[result$outcome]]
  sy <- stats::sd(y, na.rm=TRUE)

  std_beta <- rep(NA_real_, nrow(x))
  if (!is.null(mm) && is.finite(sy) && sy > 0) {
    for (i in seq_len(nrow(x))) {
      term <- x$Term[i]
      if (term == "(Intercept)") next
      idx <- match(term, colnames(mm))
      if (!is.na(idx)) {
        sx <- stats::sd(mm[,idx], na.rm=TRUE)
        if (is.finite(sx) && sx > 0) std_beta[i] <- x$Estimate[i] * sx / sy
      }
    }
  }

  level <- vapply(x$Term, hlm_term_level, character(1), l1=result$l1_vars, l2=result$l2_vars)
  level[level == "Level 1"] <- "Level 1"
  level[level == "Level 2"] <- "Level 2"
  # Squared standardized coefficients are NOT unique explained variance in HLM.
  # Export raw numerical values; Excel applies formatting without modifying them.
  data.frame(
    `Prediktor / Parameter` = ifelse(x$Term == "(Intercept)", "Intercept", x$Term),
    `Level Variabel` = level,
    `Koefisien (B)` = as.numeric(x$Estimate),
    `Std. Error (SE)` = as.numeric(x$`Std. Error`),
    `Beta standar (deskriptif)` = as.numeric(std_beta),
    `t-value` = if ("t value" %in% names(x)) as.numeric(x$`t value`) else NA_real_,
    `df` = if ("df" %in% names(x)) as.numeric(x$df) else NA_real_,
    `p-value` = as.numeric(x$p),
    `95% CI Lower (Wald)` = as.numeric(x$CI_low),
    `95% CI Upper (Wald)` = as.numeric(x$CI_high),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Conditional SD of random intercepts for Gaussian random-intercept-only HLM.
# Given fitted variance components, posterior Var(u_j | y_j) =
# (1/tau2 + n_j/sigma2)^(-1). No dense conditional-covariance array needed.
# For random-slope models use lme4::ranef(..., condVar=TRUE) instead.
ara_hlm_conditional_sd_ri <- function(model, cluster, cluster_ids) {
  vc <- lme4::VarCorr(model)
  if (!cluster %in% names(vc) || ncol(as.matrix(vc[[cluster]])) != 1L)
    return(rep(NA_real_, length(cluster_ids)))
  tau2 <- as.numeric(vc[[cluster]][1L, 1L])
  sigma2 <- stats::sigma(model)^2
  if (!is.finite(tau2) || tau2 < 0 || !is.finite(sigma2) || sigma2 <= 0)
    return(rep(NA_real_, length(cluster_ids)))
  if (tau2 == 0) return(rep(0, length(cluster_ids)))
  group <- lme4::getME(model, "flist")[[cluster]]
  counts <- table(group)
  n <- as.numeric(counts[match(as.character(cluster_ids), names(counts))])
  if (anyNA(n) || any(n <= 0)) return(rep(NA_real_, length(cluster_ids)))
  sqrt(1 / (1 / tau2 + n / sigma2))
}

hlm_blup_top20_publication <- function(result, n_top = 20) {
  b <- result$blup
  cluster <- result$cluster_var
  dat <- result$data
  grand_intercept <- lme4::fixef(result$full_model)["(Intercept)"]

  # One row per cluster, computed on exactly the complete cases used for fitting.
  # aggregate(..., FUN = function(z) c(n=..., mean=...)) may return a
  # matrix-valued column; vapply(stat$x, ...) is unsafe in that case.
  if (anyNA(b$Cluster) || anyDuplicated(as.character(b$Cluster))) {
    stop("Integritas BLUP gagal: ID cluster kosong atau berulang dalam random effects.")
  }
  ids <- as.character(dat[[cluster]])
  idx <- split(seq_len(nrow(dat)), ids, drop = TRUE)
  tmp <- data.frame(
    Cluster = names(idx),
    N = as.integer(lengths(idx)),
    Raw_Mean = vapply(idx, function(i) mean(dat[[result$outcome]][i], na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE
  )
  match_idx <- match(as.character(b$Cluster), tmp$Cluster)
  if (anyNA(match_idx)) stop("Integritas BLUP gagal: cluster model tidak ditemukan dalam data analisis.")
  z <- b
  z$N <- tmp$N[match_idx]
  z$Raw_Mean <- tmp$Raw_Mean[match_idx]
  if (anyDuplicated(as.character(z$Cluster))) stop("ID cluster BLUP tidak unik.")
  z$BLUP_Intercept <- as.numeric(grand_intercept) + z$Random_Intercept
  z <- z[order(z$BLUP_Intercept, decreasing=TRUE), , drop=FALSE]
  z <- head(z, n_top)

  data.frame(
    `ID Cluster` = z$Cluster,
    `Jumlah Observasi (n)` = as.integer(z$N),
    `Raw Mean (observed)` = as.numeric(z$Raw_Mean),
    `Random Intercept Deviation (BLUP)` = as.numeric(z$Random_Intercept),
    `Conditional Intercept (fixed + BLUP)` = as.numeric(z$BLUP_Intercept),
    `Conditional SD random effect` = as.numeric(z$Random_Intercept_SE),
    check.names=FALSE,
    stringsAsFactors=FALSE
  )
}


hlm_cluster_diagnostics <- function(data, cluster) {
  if (!cluster %in% names(data)) stop("Variabel cluster tidak ditemukan.")

  g <- data[[cluster]]
  g <- g[!is.na(g)]
  tab <- table(g)

  if (!length(tab)) {
    return(list(
      n = 0L, clusters = 0L, mean_n = NA_real_, median_n = NA_real_,
      min_n = NA_integer_, max_n = NA_integer_,
      singleton_pct = NA_real_, suitable = FALSE,
      reason = "Tidak ada cluster valid."
    ))
  }

  mean_n <- mean(as.numeric(tab))
  median_n <- stats::median(as.numeric(tab))
  singleton_pct <- mean(tab == 1) * 100

  suitable <- TRUE
  reason <- "Struktur cluster memadai."

  if (length(tab) > 10000 && mean_n < 2) {
    suitable <- FALSE
    reason <- "Jumlah cluster sangat besar tetapi rata-rata anggota per cluster < 2."
  } else if (median_n < 2 && singleton_pct >= 50) {
    suitable <- FALSE
    reason <- "Mayoritas cluster hanya memiliki satu observasi."
  } else if (length(tab) >= nrow(data) * .75) {
    suitable <- FALSE
    reason <- "Jumlah cluster hampir sama dengan jumlah observasi; variabel ini menyerupai ID unik."
  }

  list(
    n = length(g),
    clusters = length(tab),
    mean_n = mean_n,
    median_n = median_n,
    min_n = min(tab),
    max_n = max(tab),
    singleton_pct = singleton_pct,
    suitable = suitable,
    reason = reason
  )
}

fit_hlm_fast_core <- function(data, outcome, cluster, l1 = NULL, l2 = NULL,
                              random_slope = NULL, reml = TRUE,
                              interaction_l1 = NULL, interaction_l2 = NULL) {
  diag <- hlm_cluster_diagnostics(data, cluster)

  if (!isTRUE(diag$suitable)) {
    stop(
      paste0(
        "Cluster '", cluster, "' tidak cocok untuk HLM pada data ini. ",
        diag$reason,
        " Observasi=", format(diag$n, big.mark=","),
        "; cluster=", format(diag$clusters, big.mark=","),
        "; rata-rata anggota/cluster=", sprintf("%.2f", diag$mean_n),
        "; median=", sprintf("%.0f", diag$median_n),
        "; singleton=", sprintf("%.1f%%", diag$singleton_pct),
        ". Pilih unit cluster yang lebih tinggi/berulang."
      )
    )
  }

  required <- unique(c(outcome, cluster, l1, l2, random_slope, interaction_l1, interaction_l2))
  required <- required[!is.na(required) & nzchar(required)]
  dat <- data[, required, drop = FALSE]

  if (!is.numeric(dat[[outcome]])) stop("Outcome HLM harus berupa variabel numerik.")

  if (inherits(dat[[cluster]], "haven_labelled")) dat[[cluster]] <- haven::as_factor(dat[[cluster]])
  dat[[cluster]] <- droplevels(factor(dat[[cluster]]))

  prep <- prepare_model_predictors(dat, c(l1, l2))
  dat <- prep$data
  skipped_predictors <- prep$skipped
  l1 <- intersect(l1 %||% character(0), prep$valid)
  l2 <- intersect(l2 %||% character(0), prep$valid)

  if (!is.null(random_slope) && !random_slope %in% prep$valid) random_slope <- NULL
  if (!is.null(interaction_l1) && !interaction_l1 %in% prep$valid) interaction_l1 <- NULL
  if (!is.null(interaction_l2) && !interaction_l2 %in% prep$valid) interaction_l2 <- NULL

  keep <- unique(c(outcome, cluster, l1, l2, random_slope, interaction_l1, interaction_l2))
  keep <- keep[!is.na(keep) & nzchar(keep)]
  dat <- dat[, keep, drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  dat[[cluster]] <- droplevels(factor(dat[[cluster]]))

  if (nrow(dat) < 10) stop("Terlalu sedikit complete cases untuk HLM.")

  diag2 <- hlm_cluster_diagnostics(dat, cluster)
  if (!isTRUE(diag2$suitable)) {
    stop(paste0("Setelah complete-case filtering, cluster tidak lagi memadai: ", diag2$reason))
  }

  ctrl <- hlm_control()

  null_formula <- stats::as.formula(sprintf("%s ~ 1 + (1 | %s)", outcome, cluster))
  full_formula <- build_hlm_formula(
    outcome, c(l1, l2), cluster,
    random_slope, interaction_l1, interaction_l2
  )

  m0 <- lmerTest::lmer(null_formula, data = dat, REML = reml, control = ctrl)
  mf <- lmerTest::lmer(full_formula, data = dat, REML = reml, control = ctrl)

  vc0 <- as.data.frame(lme4::VarCorr(m0))
  between <- vc0$vcov[vc0$grp == cluster][1]
  within <- vc0$vcov[vc0$grp == "Residual"][1]
  icc <- between / (between + within)

  fit_tbl <- rbind(
    extract_fit(m0, "Model 0: Null", compute_r2 = FALSE),
    extract_fit(mf, "Model 3: Full", compute_r2 = TRUE)
  )
  rownames(fit_tbl) <- NULL

  re_full <- lme4::ranef(mf, condVar = FALSE)[[cluster]]
  blup <- as.data.frame(re_full)
  blup$Cluster <- rownames(re_full)
  rownames(blup) <- NULL
  names(blup)[1] <- "Random_Intercept"
  blup$Random_Intercept_SE <- ara_hlm_conditional_sd_ri(mf, cluster, blup$Cluster)

  list(
    data = dat,
    models = list(
      `Model 0: Null` = m0,
      `Model 1: Level-1` = NULL,
      `Model 2: Level-2` = NULL,
      `Model 3: Full` = mf
    ),
    formulas = list(
      `Model 0: Null` = null_formula,
      `Model 3: Full` = full_formula
    ),
    null_model = m0,
    full_model = mf,
    full_formula = full_formula,
    icc = icc, between = between, within = within,
    fixed = extract_fixed_table(mf),
    random = as.data.frame(lme4::VarCorr(mf)),
    fit = fit_tbl,
    comparison = NULL,
    comparison_note = "Mode Cepat: model Level-1 dan Level-2 belum dihitung. Klik 'Hitung Model Berjenjang Lengkap' bila diperlukan.",
    blup = blup,
    n = nrow(dat),
    clusters = nlevels(dat[[cluster]]),
    skipped_predictors = skipped_predictors,
    singular = lme4::isSingular(mf, tol = 1e-4),
    outcome = outcome,
    cluster_var = cluster,
    l1_vars = l1,
    l2_vars = l2,
    reml = reml,
    fast_mode = TRUE,
    cluster_diagnostics = diag2
  )
}

complete_hlm_incremental_models <- function(result) {
  if (is.null(result) || is.null(result$data)) stop("Hasil HLM utama belum tersedia.")

  dat <- result$data
  outcome <- result$outcome
  cluster <- result$cluster_var
  l1 <- result$l1_vars %||% character(0)
  l2 <- result$l2_vars %||% character(0)
  ctrl <- hlm_control()

  if (length(l1)) {
    f1 <- build_hlm_formula(outcome, l1, cluster)
    result$models[["Model 1: Level-1"]] <- lmerTest::lmer(
      f1, data = dat, REML = result$reml, control = ctrl
    )
    result$formulas[["Model 1: Level-1"]] <- f1
  }

  if (length(l2)) {
    f2 <- build_hlm_formula(outcome, l2, cluster)
    result$models[["Model 2: Level-2"]] <- lmerTest::lmer(
      f2, data = dat, REML = result$reml, control = ctrl
    )
    result$formulas[["Model 2: Level-2"]] <- f2
  }

  order_names <- c("Model 0: Null", "Model 1: Level-1", "Model 2: Level-2", "Model 3: Full")
  rows <- list()
  for (nm in order_names) {
    m <- result$models[[nm]]
    if (!is.null(m)) rows[[nm]] <- extract_fit(m, nm, compute_r2 = identical(nm, "Model 3: Full"))
  }
  result$fit <- do.call(rbind, rows)
  rownames(result$fit) <- NULL
  result$fast_mode <- FALSE
  result$comparison_note <- if (isTRUE(result$reml)) {
    "REML aktif: p-value LRT fixed-effects tidak dihitung."
  } else {
    NULL
  }
  result
}


# Robustness checks run only when requested; all reuse the original model formula.
# These checks are observational sensitivity analyses, not hypothesis tests.
# Refits are isolated by scenario. Large-cluster calculations use the sparse
# lme4 implementation without lmerTest's additional covariance/df machinery.
hlm_sensitivity_analysis <- function(result, min_cluster_n = 5L,
                                     require_within_variance = TRUE,
                                     drop_random_slope = FALSE) {
  if (is.null(result$full_model) || is.null(result$data)) stop("Jalankan HLM utama terlebih dahulu.")
  dat <- result$data
  cluster <- result$cluster_var
  if (!cluster %in% names(dat)) stop("ID cluster tidak ditemukan dalam data model.")
  min_cluster_n <- suppressWarnings(as.integer(min_cluster_n))
  if (length(min_cluster_n) != 1L || is.na(min_cluster_n) || min_cluster_n < 2L)
    stop("Minimum observasi per cluster harus >= 2.")
  ids <- droplevels(factor(dat[[cluster]]))
  if (anyNA(ids)) stop("ID cluster kosong pada data HLM.")
  index <- as.integer(ids)
  sizes <- tabulate(index, nbins = nlevels(ids))
  min_mask <- sizes[index] >= min_cluster_n
  scenarios <- list(`Cluster n minimum` = min_mask)
  model <- result$full_model
  formula_main <- stats::formula(model)
  bars <- lme4::findbars(formula_main)
  # Within-cluster variance is relevant to the slope tested, rather than
  # requiring EVERY numeric predictor to vary in every retained cluster.
  slope_terms <- unique(unlist(lapply(bars, function(b) {
    setdiff(all.vars(b[[2L]]), "1")
  })))
  numeric_vars <- intersect(slope_terms, names(dat))
  numeric_vars <- numeric_vars[vapply(dat[numeric_vars], is.numeric, logical(1))]
  if (isTRUE(require_within_variance)) {
    if (!length(numeric_vars)) {
      # No random slope: the within variation restriction is not applicable.
      within_status <- "Dilewati: model tidak memiliki random slope numerik"
    } else {
      good <- rep(TRUE, nlevels(ids))
      for (v in numeric_vars) {
        z <- dat[[v]]
        valid <- is.finite(z)
        if (!all(valid)) {
          good <- good & FALSE  # Model data should be complete; avoid partial invalid groups.
        } else {
          means <- as.numeric(rowsum(z, index, reorder = TRUE)) / sizes
          dev <- z - means[index]
          ss <- as.numeric(rowsum(dev * dev, index, reorder = TRUE))
          good <- good & is.finite(ss) & ss > (1e-10 * pmax(1, abs(means)))^2 * sizes
        }
      }
      scenarios[["Variasi within-cluster"]] <- good[index]
      scenarios[["n minimum + variasi within"]] <- min_mask & good[index]
      within_status <- NULL
    }
  } else within_status <- NULL

  base_coef <- lme4::fixef(model)
  output_rows <- list()
  statuses <- list()
  add_status <- function(name, message) {
    statuses[[length(statuses) + 1L]] <<- data.frame(
      Skenario = name, Status = message, stringsAsFactors = FALSE)
  }
  summarize_model <- function(m, name, n_obs, n_groups) {
    cf <- lme4::fixef(m)
    # Limit covariance extraction to a small fixed-effects covariance matrix.
    se <- tryCatch(sqrt(diag(stats::vcov(m))),
                   error = function(e) rep(NA_real_, length(cf)))
    if (is.null(names(se))) names(se) <- names(cf)
    if (!all(names(cf) %in% names(se))) stop("SE sensitivitas tidak mencakup seluruh parameter")
    if (any(!is.finite(se[names(cf)])) || any(se[names(cf)] <= 0))
      stop("SE sensitivitas tidak valid; skenario tidak boleh diekspor sebagai berhasil")
    data.frame(Skenario = name, Observasi = n_obs, Cluster = n_groups,
               Parameter = names(cf), Estimate = unname(cf),
               SE = unname(se[names(cf)]),
               Perubahan_dari_Utama = unname(cf - base_coef[names(cf)]),
               Singular = lme4::isSingular(m),
               AIC = as.numeric(stats::AIC(m)),
               Status = "OK", check.names = FALSE, stringsAsFactors = FALSE)
  }
  # If the original model summary is not available, keep sensitivity status
  # rather than collapsing the entire analysis into one notification.
  tryCatch({
    output_rows[[length(output_rows) + 1L]] <- summarize_model(
      model, "Model utama", nrow(dat), nlevels(ids))
  }, error = function(e) add_status("Model utama", paste("Ringkasan gagal:", conditionMessage(e))))
  if (!is.null(within_status)) add_status("Variasi within-cluster", within_status)
  # Avoid rebuilding a massive object inside lmerTest on the worker.
  refit <- function(form, dd) {
    lme4::lmer(form, data = dd, REML = isTRUE(result$reml),
               control = hlm_control())
  }
  run_one <- function(name, mask, form = formula_main) {
    n_obs <- sum(mask)
    n_groups <- sum(tabulate(index[mask], nbins = nlevels(ids)) > 0L)
    if (n_obs < 10L || n_groups < 2L) {
      add_status(name, "Dilewati: jumlah observasi atau cluster tidak cukup")
      return(invisible(NULL))
    }
    if (all(mask) && identical(form, formula_main)) {
      add_status(name, "Identik dengan model utama: refit tidak diperlukan")
      return(invisible(NULL))
    }
    # A large crossed random-effect design or heavy random-slope design can
    # demand a dense intermediate array above the R integer-vector limit.
    # Do not execute a known unsafe refit; record the skipped scenario.
    n_re <- length(lme4::findbars(form))
    if (n_re > 1L && n_groups > 10000L) {
      add_status(name, "Dilewati untuk keamanan memori: beberapa random effect pada >10.000 cluster")
      return(invisible(NULL))
    }
    tryCatch({
      dd <- dat[mask, , drop = FALSE]
      dd[[cluster]] <- droplevels(factor(dd[[cluster]]))
      m <- refit(form, dd)
      output_rows[[length(output_rows) + 1L]] <<- summarize_model(m, name, nrow(dd), nlevels(dd[[cluster]]))
      rm(m, dd)
      invisible(gc(verbose = FALSE))
      add_status(name, "OK")
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("long vectors not supported", msg, fixed = TRUE))
        msg <- paste0(msg, " (batas internal matriks R/Matrix; model tidak berhasil diestimasi)")
      add_status(name, paste("Refit gagal:", msg))
    })
    invisible(NULL)
  }
  for (name in names(scenarios)) run_one(name, scenarios[[name]])
  if (isTRUE(drop_random_slope)) {
    # The app currently fits one grouping factor. A random slope contributes
    # non-intercept terms to the conditional random-effects expression.
    has_slope <- any(vapply(bars, function(b) {
      length(setdiff(all.vars(b[[2L]]), "1")) > 0L
    }, logical(1)))
    if (!has_slope) {
      add_status("Random intercept saja", "Dilewati: model utama sudah random intercept")
    } else {
      tryCatch({
        f <- lme4::nobars(formula_main)
        f <- stats::as.formula(paste(deparse(f), collapse = ""))
        f <- stats::update.formula(f, paste(". ~ . + (1 |", cluster, ")"))
        run_one("Random intercept saja", rep(TRUE, nrow(dat)), f)
      }, error = function(e) add_status("Random intercept saja", paste("Formula gagal:", conditionMessage(e))))
    }
  }
  coef <- if (length(output_rows)) do.call(rbind, output_rows) else
    data.frame(Skenario=character(), Observasi=integer(), Cluster=integer(),
               Parameter=character(), Estimate=numeric(), SE=numeric(),
               Perubahan_dari_Utama=numeric(), Singular=logical(),
               AIC=numeric(), Status=character())
  rownames(coef) <- NULL
  stat <- if (length(statuses)) do.call(rbind, statuses) else
    data.frame(Skenario=character(), Status=character())
  rownames(stat) <- NULL
  list(coefficients = coef, status = stat,
       note = paste("Sensitivitas terhadap pembatasan cluster dan struktur random effects.",
                    "Perbandingan AIC pada subset data berbeda tidak valid.",
                    "Skenario gagal/dilewati tidak boleh ditafsirkan sebagai bukti ketahanan hasil."))
}
