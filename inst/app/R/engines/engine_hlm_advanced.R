# Advanced HLM tests. Procedures are independent and only computed on request.
# Inference is conditional on model specification; users choose which checks apply.
ara_hlm_advanced <- function(result, tests = c("diagnostics", "robust"),
                             bootstrap_n = 100L, seed = 2026L,
                             max_plot_n = 5000L) {
  if (is.null(result$full_model) || is.null(result$data))
    stop("Model HLM utama belum tersedia.")
  m <- result$full_model
  dat <- result$data
  cid <- result$cluster_var
  if (!cid %in% names(dat)) stop("Kolom cluster tidak tersedia.")
  tests <- intersect(unique(tests), c("diagnostics", "robust", "within_between", "bootstrap"))
  if (!length(tests)) stop("Pilih minimal satu uji lanjutan.")
  out <- list()
  status <- list()
  register <- function(test, state, detail) {
    status[[length(status) + 1L]] <<- data.frame(Uji = test, Status = state,
      Keterangan = substr(as.character(detail), 1, 800), stringsAsFactors = FALSE)
  }
  execute <- function(name, fun) {
    started <- proc.time()[["elapsed"]]
    tryCatch({
      x <- fun()
      out[[name]] <<- x
      elapsed <- round(proc.time()[["elapsed"]] - started, 1)
      register(name, "OK", paste0("Berhasil dihitung; waktu ", elapsed, " detik"))
    }, error = function(e) {
      elapsed <- round(proc.time()[["elapsed"]] - started, 1)
      register(name, "GAGAL", paste0(conditionMessage(e), "; setelah ", elapsed, " detik"))
    })
  }
  if ("diagnostics" %in% tests) execute("diagnostics", function() {
    r <- stats::residuals(m)
    f <- stats::fitted(m)
    good <- is.finite(r) & is.finite(f)
    rr <- r[good]; ff <- f[good]
    n <- length(rr)
    if (n < 5L) stop("Residual valid terlalu sedikit.")
    set.seed(seed)
    ind <- if (n > max_plot_n) sort(sample.int(n, max_plot_n)) else seq_len(n)
    group <- dat[[cid]]
    if (length(group) != length(r)) group <- stats::model.frame(m)[[cid]]
    # Cluster means of residuals; no dense dummy design matrix is built.
    gr <- factor(group[good]); gindex <- as.integer(gr)
    sums <- rowsum(rr, gindex, reorder = TRUE)
    counts <- tabulate(gindex, nlevels(gr))
    av <- as.numeric(sums[, 1]) / counts
    ord <- order(abs(av), decreasing = TRUE)
    k <- head(ord, 100L)
    list(
      Summary = data.frame(Metrik = c("N residual", "N cluster", "Mean residual", "SD residual",
          "RMSE", "Median |residual|", "Model singular", "Convergence messages"),
        Nilai = c(n, nlevels(gr), mean(rr), stats::sd(rr), sqrt(mean(rr^2)),
          stats::median(abs(rr)), lme4::isSingular(m),
          paste(unlist(m@optinfo$conv$lme4$messages), collapse = "; ")),
        stringsAsFactors = FALSE),
      Residual_Sample = data.frame(Fitted = ff[ind], Residual = rr[ind]),
      Cluster_Residual = data.frame(Cluster = levels(gr)[k], N = counts[k],
          Mean_Residual = av[k], stringsAsFactors = FALSE),
      Catatan = "Residual-sample dibatasi untuk visualisasi. Cluster_Residual adalah 100 cluster dengan rerata residual absolut tertinggi, bukan estimasi pengaruh kausal."
    )
  })
  if ("robust" %in% tests) execute("robust", function() {
    if (!requireNamespace("clubSandwich", quietly = TRUE))
      stop("Uji CR2 tidak dapat dijalankan karena paket clubSandwich belum terpasang. Analisis lain tetap dapat digunakan.")
    if (length(lme4::getME(m, "flist")) != 1L)
      stop("CR2 pada modul ini mendukung satu faktor pengelompokan.")
    if (length(unique(dat[[cid]])) < 3L) stop("Perlu >=3 cluster untuk inferensi CR2.")
    cv <- clubSandwich::vcovCR(m, cluster = dat[[cid]], type = "CR2")
    ct <- clubSandwich::coef_test(m, vcov = cv, test = "Satterthwaite")
    ans <- as.data.frame(ct)
    ans$Term <- rownames(ans); rownames(ans) <- NULL
    ans[, c("Term", setdiff(names(ans), "Term")), drop = FALSE]
  })
  if ("within_between" %in% tests) execute("within_between", function() {
    vars <- intersect(result$l1_vars %||% character(), names(dat))
    vars <- vars[vapply(dat[vars], is.numeric, logical(1))]
    if (!length(vars)) stop("Tidak ada prediktor numerik Level-1.")
    # Fit correlated random-effects / Mundlak decomposition with separate means.
    # Original Level-1 terms are replaced by the within terms, while Level-2
    # terms are retained. Requires fresh model, never mutates main model.
    dd <- dat
    g <- as.integer(factor(dd[[cid]]))
    counts <- tabulate(g)
    new_w <- new_b <- character()
    for (v in vars) {
      z <- dd[[v]]
      if (any(!is.finite(z))) stop("Prediktor numerik mempunyai nilai tidak valid: ", v)
      mu <- as.numeric(rowsum(z, g, reorder = TRUE)) / counts
      b <- paste0("ARA_between_", length(new_b) + 1L)
      w <- paste0("ARA_within_", length(new_w) + 1L)
      dd[[b]] <- mu[g]; dd[[w]] <- z - mu[g]
      new_b <- c(new_b, b); new_w <- c(new_w, w)
    }
    original_l2 <- intersect(result$l2_vars %||% character(), names(dd))
    rhs <- c(new_w, new_b, original_l2)
    form <- stats::reformulate(c(sprintf("`%s`", rhs), sprintf("(1 | `%s`)", cid)),
                               response = result$outcome)
    mm <- lme4::lmer(form, data = dd, REML = isTRUE(result$reml),
                     control = hlm_control())
    co <- lme4::fixef(mm)
    se <- sqrt(diag(stats::vcov(mm)))
    term <- names(co)
    mapping <- c(setNames(paste0("Within: ", vars), new_w),
                 setNames(paste0("Between: ", vars), new_b))
    label <- ifelse(term %in% names(mapping), unname(mapping[term]), term)
    list(Coefficients = data.frame(Term = term, Deskripsi = label,
           Estimate = unname(co), SE = unname(se[term]),
           Z_approx = unname(co / se[term]), row.names = NULL),
         Model = data.frame(N = stats::nobs(mm), Clusters = length(unique(dd[[cid]])),
            AIC = stats::AIC(mm), Singular = lme4::isSingular(mm)),
         Catatan = "Dekomposisi Mundlak (within dan between) dengan random intercept. Model ini berbeda dari model utama jika interaksi atau random slope dipakai; interpretasikan sebagai spesifikasi alternatif.")
  })
  if ("bootstrap" %in% tests) execute("bootstrap", function() {
    b <- suppressWarnings(as.integer(bootstrap_n))
    if (is.na(b) || b < 20L || b > 2000L) stop("Jumlah bootstrap harus 20 sampai 2000.")
    set.seed(seed)
    bt <- lme4::bootMer(m, FUN = function(fit) lme4::fixef(fit), nsim = b,
                         use.u = FALSE, type = "parametric", parallel = "no")
    ok <- apply(bt$t, 1L, function(z) all(is.finite(z)))
    if (sum(ok) < 10) stop("Bootstrap berhasil terlalu sedikit.")
    ci <- apply(bt$t[ok, , drop = FALSE], 2L, stats::quantile,
                probs = c(0.025, 0.975), na.rm = TRUE)
    data.frame(Term = names(lme4::fixef(m)), Estimate = unname(lme4::fixef(m)),
      CI_2_5 = ci[1L, ], CI_97_5 = ci[2L, ],
      Replicates_OK = sum(ok), Replicates_Requested = b, row.names = NULL)
  })
  out$Status <- do.call(rbind, status)
  rownames(out$Status) <- NULL
  out$Note <- "Uji merupakan pilihan peneliti; keberadaan menu tidak menjamin asumsi terpenuhi. CI bootstrap bersifat parametrik, bukan cluster bootstrap."
  out
}

ara_hlm_advanced_excel <- function(obj) {
  if (is.null(obj)) return(list())
  result <- list()
  for (nm in names(obj)) {
    v <- obj[[nm]]
    if (is.data.frame(v)) result[[paste0("Lanjutan_", nm)]] <- v
    else if (is.list(v)) for (sub in names(v)) {
      if (is.data.frame(v[[sub]])) result[[paste0("Adv_", nm, "_", sub)]] <- v[[sub]]
    }
  }
  result
}
