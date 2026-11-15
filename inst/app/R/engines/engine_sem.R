
freeze_cfa_result <- function(result) {
  result <- ensure_cfa_output_tables(result)

  list(
    fit_measures = as.data.frame(result$fit_measures, stringsAsFactors = FALSE),
    loadings = as.data.frame(result$loadings, stringsAsFactors = FALSE),
    modification_indices = as.data.frame(result$modification_indices, stringsAsFactors = FALSE),
    r2_table = as.data.frame(result$r2_table, stringsAsFactors = FALSE),
    parameters = as.data.frame(result$parameters, stringsAsFactors = FALSE),
    syntax = result$syntax,
    estimator = result$estimator,
    missing = result$missing,
    ordered = result$ordered,
    summary_text = "Hasil disajikan sebagai tabel terstruktur.",
    result_type = "CFA"
  )
}

freeze_sem_result <- function(result) {
  result <- ensure_sem_output_tables(result)

  list(
    fit_measures = as.data.frame(result$fit_measures, stringsAsFactors = FALSE),
    paths = as.data.frame(result$paths, stringsAsFactors = FALSE),
    loadings = as.data.frame(result$loadings, stringsAsFactors = FALSE),
    modification_indices = as.data.frame(result$modification_indices, stringsAsFactors = FALSE),
    r2_table = as.data.frame(result$r2_table, stringsAsFactors = FALSE),
    parameters = as.data.frame(result$parameters, stringsAsFactors = FALSE),
    syntax = result$syntax,
    estimator = result$estimator,
    missing = result$missing,
    ordered = result$ordered,
    bootstrap_applied = result$bootstrap_applied,
    summary_text = "Hasil disajikan sebagai tabel terstruktur.",
    result_type = "SEM"
  )
}


ensure_cfa_output_tables <- function(result) {
  fit <- result$fit
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)

  result$loadings <- sem_clean_table(
    pe[
      pe$op == "=~",
      intersect(
        c("lhs","op","rhs","est","se","z","pvalue","ci.lower","ci.upper","std.lv","std.all"),
        names(pe)
      ),
      drop = FALSE
    ]
  )

  result$modification_indices <- sem_modification_indices(fit, 50)
  result$r2_table <- sem_r2_table(fit)
  result$parameters <- sem_clean_table(pe)
  result
}

ensure_sem_output_tables <- function(result) {
  fit <- result$fit
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)

  result$loadings <- sem_clean_table(
    pe[
      pe$op == "=~",
      intersect(
        c("lhs","op","rhs","est","se","z","pvalue","ci.lower","ci.upper","std.lv","std.all"),
        names(pe)
      ),
      drop = FALSE
    ]
  )

  result$paths <- sem_clean_table(
    pe[
      pe$op %in% c("~", ":="),
      intersect(
        c("lhs","op","rhs","est","se","z","pvalue","ci.lower","ci.upper","std.lv","std.all"),
        names(pe)
      ),
      drop = FALSE
    ]
  )

  result$modification_indices <- sem_modification_indices(fit, 50)
  result$r2_table <- sem_r2_table(fit)
  result$parameters <- sem_clean_table(pe)
  result
}


sem_clean_table <- function(x, digits = NULL) {
  # Display formatting belongs in UI/Excel, never in the statistical result.
  # In particular p-values, SE, fit indices and CIs must not be rounded
  # before an export is constructed.
  if (is.null(x)) return(data.frame())
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

sem_modification_indices <- function(fit, n = 30) {
  mi <- tryCatch(
    lavaan::modindices(fit, standardized = TRUE),
    error = function(e) NULL
  )

  if (is.null(mi) || !nrow(mi)) {
    return(data.frame(
      lhs = character(0), op = character(0), rhs = character(0),
      mi = numeric(0), epc = numeric(0),
      sepc.lv = numeric(0), sepc.all = numeric(0)
    ))
  }

  keep <- intersect(
    c("lhs", "op", "rhs", "mi", "epc", "sepc.lv", "sepc.all", "sepc.nox"),
    names(mi)
  )
  mi <- mi[, keep, drop = FALSE]
  mi <- mi[order(mi$mi, decreasing = TRUE), , drop = FALSE]
  head(sem_clean_table(mi), n)
}

sem_r2_table <- function(fit) {
  r2 <- tryCatch(lavaan::inspect(fit, "r2"), error = function(e) NULL)
  if (is.null(r2) || !length(r2)) {
    return(data.frame(Variabel = character(0), R2 = numeric(0)))
  }
  sem_clean_table(data.frame(
    Variabel = names(r2),
    R2 = as.numeric(r2),
    stringsAsFactors = FALSE
  ))
}


lavaan_fit_ok <- function(fit, label = "Model") {
  conv <- tryCatch(isTRUE(lavaan::lavInspect(fit, "converged")), error = function(e) FALSE)
  if (!conv) stop(label, " tidak konvergen. Periksa model, variabel, skala, atau estimator.")
  invisible(TRUE)
}

validate_sem_data <- function(data, ordered = NULL) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  if (!nrow(data)) stop("Dataset kosong.")
  if (ncol(data) < 2) stop("Dataset memiliki terlalu sedikit variabel.")

  ordered <- ordered %||% character(0)
  missing <- setdiff(ordered, names(data))
  if (length(missing)) stop("Ordinal indicator tidak ditemukan: ", paste(missing, collapse = ", "))

  data
}

normalize_lavaan_options <- function(estimator = "MLR", missing = "fiml", ordered = NULL) {
  estimator <- toupper(estimator)
  ordered <- ordered[!is.na(ordered) & nzchar(ordered)]

  if (estimator == "WLSMV") {
    missing <- "listwise"
  }

  list(
    estimator = estimator,
    missing = missing,
    ordered = if (length(ordered)) ordered else NULL
  )
}

fit_cfa_model <- function(data, model_syntax,
                          estimator = "MLR",
                          std_lv = TRUE,
                          missing = "fiml",
                          ordered = NULL) {
  if (!nzchar(model_syntax)) stop("Syntax CFA belum diisi.")
  data <- validate_sem_data(data, ordered)

  opt <- normalize_lavaan_options(estimator, missing, ordered)

  fit <- lavaan::cfa(
    model = model_syntax,
    data = data,
    estimator = opt$estimator,
    std.lv = std_lv,
    missing = opt$missing,
    ordered = opt$ordered
  )

  fm <- lavaan::fitMeasures(
    fit,
    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
      "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic")
  )

  lavaan_fit_ok(fit, "CFA")

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  loadings <- pe[
    pe$op == "=~",
    c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")
  ]

  r2 <- tryCatch(lavaan::inspect(fit, "r2"), error = function(e) NULL)

  list(
    fit = fit,
    fit_measures = sem_clean_table(data.frame(
      Measure = names(fm),
      Value = as.numeric(fm),
      stringsAsFactors = FALSE
    )),
    loadings = sem_clean_table(loadings),
    modification_indices = sem_modification_indices(fit, 30),
    r2 = r2,
    r2_table = sem_r2_table(fit),
    parameters = sem_clean_table(pe),
    syntax = model_syntax,
    estimator = opt$estimator,
    missing = opt$missing,
    ordered = opt$ordered
  )
}

fit_sem_model <- function(data, model_syntax,
                          estimator = "MLR",
                          missing = "fiml",
                          bootstrap = 0,
                          ordered = NULL) {
  if (!nzchar(model_syntax)) stop("Syntax SEM belum diisi.")
  data <- validate_sem_data(data, ordered)

  opt <- normalize_lavaan_options(estimator, missing, ordered)

  args <- list(
    model = model_syntax,
    data = data,
    estimator = opt$estimator,
    missing = opt$missing,
    ordered = opt$ordered
  )

  if (bootstrap > 0 && opt$estimator == "ML") {
    args$se <- "bootstrap"
    args$bootstrap <- bootstrap
  }

  bootstrap_applied <- bootstrap > 0 && opt$estimator == "ML"

  fit <- do.call(lavaan::sem, args)
  lavaan_fit_ok(fit, "SEM")

  fm <- lavaan::fitMeasures(
    fit,
    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
      "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic")
  )

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  paths <- pe[
    pe$op %in% c("~", ":="),
    c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")
  ]

  loadings <- pe[
    pe$op == "=~",
    c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")
  ]

  list(
    fit = fit,
    fit_measures = sem_clean_table(data.frame(
      Measure = names(fm),
      Value = as.numeric(fm),
      stringsAsFactors = FALSE
    )),
    paths = sem_clean_table(paths),
    loadings = sem_clean_table(loadings),
    modification_indices = sem_modification_indices(fit, 30),
    r2_table = sem_r2_table(fit),
    parameters = sem_clean_table(pe),
    syntax = model_syntax,
    estimator = opt$estimator,
    missing = opt$missing,
    ordered = opt$ordered,
    bootstrap_applied = bootstrap_applied
  )
}

export_sem_word <- function(result, file, title = "SEM / CFA Results",
                            type = c("CFA", "SEM"), provenance = NULL) {
  type <- match.arg(type)

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")

  if (!is.null(provenance)) doc <- add_provenance_to_docx(doc, provenance)

  doc <- officer::body_add_par(
    doc,
    paste0("Estimator: ", result$estimator, "; missing-data handling: ", result$missing, "."),
    style = "Normal"
  )

  doc <- officer::body_add_par(doc, "Model Fit", style = "heading 2")
  ft_fit <- flextable::flextable(result$fit_measures)
  ft_fit <- flextable::autofit(ft_fit)
  doc <- flextable::body_add_flextable(doc, ft_fit)

  if (type == "CFA") {
    doc <- officer::body_add_par(doc, "Factor Loadings", style = "heading 2")
    ft <- flextable::flextable(result$loadings)
  } else {
    doc <- officer::body_add_par(doc, "Structural Paths", style = "heading 2")
    ft <- flextable::flextable(result$paths)
  }

  ft <- flextable::autofit(ft)
  doc <- flextable::body_add_flextable(doc, ft)

  doc <- officer::body_add_par(doc, "Model Syntax", style = "heading 2")
  doc <- officer::body_add_par(doc, result$syntax, style = "Normal")

  print(doc, target = file)
}

fit_multigroup_sem <- function(data, model_syntax, group_var,
                               estimator = "MLR",
                               missing = "fiml",
                               ordered = NULL) {
  data <- validate_sem_data(data, ordered)
  if (!group_var %in% names(data)) stop("Grouping variable tidak ditemukan.")
  if (length(unique(stats::na.omit(data[[group_var]]))) < 2) {
    stop("Grouping variable harus memiliki minimal dua kelompok.")
  }

  opt <- normalize_lavaan_options(estimator, missing, ordered)

  fit <- lavaan::sem(
    model = model_syntax,
    data = data,
    group = group_var,
    estimator = opt$estimator,
    missing = opt$missing,
    ordered = opt$ordered
  )

  lavaan_fit_ok(fit, "Multi-Group SEM")

  fm <- lavaan::fitMeasures(
    fit,
    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
  )

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)

  list(
    fit = fit,
    fit_measures = data.frame(
      Measure = names(fm),
      Value = as.numeric(fm),
      stringsAsFactors = FALSE
    ),
    parameters = pe,
    syntax = model_syntax,
    group_var = group_var,
    estimator = opt$estimator,
    missing = opt$missing,
    ordered = opt$ordered
  )
}

fit_measurement_invariance <- function(data, model_syntax, group_var,
                                       estimator = "MLR",
                                       missing = "fiml",
                                       ordered = NULL) {
  data <- validate_sem_data(data, ordered)
  if (!group_var %in% names(data)) stop("Grouping variable tidak ditemukan.")
  if (length(unique(stats::na.omit(data[[group_var]]))) < 2) {
    stop("Grouping variable harus memiliki minimal dua kelompok.")
  }

  opt <- normalize_lavaan_options(estimator, missing, ordered)

  scalar_equal <- if (opt$estimator == "WLSMV" && length(opt$ordered)) {
    c("loadings", "thresholds")
  } else {
    c("loadings", "intercepts")
  }

  strict_equal <- unique(c(scalar_equal, "residuals"))

  configural <- lavaan::cfa(
    model_syntax, data = data, group = group_var,
    estimator = opt$estimator, missing = opt$missing, ordered = opt$ordered
  )
  metric <- lavaan::cfa(
    model_syntax, data = data, group = group_var,
    group.equal = c("loadings"),
    estimator = opt$estimator, missing = opt$missing, ordered = opt$ordered
  )
  scalar <- lavaan::cfa(
    model_syntax, data = data, group = group_var,
    group.equal = scalar_equal,
    estimator = opt$estimator, missing = opt$missing, ordered = opt$ordered
  )
  strict <- lavaan::cfa(
    model_syntax, data = data, group = group_var,
    group.equal = strict_equal,
    estimator = opt$estimator, missing = opt$missing, ordered = opt$ordered
  )

  lavaan_fit_ok(configural, "Configural invariance")
  lavaan_fit_ok(metric, "Metric invariance")
  lavaan_fit_ok(scalar, "Scalar invariance")
  lavaan_fit_ok(strict, "Strict invariance")

  mods <- list(
    Configural = configural,
    Metric = metric,
    Scalar = scalar,
    Strict = strict
  )

  tbl <- do.call(rbind, lapply(names(mods), function(nm) {
    f <- mods[[nm]]
    fm <- lavaan::fitMeasures(f, c("chisq", "df", "cfi", "rmsea", "srmr", "aic", "bic"))
    data.frame(
      Model = nm,
      ChiSquare = fm["chisq"],
      df = fm["df"],
      CFI = fm["cfi"],
      RMSEA = fm["rmsea"],
      SRMR = fm["srmr"],
      AIC = fm["aic"],
      BIC = fm["bic"],
      stringsAsFactors = FALSE
    )
  }))

  rownames(tbl) <- NULL
  tbl$Delta_CFI <- c(NA, diff(tbl$CFI))
  tbl$Delta_RMSEA <- c(NA, diff(tbl$RMSEA))
  tbl$Delta_SRMR <- c(NA, diff(tbl$SRMR))

  list(
    models = mods,
    table = tbl,
    group_var = group_var,
    syntax = model_syntax,
    estimator = opt$estimator,
    missing = opt$missing,
    ordered = opt$ordered
  )
}


freeze_cfa_result_fast <- function(result) {
  pe <- lavaan::parameterEstimates(result$fit, standardized = TRUE, ci = TRUE)

  loadings <- pe[
    pe$op == "=~",
    intersect(
      c("lhs","op","rhs","est","se","z","pvalue","ci.lower","ci.upper","std.lv","std.all"),
      names(pe)
    ),
    drop = FALSE
  ]

  list(
    fit_measures = as.data.frame(result$fit_measures, stringsAsFactors = FALSE),
    loadings = sem_clean_table(loadings),
    modification_indices = data.frame(),
    r2_table = sem_r2_table(result$fit),
    parameters = data.frame(),
    syntax = result$syntax,
    estimator = result$estimator,
    missing = result$missing,
    ordered = result$ordered,
    summary_text = paste(
      capture.output(
        summary(result$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
      ),
      collapse = "\n"
    ),
    diagnostics_computed = FALSE,
    result_type = "CFA"
  )
}

freeze_sem_result_fast <- function(result) {
  pe <- lavaan::parameterEstimates(result$fit, standardized = TRUE, ci = TRUE)

  paths <- pe[
    pe$op %in% c("~", ":="),
    intersect(
      c("lhs","op","rhs","est","se","z","pvalue","ci.lower","ci.upper","std.lv","std.all"),
      names(pe)
    ),
    drop = FALSE
  ]

  loadings <- pe[
    pe$op == "=~",
    intersect(
      c("lhs","op","rhs","est","se","z","pvalue","ci.lower","ci.upper","std.lv","std.all"),
      names(pe)
    ),
    drop = FALSE
  ]

  list(
    fit_measures = as.data.frame(result$fit_measures, stringsAsFactors = FALSE),
    paths = sem_clean_table(paths),
    loadings = sem_clean_table(loadings),
    modification_indices = data.frame(),
    r2_table = sem_r2_table(result$fit),
    parameters = data.frame(),
    syntax = result$syntax,
    estimator = result$estimator,
    missing = result$missing,
    ordered = result$ordered,
    bootstrap_applied = result$bootstrap_applied,
    summary_text = paste(
      capture.output(
        summary(result$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
      ),
      collapse = "\n"
    ),
    diagnostics_computed = FALSE,
    result_type = "SEM"
  )
}

compute_cfa_diagnostics_from_data <- function(data, model_syntax, estimator, missing, ordered = NULL) {
  fit <- fit_cfa_model(
    data = data,
    model_syntax = model_syntax,
    estimator = estimator,
    missing = missing,
    ordered = ordered
  )

  pe <- lavaan::parameterEstimates(fit$fit, standardized = TRUE, ci = TRUE)

  list(
    modification_indices = sem_modification_indices(fit$fit, 50),
    parameters = sem_clean_table(pe),
    diagnostics_computed = TRUE
  )
}

compute_sem_diagnostics_from_data <- function(data, model_syntax, estimator, bootstrap = 0, ordered = NULL) {
  fit <- fit_sem_model(
    data = data,
    model_syntax = model_syntax,
    estimator = estimator,
    bootstrap = bootstrap,
    ordered = ordered
  )

  pe <- lavaan::parameterEstimates(fit$fit, standardized = TRUE, ci = TRUE)

  list(
    modification_indices = sem_modification_indices(fit$fit, 50),
    parameters = sem_clean_table(pe),
    diagnostics_computed = TRUE
  )
}

freeze_multigroup_result_fast <- function(result) {
  list(
    fit_measures = as.data.frame(result$fit_measures, stringsAsFactors = FALSE),
    parameters = if (!is.null(result$parameters))
      sem_clean_table(result$parameters) else data.frame(),
    syntax = result$syntax,
    group_var = result$group_var,
    estimator = result$estimator,
    ordered = result$ordered,
    group_counts = result$group_counts %||% data.frame(),
    fast_mode = isTRUE(result$fast_mode),
    result_type = "MG"
  )
}


sem_observed_vars <- function(model_syntax) {
  pt <- lavaan::lavaanify(model_syntax, auto = TRUE, fixed.x = FALSE)
  obs <- tryCatch(
    lavaan::lavNames(pt, type = "ov"),
    error = function(e) character(0)
  )

  if (!length(obs)) {
    # fallback for measurement models
    obs <- unique(pt$rhs[pt$op == "=~"])
  }

  unique(obs[nzchar(obs)])
}

multigroup_group_info <- function(data, group_var) {
  if (!group_var %in% names(data)) {
    stop("Grouping variable tidak ditemukan.")
  }

  g <- data[[group_var]]
  tab <- sort(table(g, useNA = "no"), decreasing = TRUE)

  list(
    n_groups = length(tab),
    counts = data.frame(
      Group = names(tab),
      N = as.integer(tab),
      stringsAsFactors = FALSE
    )
  )
}

fit_multigroup_sem_fast <- function(data, model_syntax, group_var,
                                    max_groups = 10,
                                    min_group_n = 30) {
  if (!nzchar(model_syntax)) stop("Sintaks Multi-Group SEM belum diisi.")
  if (!group_var %in% names(data)) stop("Grouping variable tidak ditemukan.")

  info <- multigroup_group_info(data, group_var)

  if (info$n_groups < 2) {
    stop("Grouping variable harus memiliki minimal dua kelompok.")
  }

  if (info$n_groups > max_groups) {
    stop(
      paste0(
        "Variabel grouping memiliki ", info$n_groups, " kelompok. ",
        "Multi-Group SEM dirancang untuk sejumlah kecil kelompok kategorikal. ",
        "Gunakan variabel dengan maksimal ", max_groups,
        " kelompok (misalnya status sekolah, wilayah kategori, atau kelompok eksperimen), ",
        "bukan ID sekolah/kabupaten."
      )
    )
  }

  too_small <- info$counts$Group[info$counts$N < min_group_n]
  if (length(too_small)) {
    stop(
      paste0(
        "Ada kelompok dengan N < ", min_group_n, ": ",
        paste(head(too_small, 8), collapse = ", "),
        if (length(too_small) > 8) " ..." else "",
        ". Multi-Group SEM dihentikan agar estimasi tidak tidak stabil."
      )
    )
  }

  ov <- sem_observed_vars(model_syntax)
  if (length(ov) < 2) stop("Variabel observasi tidak dapat diidentifikasi dari sintaks.")

  missing_vars <- setdiff(ov, names(data))
  if (length(missing_vars)) {
    stop(
      paste0(
        "Variabel model tidak ditemukan: ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }

  # Fast mode uses complete cases for the variables involved in the model.
  # This lets lavaan estimate from group covariance matrices instead of all raw rows.
  dat <- data[, unique(c(group_var, ov)), drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]

  if (!nrow(dat)) stop("Tidak ada complete cases untuk model Multi-Group.")

  spl <- split(dat[, ov, drop = FALSE], dat[[group_var]], drop = TRUE)

  # Keep deterministic order and ensure all remaining groups are usable.
  keep <- vapply(
    spl,
    function(d) nrow(d) >= min_group_n &&
      all(vapply(d, is.numeric, logical(1))) &&
      all(vapply(d, function(x) stats::sd(x) > 0, logical(1))),
    logical(1)
  )

  if (!all(keep)) {
    bad <- names(spl)[!keep]
    stop(
      paste0(
        "Kelompok berikut tidak memenuhi syarat setelah complete-case filtering: ",
        paste(head(bad, 8), collapse = ", "),
        if (length(bad) > 8) " ..." else ""
      )
    )
  }

  sample_cov <- lapply(spl, stats::cov)
  sample_mean <- lapply(spl, colMeans)
  sample_nobs <- vapply(spl, nrow, integer(1))
  group_labels <- names(spl)

  fit <- lavaan::sem(
    model = model_syntax,
    sample.cov = sample_cov,
    sample.mean = sample_mean,
    sample.nobs = sample_nobs,
    group.label = group_labels,
    estimator = "ML",
    meanstructure = TRUE
  )

  lavaan_fit_ok(fit, "Multi-Group SEM Fast")

  fm <- lavaan::fitMeasures(
    fit,
    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
  )

  pe <- lavaan::parameterEstimates(
    fit,
    standardized = TRUE,
    ci = TRUE
  )

  list(
    fit = fit,
    fit_measures = data.frame(
      Measure = names(fm),
      Value = as.numeric(fm),
      stringsAsFactors = FALSE
    ),
    parameters = pe,
    group_counts = data.frame(
      Group = group_labels,
      N = as.integer(sample_nobs),
      stringsAsFactors = FALSE
    ),
    syntax = model_syntax,
    group_var = group_var,
    estimator = "ML",
    missing = "complete-case covariance",
    ordered = NULL,
    fast_mode = TRUE
  )
}


fit_measurement_invariance_fast <- function(data, model_syntax, group_var,
                                            level = "standard",
                                            max_groups = 10,
                                            min_group_n = 30) {
  if (!nzchar(model_syntax)) stop("Sintaks CFA belum diisi.")
  if (!group_var %in% names(data)) stop("Grouping variable tidak ditemukan.")

  info <- multigroup_group_info(data, group_var)

  if (info$n_groups < 2) {
    stop("Grouping variable harus memiliki minimal dua kelompok.")
  }

  if (info$n_groups > max_groups) {
    stop(
      paste0(
        "Variabel grouping memiliki ", info$n_groups, " kelompok. ",
        "Measurement Invariance dirancang untuk sejumlah kecil kelompok kategorikal. ",
        "Gunakan maksimal ", max_groups, " kelompok."
      )
    )
  }

  too_small <- info$counts$Group[info$counts$N < min_group_n]
  if (length(too_small)) {
    stop(
      paste0(
        "Ada kelompok dengan N < ", min_group_n, ": ",
        paste(head(too_small, 8), collapse = ", "),
        if (length(too_small) > 8) " ..." else "",
        ". Analisis dihentikan untuk menjaga stabilitas estimasi."
      )
    )
  }

  ov <- sem_observed_vars(model_syntax)
  if (length(ov) < 2) stop("Variabel observasi tidak dapat diidentifikasi dari sintaks.")

  missing_vars <- setdiff(ov, names(data))
  if (length(missing_vars)) {
    stop(
      paste0(
        "Variabel model tidak ditemukan: ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }

  dat <- data[, unique(c(group_var, ov)), drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]

  if (!nrow(dat)) stop("Tidak ada complete cases untuk model Measurement Invariance.")

  spl <- split(dat[, ov, drop = FALSE], dat[[group_var]], drop = TRUE)

  keep <- vapply(
    spl,
    function(d) {
      nrow(d) >= min_group_n &&
        all(vapply(d, is.numeric, logical(1))) &&
        all(vapply(d, function(x) {
          sx <- stats::sd(x)
          is.finite(sx) && sx > 0
        }, logical(1)))
    },
    logical(1)
  )

  if (!all(keep)) {
    bad <- names(spl)[!keep]
    stop(
      paste0(
        "Kelompok berikut tidak memenuhi syarat setelah complete-case filtering: ",
        paste(head(bad, 8), collapse = ", "),
        if (length(bad) > 8) " ..." else ""
      )
    )
  }

  sample_cov <- lapply(spl, stats::cov)
  sample_mean <- lapply(spl, colMeans)
  sample_nobs <- vapply(spl, nrow, integer(1))
  group_labels <- names(spl)

  fit_one <- function(equal = character(0), label = "model") {
    fit <- lavaan::cfa(
      model = model_syntax,
      sample.cov = sample_cov,
      sample.mean = sample_mean,
      sample.nobs = sample_nobs,
      group.label = group_labels,
      estimator = "ML",
      meanstructure = TRUE,
      group.equal = equal
    )
    lavaan_fit_ok(fit, label)
    fit
  }

  configural <- fit_one(character(0), "Configural invariance")
  metric <- fit_one("loadings", "Metric invariance")
  scalar <- fit_one(c("loadings", "intercepts"), "Scalar invariance")

  mods <- list(
    Configural = configural,
    Metric = metric,
    Scalar = scalar
  )

  if (identical(level, "full")) {
    strict <- fit_one(
      c("loadings", "intercepts", "residuals"),
      "Strict invariance"
    )
    mods$Strict <- strict
  }

  tbl <- do.call(rbind, lapply(names(mods), function(nm) {
    f <- mods[[nm]]
    fm <- lavaan::fitMeasures(
      f,
      c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
    )

    data.frame(
      Model = nm,
      ChiSquare = unname(fm["chisq"]),
      df = unname(fm["df"]),
      CFI = unname(fm["cfi"]),
      TLI = unname(fm["tli"]),
      RMSEA = unname(fm["rmsea"]),
      SRMR = unname(fm["srmr"]),
      AIC = unname(fm["aic"]),
      BIC = unname(fm["bic"]),
      stringsAsFactors = FALSE
    )
  }))

  rownames(tbl) <- NULL
  tbl$Delta_CFI <- c(NA_real_, diff(tbl$CFI))
  tbl$Delta_RMSEA <- c(NA_real_, diff(tbl$RMSEA))
  tbl$Delta_SRMR <- c(NA_real_, diff(tbl$SRMR))

  list(
    table = sem_clean_table(tbl),
    group_counts = data.frame(
      Group = group_labels,
      N = as.integer(sample_nobs),
      stringsAsFactors = FALSE
    ),
    group_var = group_var,
    syntax = model_syntax,
    estimator = "ML",
    missing = "complete-case covariance",
    ordered = NULL,
    fast_mode = TRUE,
    level = level
  )
}
