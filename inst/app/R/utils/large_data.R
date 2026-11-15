read_selected_sav <- function(path, vars = NULL) {
  # haven currently reads the file, then selected columns are retained.
  # This wrapper centralizes the interface so a more memory-efficient
  # backend can replace it later without changing the UI.
  x <- haven::read_sav(path)
  x <- as.data.frame(x)

  if (!is.null(vars) && length(vars)) {
    keep <- intersect(vars, names(x))
    x <- x[, keep, drop = FALSE]
  }
  x
}

build_country_subset <- function(data, country_var, country_value, variables = NULL) {
  if (!country_var %in% names(data)) stop("Country variable not found.")
  out <- data[data[[country_var]] == country_value, , drop = FALSE]

  if (!is.null(variables) && length(variables)) {
    variables <- unique(c(country_var, variables))
    variables <- intersect(variables, names(out))
    out <- out[, variables, drop = FALSE]
  }
  out
}

save_analysis_extract <- function(data, filename, dir = APP_DATA_DIR) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  safe <- gsub("[^A-Za-z0-9._-]+", "_", filename)
  if (!grepl("\\.rds$", safe, ignore.case = TRUE)) safe <- paste0(safe, ".rds")
  path <- file.path(dir, safe)
  saveRDS(data, path, compress = FALSE)
  path
}

detect_pisa_design <- function(df) {
  nms <- names(df)

  list(
    plausible_values = grep("^PV[0-9]+", nms, value = TRUE, ignore.case = TRUE),
    final_weights = grep("W_FSTUWT|W_FSCHWT|W_FTCHWT", nms, value = TRUE, ignore.case = TRUE),
    replicate_weights = grep("W_FSTURWT[0-9]+|W_FSTR[0-9]+", nms, value = TRUE, ignore.case = TRUE),
    school_id = grep("CNTSCHID|SCHOOLID", nms, value = TRUE, ignore.case = TRUE),
    country = grep("^CNT$|^CNTRYID$|^COUNTRY$", nms, value = TRUE, ignore.case = TRUE)
  )
}

pool_plausible_value_estimates <- function(estimates, variances) {
  if (length(estimates) != length(variances)) {
    stop("Estimates and variances must have the same length.")
  }
  m <- length(estimates)
  if (m < 2) stop("At least two plausible-value estimates are required.")

  qbar <- mean(estimates, na.rm = TRUE)
  ubar <- mean(variances, na.rm = TRUE)
  b <- stats::var(estimates, na.rm = TRUE)
  total_var <- ubar + (1 + 1/m) * b
  se <- sqrt(total_var)

  data.frame(
    Estimate = qbar,
    SamplingVariance = ubar,
    ImputationVariance = b,
    TotalVariance = total_var,
    SE = se,
    CI_low = qbar - 1.96 * se,
    CI_high = qbar + 1.96 * se,
    stringsAsFactors = FALSE
  )
}

weighted_mean_with_replicates <- function(y, final_weight, replicate_weights = NULL, fay_factor = 0.5) {
  ok <- complete.cases(y, final_weight)
  y <- y[ok]
  w <- final_weight[ok]

  theta <- sum(w * y) / sum(w)

  if (is.null(replicate_weights)) {
    return(list(estimate = theta, variance = NA_real_, se = NA_real_))
  }

  rw <- as.matrix(replicate_weights[ok, , drop = FALSE])
  reps <- apply(rw, 2, function(wr) sum(wr * y) / sum(wr))

  # BRR/Fay-style variance framework. Exact constants depend on survey design.
  # User-facing text clearly labels this as a framework, not OECD-certified replication.
  r <- ncol(rw)
  var_brr <- sum((reps - theta)^2) / (r * (1 - fay_factor)^2)

  list(
    estimate = theta,
    variance = var_brr,
    se = sqrt(var_brr),
    replicate_estimates = reps
  )
}
