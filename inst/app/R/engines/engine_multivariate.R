
normalize_manova_test <- function(model, test_name) {
  sm <- summary(model, test = test_name)
  raw <- as.data.frame(sm$stats, check.names = FALSE)
  # Residuals is the denominator, not an additional multivariate hypothesis.
  raw <- raw[trimws(rownames(raw)) != "Residuals", , drop = FALSE]

  if (ncol(raw) < 6) {
    stop("Output uji multivariat tidak memiliki struktur yang diharapkan.")
  }

  data.frame(
    Term = rownames(raw),
    Test = test_name,
    Df = raw[[1]],
    Statistic = raw[[2]],
    Approx_F = raw[[3]],
    Num_Df = raw[[4]],
    Den_Df = raw[[5]],
    p_value = raw[[6]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

validate_numeric_variables <- function(data, vars, label = "variabel") {
  missing <- setdiff(vars, names(data))
  if (length(missing)) stop("Variabel tidak ditemukan: ", paste(missing, collapse = ", "))
  bad <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(bad)) stop(label, " harus numerik: ", paste(bad, collapse = ", "))
}

prepare_complete_analysis_data <- function(data, vars) {
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

  dat <- dat[stats::complete.cases(dat), , drop = FALSE]

  for (v in names(dat)) {
    if (is.factor(dat[[v]])) dat[[v]] <- droplevels(dat[[v]])
  }

  dat
}

fit_manova_model <- function(data, outcomes, factor_var, covariates = NULL) {
  if (length(outcomes) < 2) stop("MANOVA membutuhkan minimal dua outcome.")
  validate_numeric_variables(data, outcomes, "Outcome")
  if (length(covariates)) validate_numeric_variables(data, covariates, "Kovariat")

  dat <- prepare_complete_analysis_data(
    data,
    unique(c(outcomes, factor_var, covariates))
  )

  dat[[factor_var]] <- droplevels(factor(dat[[factor_var]]))
  if (nlevels(dat[[factor_var]]) < 2)
    stop("Variabel kelompok harus memiliki minimal dua kelompok setelah missing value ditangani.")

  if (nrow(dat) <= length(outcomes) + nlevels(dat[[factor_var]]))
    stop("Jumlah kasus terlalu sedikit untuk MANOVA/MANCOVA.")

  rhs <- paste(c(factor_var, covariates), collapse = " + ")
  lhs <- paste0("cbind(", paste(outcomes, collapse = ", "), ")")
  f <- stats::as.formula(paste(lhs, "~", rhs))
  fit <- stats::manova(f, data = dat)

  tests <- lapply(
    c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
    function(test) normalize_manova_test(fit, test)
  )

  # Ringkasan deskriptif disusun dari complete-case sample yang benar-benar
  # digunakan dalam model; tidak menggunakan seluruh baris data awal.
  grp <- as.character(dat[[factor_var]])
  spl <- split(seq_len(nrow(dat)), grp, drop = TRUE)
  desc <- do.call(rbind, lapply(names(spl), function(g) {
    idx <- spl[[g]]
    do.call(rbind, lapply(outcomes, function(v) data.frame(
      Kelompok = g, Outcome = v, N = length(idx),
      Mean = mean(dat[[v]][idx]), SD = stats::sd(dat[[v]][idx]),
      stringsAsFactors = FALSE)))
  }))
  rownames(desc) <- NULL
  list(
    fit = fit,
    formula = f,
    multivariate_tests = do.call(rbind, tests),
    univariate = summary.aov(fit),
    group_descriptives = desc,
    sample_info = data.frame(N_Awal = nrow(data), N_Dianalisis = nrow(dat),
      N_Dikeluarkan = nrow(data) - nrow(dat), Jumlah_Kelompok = nlevels(dat[[factor_var]])),
    n = nrow(dat)
  )
}

fit_multivar_regression <- function(data, outcomes, predictors) {
  if (length(outcomes) < 2)
    stop("Multivariate multiple regression membutuhkan minimal dua outcome.")
  if (!length(predictors)) stop("Pilih minimal satu prediktor.")

  validate_numeric_variables(data, outcomes, "Outcome")

  dat <- prepare_complete_analysis_data(data, unique(c(outcomes, predictors)))

  # Remove categorical predictors that collapse to one level.
  usable <- character(0)
  skipped <- character(0)

  for (v in predictors) {
    x <- dat[[v]]
    if (is.factor(x) && nlevels(x) < 2) {
      skipped <- c(skipped, v)
    } else if (length(unique(x)) < 2) {
      skipped <- c(skipped, v)
    } else {
      usable <- c(usable, v)
    }
  }

  if (!length(usable)) stop("Tidak ada prediktor yang memiliki variasi cukup.")

  lhs <- paste0("cbind(", paste(outcomes, collapse = ", "), ")")
  rhs <- paste(usable, collapse = " + ")
  f <- stats::as.formula(paste(lhs, "~", rhs))

  fit <- stats::lm(f, data = dat)
  man <- stats::manova(fit)

  tests <- lapply(
    c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
    function(test) normalize_manova_test(man, test)
  )

  list(
    fit = fit,
    formula = f,
    multivariate_tests = do.call(rbind, tests),
    coefficients = summary(fit),
    n = nrow(dat),
    skipped_predictors = skipped
  )
}

fit_discriminant <- function(data, group, predictors,
                             method = c("lda", "qda"), cv = TRUE) {
  method <- match.arg(method)
  if (!length(predictors)) stop("Pilih minimal satu prediktor.")
  validate_numeric_variables(data, predictors, "Prediktor")

  dat <- prepare_complete_analysis_data(data, unique(c(group, predictors)))
  dat[[group]] <- droplevels(factor(dat[[group]]))

  if (nlevels(dat[[group]]) < 2)
    stop("Variabel kelompok harus memiliki minimal dua kelompok.")
  if (nrow(dat) <= nlevels(dat[[group]]))
    stop("Jumlah kasus terlalu sedikit untuk discriminant analysis.")

  zero_var <- predictors[vapply(dat[predictors], function(x) stats::sd(x) == 0, logical(1))]
  if (length(zero_var))
    stop("Prediktor tanpa variasi harus dikeluarkan: ", paste(zero_var, collapse = ", "))

  group_n <- table(dat[[group]])
  if (method == "qda" && any(group_n <= length(predictors)))
    stop("QDA membutuhkan lebih banyak kasus per kelompok daripada jumlah prediktor.")

  f <- stats::as.formula(paste(group, "~", paste(predictors, collapse = " + ")))

  if (method == "lda") {
    fit <- MASS::lda(f, data = dat, CV = FALSE)
    pred <- predict(fit, dat)$class
    fit_cv <- if (isTRUE(cv)) MASS::lda(f, data = dat, CV = TRUE) else NULL
    cv_class <- if (!is.null(fit_cv)) fit_cv$class else NULL
  } else {
    fit <- MASS::qda(f, data = dat, CV = FALSE)
    pred <- predict(fit, dat)$class
    fit_cv <- if (isTRUE(cv)) MASS::qda(f, data = dat, CV = TRUE) else NULL
    cv_class <- if (!is.null(fit_cv)) fit_cv$class else NULL
  }

  conf <- table(Actual = dat[[group]], Predicted = pred)
  acc <- sum(diag(conf)) / sum(conf)

  cv_conf <- NULL
  cv_acc <- NA_real_

  if (!is.null(cv_class)) {
    cv_conf <- table(Actual = dat[[group]], Predicted = cv_class)
    cv_acc <- sum(diag(cv_conf)) / sum(cv_conf)
  }

  scores <- NULL
  if (method == "lda") {
    pr <- predict(fit, dat)
    scores <- as.data.frame(pr$x)
    scores$Group <- dat[[group]]
  }

  list(
    fit = fit,
    formula = f,
    confusion = conf,
    accuracy = acc,
    cv_confusion = cv_conf,
    cv_accuracy = cv_acc,
    scores = scores,
    n = nrow(dat)
  )
}

# Memori O(n^2) pada matriks jarak tidak boleh dialokasikan untuk dataset besar.
# Jika sampling diaktifkan, semua hasil dinyatakan sebagai hasil SUBSET, bukan populasi penuh.
fit_cluster_model <- function(data, variables,
                              method = c("kmeans", "pam", "hierarchical"),
                              k = 3,
                              scale_data = TRUE,
                              distance = "euclidean",
                              linkage = "ward.D2",
                              sample_large = TRUE,
                              max_cases = 2000L,
                              sample_seed = 2026L) {
  method <- match.arg(method)
  if (length(variables) < 2) stop("Cluster analysis membutuhkan minimal dua variabel.")
  validate_numeric_variables(data, variables, "Variabel cluster")
  dat <- prepare_complete_analysis_data(data, variables)
  original_n <- nrow(dat)
  if (original_n < 5) stop("Jumlah kasus terlalu sedikit untuk cluster analysis.")
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 2L || k >= original_n)
    stop("Jumlah cluster harus >= 2 dan lebih kecil dari jumlah kasus.")

  quadratic <- method %in% c("hierarchical", "pam")
  max_cases <- suppressWarnings(as.integer(max_cases))
  if (is.na(max_cases) || max_cases < 100L || max_cases > 3000L)
    stop("Batas sampel untuk Hierarchical/PAM harus antara 100 dan 3000 kasus.")
  if (quadratic && original_n > max_cases && !isTRUE(sample_large)) {
    gib <- as.double(original_n) * (original_n - 1) / 2 * 8 / 1024^3
    stop(sprintf("%s dengan %s kasus memerlukan setidaknya %.1f GiB untuk vektor jarak saja. Aktifkan 'Gunakan sampel untuk metode berbasis jarak' atau gunakan K-Means.",
                 method, format(original_n, big.mark = "."), gib))
  }
  sampled <- quadratic && original_n > max_cases
  selected_rows <- seq_len(original_n)
  if (sampled) {
    set.seed(as.integer(sample_seed))
    selected_rows <- sort(sample.int(original_n, max_cases))
    dat <- dat[selected_rows, , drop = FALSE]
  }
  if (k >= nrow(dat)) stop("Jumlah cluster harus lebih kecil daripada jumlah sampel analisis.")

  zero_var <- variables[vapply(dat, function(v) is.na(stats::sd(v)) || stats::sd(v) == 0, logical(1))]
  if (length(zero_var)) stop("Variabel tanpa variasi harus dikeluarkan: ", paste(zero_var, collapse = ", "))
  x <- as.matrix(dat)
  if (isTRUE(scale_data)) x <- scale(x)
  if (any(!is.finite(x))) stop("Data cluster mengandung nilai tidak terhingga setelah standardisasi.")

  if (method == "kmeans") {
    fit <- stats::kmeans(x, centers = k, nstart = 25)
    cluster_id <- fit$cluster
    centers <- fit$centers
    # Silhouette membutuhkan matriks jarak O(n^2); hitung hanya pada subset evaluasi.
    set.seed(as.integer(sample_seed))
    sil_idx <- if (nrow(x) > 1200L) sort(sample.int(nrow(x), 1200L)) else seq_len(nrow(x))
    sil <- tryCatch(cluster::silhouette(cluster_id[sil_idx], stats::dist(x[sil_idx, , drop = FALSE])),
                    error = function(e) NULL)
    silhouette_n <- length(sil_idx)
  } else if (method == "pam") {
    fit <- cluster::pam(x, k = k)
    cluster_id <- fit$clustering
    centers <- fit$medoids
    sil <- fit$silinfo$widths
    silhouette_n <- nrow(x)
  } else {
    d <- stats::dist(x, method = distance)
    fit <- stats::hclust(d, method = linkage)
    cluster_id <- stats::cutree(fit, k = k)
    centers <- aggregate(as.data.frame(x), list(cluster = cluster_id), mean)
    sil <- tryCatch(cluster::silhouette(cluster_id, d), error = function(e) NULL)
    silhouette_n <- nrow(x)
  }

  sizes <- as.data.frame(table(cluster_id))
  names(sizes) <- c("Cluster", "N")
  avg_sil <- NA_real_
  if (!is.null(sil)) {
    if (is.matrix(sil)) avg_sil <- mean(sil[, "sil_width"])
    if (is.list(sil) && !is.null(sil$avg.width)) avg_sil <- sil$avg.width
  }
  note <- if (sampled) {
    sprintf("SAMPEL: %s dari %s kasus complete-case (seed %s); klaster tidak dihitung untuk kasus di luar sampel.",
            nrow(dat), original_n, sample_seed)
  } else if (method == "kmeans" && silhouette_n < original_n) {
    sprintf("Model K-Means menggunakan %s kasus; silhouette diestimasi dari %s kasus.", original_n, silhouette_n)
  } else sprintf("Analisis menggunakan seluruh %s kasus complete-case.", original_n)

  list(
    fit = fit, cluster = cluster_id, centers = centers, sizes = sizes,
    silhouette = sil, average_silhouette = avg_sil,
    data = dat, scaled = x, method = method, k = k,
    original_n = original_n, analyzed_n = nrow(dat), sampled = sampled,
    selected_rows = selected_rows, sample_seed = as.integer(sample_seed),
    silhouette_n = silhouette_n, sample_note = note
  )
}
