# Diagnostics intended for multivariate analysis and SEM screening.
# Input variable names are explicitly selected and numerical-only.
ara_assumptions <- function(data, variables, tests = c("descriptive", "correlation", "kmo", "vif"),
                            sample_n = 5000L, seed = 2026L) {
  variables <- unique(intersect(variables, names(data)))
  if (length(variables) < 2L) stop("Pilih setidaknya dua variabel.")
  if (any(!vapply(data[variables], is.numeric, logical(1))))
    stop("Uji ini memerlukan kolom numerik; recode kategori jika perlu.")
  dd <- data[, variables, drop = FALSE]
  n_raw <- nrow(dd)
  na_count <- colSums(is.na(dd))
  dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  n_complete <- nrow(dd)
  if (n_complete < 10L) stop("Complete cases minimal 10.")
  v <- vapply(dd, stats::sd, numeric(1))
  if (any(!is.finite(v) | v == 0)) stop("Terdapat variabel konstan atau tidak valid.")
  status <- list(); out <- list()
  attempt <- function(name, fun) {
    tryCatch({
      out[[name]] <<- fun()
      status[[length(status) + 1L]] <<- data.frame(Uji = name, Status = "OK", Pesan = "Selesai")
    }, error = function(e) {
      status[[length(status) + 1L]] <<- data.frame(Uji = name, Status = "GAGAL", Pesan = conditionMessage(e))
    })
  }
  if ("descriptive" %in% tests) attempt("Deskriptif dan data hilang", function() {
    data.frame(Variabel = variables, Total = n_raw, Missing = unname(na_count),
      Missing_pct = 100 * unname(na_count) / n_raw, Complete_N = n_complete,
      Mean = vapply(dd, mean, numeric(1)), SD = v,
      Skewness = vapply(dd, function(x) mean((x - mean(x))^3)/stats::sd(x)^3, numeric(1)),
      Excess_Kurtosis = vapply(dd, function(x) mean((x-mean(x))^4)/stats::sd(x)^4 - 3, numeric(1)),
      row.names = NULL)
  })
  if ("correlation" %in% tests) attempt("Matriks korelasi", function() {
    mm <- stats::cor(dd)
    data.frame(Variabel = rownames(mm), mm, row.names = NULL, check.names = FALSE)
  })
  if ("kmo" %in% tests) attempt("KMO dan Bartlett", function() {
    p <- length(variables)
    if (n_complete <= p) stop("Jumlah complete case harus melebihi jumlah variabel.")
    r <- stats::cor(dd)
    if (min(eigen(r, only.values = TRUE)$values) <= 1e-8)
      stop("Matriks korelasi singular; hilangkan variabel redundant.")
    inv <- solve(r)
    pc <- -stats::cov2cor(inv)
    diag(pc) <- 0
    r0 <- r; diag(r0) <- 0
    overall <- sum(r0^2) / (sum(r0^2) + sum(pc^2))
    msa <- rowSums(r0^2)/(rowSums(r0^2)+rowSums(pc^2))
    chi <- -(n_complete - 1 - (2*p+5)/6) * determinant(r, logarithm=TRUE)$modulus[[1]]
    df <- p*(p-1)/2
    data.frame(Variabel = c("TOTAL", variables), KMO = c(overall, msa),
       Bartlett_Chi2 = c(as.numeric(chi), rep(NA_real_, p)),
       Bartlett_df = c(df, rep(NA_integer_, p)),
       Bartlett_p = c(stats::pchisq(chi, df, lower.tail = FALSE), rep(NA_real_, p)))
  })
  if ("vif" %in% tests) attempt("VIF prediktor", function() {
    r <- stats::cor(dd)
    if (min(eigen(r, only.values = TRUE)$values) <= 1e-8)
      stop("Kolinearitas sempurna; VIF tidak terdefinisi.")
    data.frame(Variabel = variables, VIF = diag(solve(r)),
      Tolerance = 1/diag(solve(r)), row.names = NULL)
  })
  if ("normality" %in% tests) attempt("Normalitas univariat sampel", function() {
    set.seed(seed)
    ix <- if (n_complete > sample_n) sample.int(n_complete, sample_n) else seq_len(n_complete)
    ss <- dd[ix, , drop=FALSE]
    data.frame(Variabel = variables, Sample_N = nrow(ss),
      Shapiro_W = vapply(ss, function(z) if (length(z) > 5000) NA_real_ else unname(stats::shapiro.test(z)$statistic), numeric(1)),
      P_value = vapply(ss, function(z) if (length(z) > 5000) NA_real_ else stats::shapiro.test(z)$p.value, numeric(1)),
      row.names = NULL)
  })
  out$Status <- if(length(status)) do.call(rbind, status) else data.frame()
  out$Catatan <- "KMO/Bartlett bersifat screening analisis faktor; Shapiro-Wilk menguji normalitas univariat, bukan normalitas multivariat. VIF dihitung pada prediktor numerik terpilih."
  out
}
