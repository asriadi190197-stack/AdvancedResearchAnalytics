score_excel_sheet <- function(path, sheet) {
  nm <- tolower(sheet)

  # Strong name-based preferences
  bonus <- 0
  if (grepl("rapor[_ ]?publik|raw[_ ]?data|^data$|dataset|response|responses", nm)) bonus <- bonus + 1000
  if (grepl("codebook|pedoman|petunjuk|readme|metadata|keterangan|dictionary", nm)) bonus <- bonus - 1200

  preview <- tryCatch(
    suppressMessages(readxl::read_excel(path, sheet = sheet, n_max = 25)),
    error = function(e) NULL
  )

  if (is.null(preview)) return(-Inf)

  # Prefer sheets with many usable columns and rows
  bonus + (ncol(preview) * 20) + min(nrow(preview), 25)
}

smart_read_excel <- function(path) {
  sheets <- readxl::excel_sheets(path)
  if (!length(sheets)) stop("Workbook Excel tidak memiliki sheet yang dapat dibaca.")

  scores <- vapply(sheets, function(s) score_excel_sheet(path, s), numeric(1))
  selected_sheet <- sheets[which.max(scores)]

  dat <- suppressMessages(
    readxl::read_excel(path, sheet = selected_sheet, .name_repair = "unique")
  )
  dat <- as.data.frame(dat)

  # Read a codebook automatically when available
  cb_idx <- grep("codebook|kamus|dictionary", sheets, ignore.case = TRUE)
  codebook <- NULL
  codebook_sheet <- NA_character_

  if (length(cb_idx)) {
    codebook_sheet <- sheets[cb_idx[1]]
    codebook <- tryCatch(
      as.data.frame(
        suppressMessages(
          readxl::read_excel(path, sheet = codebook_sheet, .name_repair = "unique")
        )
      ),
      error = function(e) NULL
    )
  }

  attr(dat, "ara_excel_sheets") <- sheets
  attr(dat, "ara_selected_sheet") <- selected_sheet
  attr(dat, "ara_sheet_scores") <- scores
  attr(dat, "ara_codebook") <- codebook
  attr(dat, "ara_codebook_sheet") <- codebook_sheet

  dat
}

read_user_data <- function(path, ext) {
  ext <- tolower(ext)

  out <- if (ext == "csv") {
    data.table::fread(path, data.table = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    smart_read_excel(path)
  } else if (ext == "sav") {
    haven::read_sav(path)
  } else if (ext == "dta") {
    haven::read_dta(path)
  } else if (ext == "rds") {
    readRDS(path)
  } else {
    stop("Format file belum didukung.")
  }

  # Preserve smart-import metadata before coercion
  selected_sheet <- attr(out, "ara_selected_sheet")
  sheets <- attr(out, "ara_excel_sheets")
  scores <- attr(out, "ara_sheet_scores")
  codebook <- attr(out, "ara_codebook")
  codebook_sheet <- attr(out, "ara_codebook_sheet")

  out <- as.data.frame(out)

  original_names <- names(out)
  safe_names <- make.names(original_names, unique = TRUE)

  if (!identical(original_names, safe_names)) {
    attr(out, "ara_original_names") <- setNames(original_names, safe_names)
    names(out) <- safe_names
  }

  attr(out, "ara_selected_sheet") <- selected_sheet
  attr(out, "ara_excel_sheets") <- sheets
  attr(out, "ara_sheet_scores") <- scores
  attr(out, "ara_codebook") <- codebook
  attr(out, "ara_codebook_sheet") <- codebook_sheet

  out
}

summarise_variables <- function(df, codebook = NULL) {
  if (is.null(df) || ncol(df) == 0) return(data.frame())

  out <- data.frame(
    Variabel = names(df),
    Tipe = vapply(df, function(x) class(x)[1], character(1)),
    Missing = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    MissingPct = vapply(df, function(x) round(mean(is.na(x)) * 100, 2), numeric(1)),
    Unik = vapply(df, function(x) length(unique(x)), numeric(1)),
    stringsAsFactors = FALSE
  )

  # AN codebooks typically use the first column as variable code
  # and the second column as the indicator/variable label.
  if (!is.null(codebook) && ncol(codebook) >= 2 && nrow(codebook) > 0) {
    cb <- codebook[, 1:2, drop = FALSE]
    names(cb) <- c("Variabel", "Keterangan")
    cb$Variabel <- as.character(cb$Variabel)
    cb$Keterangan <- as.character(cb$Keterangan)
    out <- merge(out, cb, by = "Variabel", all.x = TRUE, sort = FALSE)

    # Restore original dataset order after merge.
    ord <- match(names(df), out$Variabel)
    out <- out[ord, , drop = FALSE]
  }

  out
}

numeric_vars <- function(df) {
  names(df)[vapply(df, is.numeric, logical(1))]
}

factor_candidate_vars <- function(df) {
  names(df)[vapply(df, function(x) {
    is.factor(x) || is.character(x) || is.integer(x)
  }, logical(1))]
}

safe_as_factor <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::as_factor(x)
  factor(x)
}

center_variable <- function(x, method = c("none", "grand", "group"), group = NULL) {
  method <- match.arg(method)

  if (method == "none") return(x)

  if (method == "grand") {
    return(as.numeric(scale(x, center = TRUE, scale = FALSE)))
  }

  if (is.null(group)) stop("Variabel kelompok diperlukan untuk group-mean centering.")

  ave(x, group, FUN = function(z) z - mean(z, na.rm = TRUE))
}

first_existing <- function(candidates, vars) {
  x <- intersect(candidates, vars)
  if (length(x)) x[1] else NULL
}

infer_dataset_map <- function(df, filename = "", selected_sheet = NULL) {
  vars <- names(df)
  num <- numeric_vars(df)

  profile <- "Dataset umum"
  if ("kd_sekolah" %in% vars && any(grepl("^(GD|GE)\\.", vars))) {
    profile <- "Asesmen Nasional - Guru"
  } else if ("kd_sekolah" %in% vars && any(grepl("nilai|literasi|numerasi", vars, ignore.case = TRUE))) {
    profile <- "Asesmen Nasional - Peserta Didik"
  } else if (any(c("CNTSCHID", "CNTSTUID") %in% vars)) {
    profile <- "OECD PISA"
  }

  cluster_school <- first_existing(
    c("kd_sekolah", "CNTSCHID", "school_id", "SCHOOLID"),
    vars
  )
  cluster_region <- first_existing(
    c("kd_kokab", "wilayah_region", "region", "country"),
    vars
  )

  group_var <- first_existing(
    c("sts_sek", "jenis_wilayah", "pendidikan_sederajat",
      "instansi_kewenangan", "gender", "jenis_kelamin"),
    vars
  )

  # Generic safe fallbacks
  generic_y <- if (length(num)) num[1] else NULL
  generic_x <- if (length(num) > 1) num[2] else NULL

  hlm_outcome <- first_existing(
    c("GD.2.5", "GD.4.1", "nilai_numerasi", "nilai_literasi"),
    vars
  )
  if (is.null(hlm_outcome)) hlm_outcome <- generic_y

  hlm_l1 <- intersect(
    c("GD.2.3", "GD.2.4", "GE.3.3", "GE.3.4"),
    vars
  )
  if (!length(hlm_l1)) hlm_l1 <- head(setdiff(num, hlm_outcome), 3)

  # Use variables that are commonly constant within school as initial L2 candidates.
  hlm_l2_candidates <- intersect(
    c("sts_sek", "instansi_kewenangan", "pendidikan_sederajat", "jenis_wilayah"),
    vars
  )
  hlm_l2 <- hlm_l2_candidates[
    vapply(hlm_l2_candidates, function(v) nonmissing_unique_n(df[[v]]) >= 2, logical(1))
  ]

  ipd_y <- first_existing(c("GD.4.1", "GD.2.5", "nilai_numerasi"), vars)
  if (is.null(ipd_y)) ipd_y <- generic_y
  ipd_x <- first_existing(c("GD.4.2", "GE.4.2", "GD.2.4"), vars)
  if (is.null(ipd_x)) ipd_x <- generic_x
  ipd_cluster <- if (!is.null(cluster_region)) cluster_region else cluster_school
  ipd_cov <- intersect(c("GE.4.2", "sts_sek", "instansi_kewenangan"), vars)
  ipd_cov <- setdiff(ipd_cov, c(ipd_x, ipd_y))

  mv_outcomes <- intersect(c("GD.2.4", "GD.2.5", "GD.2.7", "GD.4.1"), vars)
  if (length(mv_outcomes) < 2) mv_outcomes <- head(num, min(3, length(num)))
  mv_predictors <- head(setdiff(num, mv_outcomes), 3)

  list(
    profile = profile,
    filename = filename,
    selected_sheet = selected_sheet,
    n = nrow(df),
    p = ncol(df),

    hlm = list(
      outcome = hlm_outcome,
      cluster = cluster_school,
      l1 = hlm_l1,
      l2 = hlm_l2
    ),

    ipd = list(
      outcome = ipd_y,
      predictor = ipd_x,
      cluster = ipd_cluster,
      covariates = ipd_cov
    ),

    multivariate = list(
      outcomes = mv_outcomes,
      group = group_var,
      predictors = mv_predictors,
      cluster_vars = head(num, min(5, length(num)))
    )
  )
}

center_variable <- function(x, method = c("none", "grand", "group"), group = NULL) {
  method <- match.arg(method)
  if (method == "none") return(x)
  if (method == "grand") return(as.numeric(scale(x, center = TRUE, scale = FALSE)))
  if (is.null(group)) stop("Variabel kelompok diperlukan untuk group-mean centering.")
  ave(x, group, FUN = function(z) z - mean(z, na.rm = TRUE))
}

read_data_path <- function(path) {
  if (!file.exists(path)) stop("File tidak ditemukan: ", path)
  ext <- tools::file_ext(path)
  read_user_data(path, ext)
}

supported_data_file <- function(path) {
  tolower(tools::file_ext(path)) %in% c("csv", "xlsx", "xls", "sav", "dta", "rds")
}


nonmissing_unique_n <- function(x) {
  length(unique(x[!is.na(x)]))
}

prepare_model_predictors <- function(data, vars) {
  vars <- intersect(vars %||% character(0), names(data))
  valid <- character(0)
  skipped <- list()

  for (v in vars) {
    x <- data[[v]]

    if (inherits(x, "haven_labelled")) x <- haven::as_factor(x)
    if (is.character(x)) x <- factor(x)
    if (is.factor(x)) x <- droplevels(x)

    nunique <- nonmissing_unique_n(x)

    if (nunique < 2) {
      skipped[[length(skipped)+1]] <- data.frame(
        Variabel = v,
        Alasan = "Hanya memiliki satu level/nilai pada data yang dianalisis.",
        stringsAsFactors = FALSE
      )
    } else {
      data[[v]] <- x
      valid <- c(valid, v)
    }
  }

  skipped_df <- if (length(skipped)) do.call(rbind, skipped) else
    data.frame(Variabel=character(0), Alasan=character(0))

  list(data=data, valid=valid, skipped=skipped_df)
}

predictor_quality_table <- function(data, vars) {
  vars <- intersect(vars %||% character(0), names(data))
  if (!length(vars)) return(data.frame())

  do.call(rbind, lapply(vars, function(v) {
    x <- data[[v]]
    if (inherits(x, "haven_labelled")) x <- haven::as_factor(x)
    if (is.character(x)) x <- factor(x)
    data.frame(
      Variabel=v,
      Tipe=if (is.factor(x)) "Kategorik" else class(x)[1],
      Level_Nilai_Unik=nonmissing_unique_n(x),
      Status=if (nonmissing_unique_n(x) >= 2) "Dapat digunakan" else "Tidak dapat digunakan",
      stringsAsFactors=FALSE
    )
  }))
}


analysis_count_columns <- function() {
  c(
    "n", "N", "k", "df", "Df", "Num_Df", "Den_Df",
    "Cluster", "Clusters", "Cases", "Observations", "Observasi",
    "DF_Residual", "Jumlah", "Count"
  )
}

analysis_p_columns <- function() {
  c(
    "p", "p_value", "pvalue", "Pr(>|t|)", "Pr(>F)",
    "Q_p", "QEp", "P"
  )
}

format_p_2 <- function(x) {
  # Presentation only: keep exact numeric p-values in computation/export objects.
  ifelse(is.na(x), NA_character_,
         ifelse(x >= 0 & x < 0.0001, "< 0.0001", sprintf("%.4f", x)))
}

analysis_display_df <- function(df, digits = 4) {
  if (is.null(df)) return(data.frame())
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  for (nm in names(df)) {
    if (!is.numeric(df[[nm]])) next

    if (nm %in% analysis_count_columns()) {
      # Keep counts / degrees of freedom readable as whole numbers when they are integer-like.
      vals <- df[[nm]]
      if (all(is.na(vals) | abs(vals - round(vals)) < 1e-10)) {
        df[[nm]] <- ifelse(is.na(vals), NA, round(vals))
      } else {
        df[[nm]] <- round(vals, digits)
      }
    } else if (nm %in% analysis_p_columns() || grepl("(^p$|p[_\\.]?value|Pr\\()", nm, ignore.case = TRUE)) {
      df[[nm]] <- format_p_2(df[[nm]])
    } else {
      # For tiny nonzero effects/SEs, avoid displaying a misleading zero.
      vals <- df[[nm]]
      small <- is.finite(vals) & vals != 0 & abs(vals) < 0.5 * 10^(-digits)
      if (any(small)) {
        rendered <- format(round(vals, digits), trim = TRUE, scientific = FALSE)
        rendered[small] <- formatC(vals[small], digits = 2, format = "e")
        rendered[is.na(vals)] <- NA_character_
        df[[nm]] <- rendered
      } else {
        df[[nm]] <- round(vals, digits)
      }
    }
  }

  df
}

ara_datatable <- function(data, ..., digits = 4) {
  data <- analysis_display_df(data, digits = digits)
  args <- list(...)
  defaults <- list(scrollX = TRUE, autoWidth = FALSE, pageLength = 10,
                   lengthMenu = c(10, 25, 50), deferRender = TRUE)
  if (is.null(args$options)) args$options <- defaults
  else args$options <- utils::modifyList(defaults, args$options)
  # Global UI safety: DataTables scrollX clones the header in a separate table.
  # Initialising while a nav tab / accordion is hidden can then misalign columns.
  # Use a single HTML table and native horizontal overflow instead.
  args$options$scrollX <- FALSE
  args$options$scrollCollapse <- FALSE
  args$options$autoWidth <- FALSE
  args$options$responsive <- FALSE
  args$options$deferRender <- TRUE
  css_class <- if (is.null(args$class)) "stripe hover compact" else args$class
  args$class <- paste(css_class, "ara-dt-stable")
  do.call(DT::datatable, c(list(data), args))
}
