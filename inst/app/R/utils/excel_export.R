# Exporter lintas-platform; tidak memerlukan Microsoft Excel atau Java.
excel_sheet_name <- function(x) {
  x <- as.character(x)[1L]
  if (is.na(x) || !nzchar(trimws(x))) x <- "Sheet"
  x <- gsub("[\\\\/:*?\\[\\]]", "_", x)
  x <- gsub("[[:cntrl:]]", "_", x)
  x <- sub("^'+", "", sub("'+$", "", trimws(x)))
  if (!nzchar(x)) x <- "Sheet"
  substr(x, 1L, 31L)
}

excel_as_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.table(x) || is.matrix(x)) {
    out <- as.data.frame.matrix(x)
    return(data.frame(Kategori = rownames(out), out, row.names = NULL, check.names = FALSE))
  }
  if (is.atomic(x) && !is.null(names(x))) {
    return(data.frame(Parameter = names(x), Nilai = unname(x), check.names = FALSE))
  }
  tryCatch(as.data.frame(x), error = function(e) NULL)
}

excel_safe_df <- function(x) {
  df <- excel_as_df(x)
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!ncol(df)) return(data.frame(Catatan = "Tabel tidak memiliki kolom."))
  nms <- names(df)
  nms[is.na(nms) | !nzchar(trimws(nms))] <- "Kolom"
  names(df) <- make.unique(nms, sep = "_")
  for (j in seq_along(df)) {
    z <- df[[j]]
    if (inherits(z, "POSIXt")) {
      df[[j]] <- as.character(z)
    } else if (inherits(z, "difftime") || inherits(z, "integer64")) {
      df[[j]] <- as.character(z)
    } else if (is.factor(z)) {
      df[[j]] <- as.character(z)
    } else if (is.list(z)) {
      df[[j]] <- vapply(z, function(v) {
        if (is.null(v) || !length(v)) return("")
        paste(as.character(v), collapse = ", ")
      }, character(1))
    } else if (inherits(z, c("labelled", "haven_labelled"))) {
      df[[j]] <- as.character(z)
    } else if (is.complex(z) || (is.object(z) && !inherits(z, c("Date", "POSIXt")))) {
      df[[j]] <- as.character(z)
    }
    if (is.character(df[[j]])) {
      df[[j]] <- iconv(df[[j]], from = "", to = "UTF-8", sub = "")
      # Excel cells must not contain XML-prohibited control characters.
      df[[j]] <- gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", "", df[[j]], perl = TRUE)
      # Prevent formula execution when exporting untrusted labels/text.
      df[[j]] <- ifelse(!is.na(df[[j]]) & grepl("^[[:space:]]*[=+@]", df[[j]]), paste0("'", df[[j]]), df[[j]])
    }
  }
  df
}


# Central validation shared by HLM, IPD, SEM/CFA, MANOVA, regression,
# discriminant analysis, cluster analysis and assumptions modules.
ara_excel_integrity_check <- function(tables) {
  issues <- list()
  add <- function(severity, sheet, reason) {
    issues[[length(issues) + 1L]] <<- data.frame(
      Tingkat = severity, Worksheet = substr(sheet, 1L, 100L),
      Temuan = substr(reason, 1L, 500L), stringsAsFactors = FALSE)
  }
  if (!is.list(tables) || !length(tables)) add("KRITIS", "(workbook)", "Tidak ada tabel hasil")
  names0 <- names(tables)
  if (is.null(names0)) names0 <- rep("", length(tables))
  for (i in seq_along(tables)) {
    nm <- if (is.na(names0[i]) || !nzchar(names0[i])) paste0("Tabel_", i) else names0[i]
    raw <- tables[[i]]
    df <- excel_as_df(raw)
    if (is.null(df)) {
      if (!is.null(raw)) add("KRITIS", nm, "Objek hasil tidak dapat dikonversi menjadi tabel")
      next
    }
    if (!ncol(df)) add("KRITIS", nm, "Tabel tidak memiliki kolom")
    if (!nrow(df)) add("PERINGATAN", nm, "Tabel kosong; tidak ada hasil untuk dilaporkan")
    for (j in seq_along(df)) {
      z <- df[[j]]
      if (is.numeric(z) && any(is.infinite(z), na.rm=TRUE))
        add("KRITIS", nm, paste0("Nilai Inf/-Inf di kolom ", names(df)[j]))
    }
    if ("ID Cluster" %in% names(df)) {
      ids <- trimws(as.character(df[["ID Cluster"]]))
      if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))
        add("KRITIS", nm, "ID cluster BLUP kosong atau duplikat")
    }
    if (all(c("Estimate", "SE", "Status") %in% names(df))) {
      ok <- !is.na(df$Status) & toupper(as.character(df$Status)) == "OK"
      if (any(ok) && (anyNA(df$SE[ok]) || any(!is.finite(suppressWarnings(as.numeric(df$SE[ok]))))))
        add("KRITIS", nm, "SE kosong/tidak valid pada hasil estimasi berstatus OK")
    }
    if ("Status" %in% names(df) && nrow(df)) {
      states <- toupper(trimws(as.character(df$Status)))
      failures <- which(states %in% c("GAGAL", "ERROR", "FAILED") |
                         grepl("^(REFIT GAGAL|FORMULA GAGAL|RINGKASAN GAGAL)", states))
      if (length(failures)) add("KRITIS", nm,
        paste0(length(failures), " uji gagal; baca status dan perbaiki dependensi/model sebelum ekspor"))
      skipped <- sum(grepl("^DILEWATI|^SKIPPED", states))
      if (skipped) add("PERINGATAN", nm, paste0(skipped, " skenario dilewati; bukan hasil uji sukses"))
    }
    # Statistical consistency checks for common outputs, without relying on a
    # specific engine. Do not convert NA (not estimated) to a fabricated zero.
    for (j in seq_along(df)) {
      z <- df[[j]]
      col <- names(df)[j]
      if (!is.numeric(z)) next
      if (grepl("(^p$|p[-_. ]?value$|pvalue$|bartlett_p$)", col, ignore.case=TRUE, perl=TRUE) || startsWith(col, "Pr(")) {
        if (any(z < 0 | z > 1, na.rm=TRUE)) add("KRITIS", nm,
          paste0("p-value di luar [0,1]: ", col))
      }
      if (grepl("^(SE|Std[. _]*Error|Std[.] Error)", col, ignore.case=TRUE)) {
        if (any(z < 0, na.rm=TRUE)) add("KRITIS", nm,
          paste0("Standard error negatif: ", col))
      }
    }
    bounds <- list(c("CI_low", "CI_high"), c("ci.lb", "ci.ub"),
       c("95% CI Lower (Wald)", "95% CI Upper (Wald)"))
    for (pair in bounds) if (all(pair %in% names(df))) {
      low <- df[[pair[1L]]]; high <- df[[pair[2L]]]
      if (is.numeric(low) && is.numeric(high) && any(low > high, na.rm=TRUE))
        add("KRITIS", nm, paste0("Batas interval kepercayaan terbalik: ", pair[1L]))
    }
    # NaN is an invalid statistic; NA can mean not estimated and must remain explicit.
    for (j in seq_along(df)) {
      z <- df[[j]]
      if (is.numeric(z) && any(is.nan(z))) add("KRITIS", nm,
        paste0("NaN pada kolom ", names(df)[j]))
    }
  }
  if (!length(issues)) {
    return(data.frame(Tingkat = "LULUS", Worksheet = "Semua tabel",
      Temuan = "Tidak ada kesalahan struktural yang terdeteksi oleh pemeriksaan otomatis.", stringsAsFactors = FALSE))
  }
  unique(do.call(rbind, issues))
}

ara_excel_assert_integrity <- function(tables) {
  qc <- ara_excel_integrity_check(tables)
  serious <- qc[qc$Tingkat == "KRITIS", , drop=FALSE]
  if (nrow(serious)) {
    msg <- paste0("EKSPOR DIBLOKIR: ", nrow(serious),
      " masalah kritis. ", paste(paste0(serious$Worksheet, ": ", serious$Temuan), collapse=" | "),
      ". Perbaiki dan jalankan ulang uji, kemudian ekspor kembali.")
    if (requireNamespace("shiny", quietly=TRUE)) {
      try(shiny::showNotification(msg, type="error", duration=NULL), silent=TRUE)
    }
    stop(msg, call.=FALSE)
  }
  qc
}

export_analysis_excel <- function(tables, file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Paket 'openxlsx' belum terpasang. Jalankan scripts/install_dependencies.R.")
  }
  if (!is.list(tables) || !length(tables)) stop("Tidak ada tabel yang dapat diekspor.")
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) stop("Lokasi berkas Excel tidak valid.")
  dir_target <- dirname(file)
  if (!dir.exists(dir_target)) stop("Folder ekspor tidak ditemukan: ", dir_target)
  supplied_names <- names(tables)
  if (is.null(supplied_names)) supplied_names <- paste0("Tabel_", seq_along(tables))
  keep <- !vapply(tables, is.null, logical(1))
  missing_tables <- supplied_names[!keep]
  tables <- tables[keep]
  if (!length(tables)) stop("Tidak ada tabel hasil yang dapat diekspor.")
  # Tables needed to interpret the chosen analysis may never be silently omitted.
  mandatory <- ara_export_required_sheets(tables)
  absent <- mandatory[!vapply(mandatory, function(nm) {
    x <- tables[[nm]]
    df <- excel_as_df(x)
    !is.null(df) && nrow(df) > 0L && ncol(df) > 0L
  }, logical(1))]
  if (length(absent)) stop("EKSPOR DIBLOKIR: tabel inti tidak tersedia: ",
                           paste(absent, collapse = ", "), call. = FALSE)
  # No file is written if any critical failure is found.
  quality <- ara_excel_assert_integrity(tables)
  if (length(missing_tables)) {
    quality <- rbind(quality, data.frame(Tingkat = "PERINGATAN",
      Worksheet = missing_tables,
      Temuan = "Hasil tidak tersedia, sehingga worksheet tidak diekspor; bukan berarti nilai nol.",
      stringsAsFactors = FALSE))
  }
  if ("Audit_Integritas" %in% names(tables))
    stop("Nama worksheet Audit_Integritas disediakan untuk validasi sistem.")
  glossary <- data.frame(
    Istilah = c("LULUS", "PERINGATAN", "KRITIS", "NA/kosong", "p-value", "Interval kepercayaan",
                "REML", "BLUP", "Hasil uji tidak tersedia"),
    Arti = c("Tidak ditemukan masalah oleh aturan otomatis; bukan validasi independen perhitungan",
             "Keterbatasan yang harus dipertimbangkan ketika menafsirkan hasil",
             "Workbook tidak diizinkan diekspor sampai masalah diselesaikan",
             "Parameter tidak dihitung atau tidak terdefinisi; bukan sama dengan nol",
             "Nilai numerik dipertahankan, tidak dibulatkan menjadi 0,00",
             "Batas bawah dan atas harus berasal dari prosedur model terkait",
             "REML tidak digunakan untuk likelihood ratio test struktur fixed-effects berbeda",
             "Efek acak bersyarat; bukan rata-rata mentah atau efek kausal",
             "Hasil kosong/dilewati tidak boleh diinterpretasikan sebagai hasil signifikan/tidak signifikan"),
    stringsAsFactors = FALSE)
  tables <- c(list(Audit_Integritas = quality, Panduan_Output = glossary), tables)

  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#243B53",
                                   textDecoration = "bold", halign = "center", valign = "center")
  used <- character()
  tbl_names <- names(tables)
  if (is.null(tbl_names)) tbl_names <- rep("", length(tables))
  for (i in seq_along(tables)) {
    nm <- tbl_names[i]
    if (is.na(nm) || !nzchar(trimws(nm))) nm <- paste0("Tabel_", i)
    df <- excel_safe_df(tables[[i]])
    if (is.null(df)) next
    # Preserve precision of effect sizes, p-values and confidence intervals.
    # Rounding to 2 decimals can falsely turn significant p-values into 0.00.
    sheet <- excel_sheet_name(nm)
    base <- sheet
    idx <- 2L
    while (tolower(sheet) %in% tolower(used)) {
      suffix <- paste0("_", idx)
      sheet <- paste0(substr(base, 1L, 31L - nchar(suffix)), suffix)
      idx <- idx + 1L
    }
    used <- c(used, sheet)
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
    if (nrow(df)) {
      openxlsx::writeData(wb, sheet, df, withFilter = TRUE, keepNA = FALSE)
    } else {
      openxlsx::writeData(wb, sheet, df, withFilter = FALSE)
    }
    openxlsx::addStyle(wb, sheet, header, rows = 1, cols = seq_len(ncol(df)),
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    numerical_cols <- which(vapply(df, is.numeric, logical(1)))
    if (length(numerical_cols) && nrow(df)) {
      integer_cols <- numerical_cols[vapply(df[numerical_cols], is.integer, logical(1))]
      decimal_cols <- setdiff(numerical_cols, integer_cols)
      if (length(integer_cols)) openxlsx::addStyle(wb, sheet,
        openxlsx::createStyle(numFmt = "0"), rows = 2:(nrow(df)+1L),
        cols = integer_cols, gridExpand = TRUE, stack = TRUE)
      if (length(decimal_cols)) openxlsx::addStyle(wb, sheet,
        openxlsx::createStyle(numFmt = "0.####"), rows = 2:(nrow(df)+1L),
        cols = decimal_cols, gridExpand = TRUE, stack = TRUE)
      # Very small p-values remain numeric in Excel; scientific notation prevents 0.0000.
      p_cols <- decimal_cols[vapply(names(df)[decimal_cols], function(nm) {
        nm %in% analysis_p_columns() ||
          grepl("(^p$|p[_\\.]?value|Pr\\()", nm, ignore.case = TRUE)
      }, logical(1))]
      if (length(p_cols)) for (j in p_cols) {
        vals <- df[[j]]
        if (any(is.finite(vals) & vals > 0 & vals < 0.0001)) {
          openxlsx::addStyle(wb, sheet,
            openxlsx::createStyle(numFmt = "[>=0.0001]0.####;0.00E+00"),
            rows = 2:(nrow(df)+1L), cols = j, gridExpand = TRUE, stack = TRUE)
        }
      }
    }
    widths <- vapply(seq_len(ncol(df)), function(j) {
      preview <- utils::head(df[[j]], 200L)
      if (is.numeric(preview)) preview <- format(round(preview, 4), trim = TRUE, scientific = FALSE)
      vals <- c(names(df)[j], as.character(preview))
      widths <- nchar(vals, type = "width", allowNA = TRUE)
      widths <- widths[is.finite(widths)]
      min(42, max(12, if(length(widths)) max(widths) + 2 else 12))
    }, numeric(1))
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(df)), widths = widths)
  }
  if (!length(used)) stop("Tidak ada tabel yang dapat dikonversi ke Excel.")
  # Save atomically and inspect workbook as a valid ZIP container before copying.
  tmp <- tempfile(pattern = "ara_export_", fileext = ".xlsx", tmpdir = dir_target)
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE),
           error = function(e) stop("Gagal menulis workbook Excel: ", conditionMessage(e)))
  valid <- tryCatch(utils::unzip(tmp, list = TRUE), error = function(e) NULL)
  if (is.null(valid) || !any(valid$Name == "xl/workbook.xml") ||
      !any(valid$Name == "[Content_Types].xml")) {
    stop("Validasi Excel gagal: workbook tidak lengkap.")
  }
  if (!file.copy(tmp, file, overwrite = TRUE)) {
    stop("Gagal menyimpan file Excel. Periksa izin folder dan tutup berkas jika sedang digunakan: ", file)
  }
  if (!file.exists(file) || file.info(file)$size == 0) stop("Berkas Excel kosong setelah ekspor.")
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

hlm_assert_unique_blup <- function(df) {
  if (!is.data.frame(df) || !"ID Cluster" %in% names(df)) {
    stop("Tabel BLUP harus memiliki kolom 'ID Cluster'.")
  }
  id <- as.character(df[["ID Cluster"]])
  if (anyNA(id) || any(!nzchar(trimws(id))) || anyDuplicated(id)) {
    stop("Ekspor dibatalkan: ada ID cluster kosong atau duplikat pada tabel BLUP.")
  }
  invisible(TRUE)
}

# Statistik campuran tidak boleh memaksa seluruh kolom nilai menjadi teks.
ara_mixed_summary <- function(parameters, values) {
  values <- as.list(values)
  if (length(parameters) != length(values)) stop("Panjang ringkasan tidak cocok")
  numeric_value <- vapply(values, function(v) {
    if (length(v) == 1L && is.numeric(v) && !is.na(v)) as.numeric(v) else NA_real_
  }, numeric(1))
  textual_value <- vapply(values, function(v) {
    if (length(v) == 1L && is.numeric(v) && !is.na(v)) ""
    else if (!length(v) || all(is.na(v))) "Tidak tersedia"
    else paste(as.character(v), collapse = ", ")
  }, character(1))
  data.frame(Parameter = parameters, Nilai_Numerik = numeric_value,
             Nilai_Teks = textual_value, stringsAsFactors = FALSE)
}

# Daftar tabel inti untuk mencegah workbook berstatus berhasil padahal
# keluaran utama analisis hilang. Tambahan diagnostik tetap opsional.
ara_export_required_sheets <- function(tables) {
  info <- tables[["Info_Analisis"]]
  if (is.null(info) || !is.data.frame(info) ||
      !all(c("Informasi", "Nilai") %in% names(info))) return(character(0))
  analysis <- as.character(info$Nilai[match("Analisis", info$Informasi)])
  if (!length(analysis) || is.na(analysis)) return(character(0))
  switch(analysis,
    "Multilevel / HLM" = c("Tabel_1_Model_Berjenjang", "Tabel_2_Koefisien_Penuh",
      "Tabel_3_BLUP_Top20", "Ringkasan_Model", "Kecocokan_Model", "Efek_Acak"),
    "Two-Stage IPD Meta-Analysis" = c("Tabel_1_Efek_Gabungan", "Tabel_2_Regresi_Klaster"),
    "One-Stage IPD Meta-Analysis" = c("Ringkasan_Model", "Efek_Tetap", "Efek_Acak"),
    "MANOVA / MANCOVA" = c("Uji_Multivariat", "Deskriptif_Kelompok", "Informasi_Sampel"),
    "Multivariate Regression" = c("Uji_Multivariat", "Statistik_Model"),
    "Discriminant Analysis" = c("Ringkasan", "Confusion_Matrix"),
    "Cluster Analysis" = c("Ringkasan", "Ukuran_Cluster", "Pusat_Medoid_Cluster"),
    "CFA" = c("Kecocokan_Model", "Muatan_Faktor", "Parameter_Model"),
    "SEM" = c("Kecocokan_Model", "Jalur_Struktural", "Parameter_Model"),
    "Multi-Group SEM" = c("Kecocokan_Model", "Parameter_Model"),
    "Measurement Invariance" = c("Perbandingan_Invariance"),
    character(0))
}


hlm_excel_tables <- function(x) {
  z <- tail(x$fit, 1)

  summary_tbl <- ara_mixed_summary(
    c("Observasi", "Cluster", "ICC null model", "Varians antar-cluster",
      "Varians dalam cluster", "Singular fit", "AIC", "BIC", "Marginal R2", "Conditional R2"),
    list(x$n, x$clusters, x$icc, x$between, x$within,
         if (isTRUE(x$singular)) "YA" else "TIDAK",
         z$AIC, z$BIC, z$Marginal_R2, z$Conditional_R2)
  )

  blup_tbl <- hlm_blup_top20_publication(x, 20)
  hlm_assert_unique_blup(blup_tbl)
  metodologi <- data.frame(
    Komponen=c("ICC model null", "REML dan ML", "Perbandingan LRT",
      "BLUP", "SD bersyarat BLUP", "SE dan CI koefisien", "Sensitivitas subset"),
    Keterangan=c(
      "ICC pada Ringkasan_Model berasal dari null model, bukan otomatis ICC conditional model penuh",
      "REML cocok untuk estimasi komponen varians; perbandingan fixed-effects berbeda menggunakan ML dan model nested",
      "Saat REML aktif, p-value LRT fixed-effects tidak dihitung; struktur random effect berbeda memerlukan perlakuan tersendiri",
      "Random intercept deviation adalah efek acak bersyarat; conditional intercept adalah fixed intercept + BLUP",
      "Nilai tidak tersedia dinyatakan kosong (NA), tidak diganti angka nol",
      "Koefisien, SE, p, CI disimpan sebagai numerik; batas Wald aproksimasi 1.96 SE",
      "Koefisien pada subset berbeda komposisi; AIC lintas subset tidak dapat dibandingkan langsung"
    ), stringsAsFactors=FALSE)
  out <- list(
    "Catatan_Metodologi" = metodologi,
    "Tabel_1_Model_Berjenjang" = hlm_model_comparison_publication(x),
    "Tabel_2_Koefisien_Penuh" = hlm_full_coefficients_publication(x),
    "Tabel_3_BLUP_Top20" = blup_tbl,
    "Ringkasan_Model" = summary_tbl,
    "Kecocokan_Model" = x$fit,
    "Efek_Acak" = x$random
  )
  if (!is.null(x$sensitivity)) {
    out[["Sensitivitas_Koefisien"]] <- x$sensitivity$coefficients
    if (nrow(x$sensitivity$status)) out[["Sensitivitas_Status"]] <- x$sensitivity$status
  }
  if (!is.null(x$advanced)) out <- c(out, ara_hlm_advanced_excel(x$advanced))
  out
}

ipd_two_stage_excel_tables <- function(x) {
  out <- list(
    "Tabel_1_Efek_Gabungan" = ipd_pooled_publication_table(x),
    "Tabel_2_Regresi_Klaster" = ipd_stage1_publication_table(x)
  )

  if (!is.null(x$leave1out)) {
    out[["LOO_Metode"]] <- data.frame(
      Metode = if (identical(x$diagnostics_mode, "fast"))
        "Cepat: tau² tetap" else "Penuh: refit setiap klaster",
      Keterangan = x$diagnostics_note %||% "Metode belum dicatat.",
      stringsAsFactors = FALSE
    )
    out[["Leave_One_Out"]] <- tryCatch(
      as.data.frame(x$leave1out),
      error = function(e) data.frame(Catatan = "Leave-one-out tidak dapat dikonversi.")
    )
  }

  if (!is.null(x$influence)) {
    infl <- tryCatch({
      if (!is.null(x$influence$inf)) as.data.frame(x$influence$inf)
      else as.data.frame(x$influence)
    }, error = function(e) NULL)
    if (!is.null(infl)) out[["Influence_Diagnostics"]] <- infl
  }

  if (!is.null(x$meta_reg)) {
    reg <- tryCatch(as.data.frame(coef(summary(x$meta_reg))), error = function(e) NULL)
    if (!is.null(reg)) {
      reg$Parameter <- rownames(reg)
      rownames(reg) <- NULL
      reg <- reg[, c("Parameter", setdiff(names(reg), "Parameter")), drop = FALSE]
      out[["Meta_Regression"]] <- reg
    }
  }

  if (!is.null(x$extra)) {
    for (nm in names(x$extra)) {
      if (is.data.frame(x$extra[[nm]])) out[[paste0("IPD_", nm)]] <- x$extra[[nm]]
    }
  }
  out
}

ipd_one_stage_excel_tables <- function(x) {
  summary_tbl <- ara_mixed_summary(
    c("Observasi", "Cluster", "AIC", "BIC", "LogLik",
      "Marginal R2", "Conditional R2", "Singular fit"),
    list(x$n, x$clusters, x$aic, x$bic, x$logLik,
      x$r2_marginal, x$r2_conditional,
      if (isTRUE(x$singular)) "YA" else "TIDAK")
  )

  random_tbl <- tryCatch(
    as.data.frame(lme4::VarCorr(x$model)),
    error = function(e) data.frame(Catatan = "Efek acak tidak tersedia.")
  )

  list(
    "Ringkasan_Model" = summary_tbl,
    "Efek_Tetap" = x$coefficients,
    "Efek_Acak" = random_tbl
  )
}

lavaan_r2_table <- function(fit) {
  r2 <- tryCatch(lavaan::inspect(fit, "r2"), error = function(e) NULL)
  if (is.null(r2)) return(NULL)

  data.frame(
    Variabel = names(r2),
    R2 = as.numeric(r2),
    stringsAsFactors = FALSE
  )
}

lavaan_mi_table <- function(fit) {
  tryCatch({
    mi <- lavaan::modindices(fit)
    mi[order(mi$mi, decreasing = TRUE), , drop = FALSE]
  }, error = function(e) NULL)
}

lavaan_all_parameters <- function(fit) {
  tryCatch(
    lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE),
    error = function(e) NULL
  )
}

cfa_excel_tables <- function(x) {
  list(
    "Kecocokan_Model" = x$fit_measures,
    "Muatan_Faktor" = x$loadings,
    "R2_Indikator" = x$r2_table,
    "Modification_Indices" = x$modification_indices,
    "Parameter_Model" = x$parameters
  )
}

sem_excel_tables <- function(x) {
  list(
    "Kecocokan_Model" = x$fit_measures,
    "Jalur_Struktural" = x$paths,
    "Muatan_Faktor" = x$loadings,
    "R2_Endogen" = x$r2_table,
    "Modification_Indices" = x$modification_indices,
    "Parameter_Model" = x$parameters
  )
}

multigroup_excel_tables <- function(x) {
  group_n <- tryCatch(lavaan::lavInspect(x$fit, "nobs"), error = function(e) NULL)
  group_tbl <- NULL
  if (!is.null(group_n)) {
    group_tbl <- data.frame(
      Kelompok = names(group_n) %||% seq_along(group_n),
      N = as.numeric(group_n),
      stringsAsFactors = FALSE
    )
  }

  list(
    "Kecocokan_Model" = x$fit_measures,
    "Ukuran_Kelompok" = group_tbl,
    "Parameter_Model" = x$parameters
  )
}

invariance_excel_tables <- function(x) {
  out <- list("Perbandingan_Invariance" = x$table)

  for (nm in names(x$models)) {
    pe <- tryCatch(
      lavaan::parameterEstimates(x$models[[nm]], standardized = TRUE, ci = TRUE),
      error = function(e) NULL
    )
    if (!is.null(pe)) {
      out[[paste0("Parameter_", nm)]] <- pe
    }
  }

  out
}

manova_univariate_tables <- function(x) {
  out <- list("Uji_Multivariat" = x$multivariate_tests)
  if (!is.null(x$sample_info)) out[["Informasi_Sampel"]] <- x$sample_info
  if (!is.null(x$group_descriptives)) out[["Deskriptif_Kelompok"]] <- x$group_descriptives
  if (!is.null(x$formula)) out[["Spesifikasi_Model"]] <- data.frame(
    Komponen = "Formula", Nilai = paste(deparse(x$formula), collapse = ""))

  uni <- x$univariate
  if (length(uni)) {
    for (i in seq_along(uni)) {
      nm <- names(uni)[i]
      if (is.null(nm) || !nzchar(nm)) nm <- paste0("Outcome_", i)

      obj <- uni[[i]]
      tbl <- NULL

      if (is.list(obj) && length(obj) && inherits(obj[[1]], "anova")) {
        tbl <- as.data.frame(obj[[1]])
      } else {
        tbl <- tryCatch(as.data.frame(obj), error = function(e) NULL)
      }

      if (!is.null(tbl)) {
        # Partial eta-squared untuk tiap efek; jangan melaporkan residual sebagai efek.
        if ("Sum Sq" %in% names(tbl)) {
          rs <- which(trimws(rownames(tbl)) == "Residuals")
          ss_res <- if (length(rs)) suppressWarnings(as.numeric(tbl[["Sum Sq"]][rs[1L]])) else NA_real_
          ss <- suppressWarnings(as.numeric(tbl[["Sum Sq"]]))
          tbl$Partial_Eta2 <- if (is.finite(ss_res)) ss / (ss + ss_res) else NA_real_
          if (length(rs)) tbl$Partial_Eta2[rs] <- NA_real_
        }
        tbl$Term <- rownames(tbl)
        rownames(tbl) <- NULL
        tbl <- tbl[, c("Term", setdiff(names(tbl), "Term")), drop = FALSE]
        out[[paste0("Univariat_", nm)]] <- tbl
      }
    }
  }

  out
}

multivar_reg_excel_tables <- function(x) {
  out <- list("Uji_Multivariat" = x$multivariate_tests)

  sm <- x$coefficients
  if (length(sm)) {
    model_stats <- list()

    for (i in seq_along(sm)) {
      nm <- names(sm)[i]
      if (is.null(nm) || !nzchar(nm)) nm <- paste0("Outcome_", i)
      s <- sm[[i]]

      co <- as.data.frame(s$coefficients)
      co$Term <- rownames(co)
      rownames(co) <- NULL
      co <- co[, c("Term", setdiff(names(co), "Term")), drop = FALSE]
      out[[paste0("Koef_", nm)]] <- co

      model_stats[[i]] <- data.frame(
        Outcome = nm,
        R2 = s$r.squared,
        Adjusted_R2 = s$adj.r.squared,
        Residual_SE = s$sigma,
        DF_Residual = s$df[2],
        stringsAsFactors = FALSE
      )
    }

    out[["Statistik_Model"]] <- do.call(rbind, model_stats)
  }

  out
}

discriminant_excel_tables <- function(x) {
  summary_tbl <- ara_mixed_summary(
    c("N", "Training Accuracy", "Cross-Validation Accuracy"),
    list(x$n, x$accuracy, x$cv_accuracy)
  )

  conf <- as.data.frame.matrix(x$confusion)
  conf <- cbind(Aktual = rownames(conf), conf, row.names = NULL)

  cv_conf <- NULL
  if (!is.null(x$cv_confusion)) {
    cv_conf <- as.data.frame.matrix(x$cv_confusion)
    cv_conf <- cbind(Aktual = rownames(cv_conf), cv_conf, row.names = NULL)
  }

  out <- list(
    "Ringkasan" = summary_tbl,
    "Confusion_Matrix" = conf,
    "CV_Confusion_Matrix" = cv_conf
  )

  fit <- x$fit

  if (!is.null(fit$prior)) {
    out[["Prior_Probability"]] <- data.frame(
      Kelompok = names(fit$prior),
      Prior = as.numeric(fit$prior),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(fit$counts)) {
    out[["Jumlah_per_Kelompok"]] <- data.frame(
      Kelompok = names(fit$counts),
      N = as.numeric(fit$counts),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(fit$means)) {
    means <- as.data.frame(fit$means)
    means <- cbind(Kelompok = rownames(means), means, row.names = NULL)
    out[["Rata_Rata_Kelompok"]] <- means
  }

  if (!is.null(fit$scaling) && is.matrix(fit$scaling)) {
    sc <- as.data.frame(fit$scaling)
    sc <- cbind(Variabel = rownames(sc), sc, row.names = NULL)
    out[["Koefisien_Diskriminan"]] <- sc
  }

  out
}

cluster_excel_tables <- function(x) {
  summary_tbl <- ara_mixed_summary(
    c("Metode", "Jumlah Cluster", "Jumlah Kasus", "Average Silhouette"),
    list(x$method, x$k, nrow(x$data), x$average_silhouette)
  )

  centers <- x$centers
  if (is.matrix(centers)) centers <- as.data.frame(centers)
  if (is.data.frame(centers) && is.null(centers$Cluster) && !is.null(rownames(centers))) {
    centers <- cbind(Cluster = rownames(centers), centers, row.names = NULL)
  }

  sample_info <- data.frame(
    Parameter = c("Metode", "N complete-case awal", "N dianalisis", "Sampling", "Seed", "N evaluasi silhouette", "Catatan"),
    Nilai = c(x$method, x$original_n %||% nrow(x$data), nrow(x$data),
              if (isTRUE(x$sampled)) "Ya" else "Tidak", x$sample_seed %||% 2026L,
              x$silhouette_n %||% NA_integer_, x$sample_note %||% ""),
    stringsAsFactors = FALSE
  )
  list(
    "Ringkasan" = summary_tbl,
    "Informasi_Sampel" = sample_info,
    "Ukuran_Cluster" = x$sizes,
    "Pusat_Medoid_Cluster" = centers
  )
}


analysis_info_table <- function(analysis_name, provenance = NULL, extra = NULL) {
  info <- data.frame(
    Informasi = c("Analisis", "Tanggal Analisis", "Versi Aplikasi"),
    Nilai = c(analysis_name, as.character(Sys.Date()), APP_VERSION),
    stringsAsFactors = FALSE
  )

  if (!is.null(provenance)) {
    p_rows <- data.frame(
      Informasi = c("Dataset", "Sumber Dataset", "Lokasi/File", "Catatan"),
      Nilai = c(
        provenance$name %||% "",
        provenance$source %||% "",
        provenance$local_path %||% "",
        provenance$notes %||% ""
      ),
      stringsAsFactors = FALSE
    )
    info <- rbind(info, p_rows)
  }

  if (!is.null(extra) && length(extra)) {
    extra_df <- data.frame(
      Informasi = names(extra),
      Nilai = vapply(
        extra,
        function(x) paste(as.character(x), collapse = ", "),
        character(1)
      ),
      stringsAsFactors = FALSE
    )
    info <- rbind(info, extra_df)
  }

  info
}

prepend_analysis_info <- function(tables, analysis_name, provenance = NULL, extra = NULL) {
  c(
    list(
      "Info_Analisis" = analysis_info_table(
        analysis_name = analysis_name,
        provenance = provenance,
        extra = extra
      )
    ),
    tables
  )
}
