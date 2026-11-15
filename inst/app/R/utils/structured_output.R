# Tampilan tabel yang sama dengan data ekspor; tanpa keluaran konsol R.
ara_output_table <- function(x, message = "Hasil belum tersedia.") {
  df <- excel_as_df(x)
  if (is.null(df) || !nrow(df)) df <- data.frame(Catatan = message)
  ara_datatable(df, rownames = FALSE,
    options = list(scrollX = FALSE, pageLength = 15, autoWidth = FALSE))
}

ara_syntax_sheet <- function(code) {
  data.frame(Baris = seq_along(strsplit(code, "\n", fixed = TRUE)[[1L]]),
             Sintaks_R = strsplit(code, "\n", fixed = TRUE)[[1L]],
             stringsAsFactors = FALSE)
}

ara_sem_summary_table <- function(x) {
  fm <- x$fit_measures
  if (is.null(fm) || !nrow(as.data.frame(fm)))
    return(data.frame(Catatan = "Indeks kecocokan tidak tersedia."))
  as.data.frame(fm, check.names = FALSE)
}

# Standar hasil lintas-modul: tampilan dibatasi tanpa mengubah objek ekspor.
ara_html_result_table <- function(data, empty_message = "Hasil belum tersedia.", max_rows = 50L) {
  df <- tryCatch(as.data.frame(data, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !ncol(df) || !nrow(df))
    return(shiny::div(class = "ara-result-empty", empty_message))
  n_all <- nrow(df)
  limit <- if (is.null(max_rows)) n_all else min(n_all, max(1L, as.integer(max_rows)))
  shown <- analysis_display_df(utils::head(df, limit))
  shiny::div(
    class = "ara-result-block",
    shiny::div(class = "ara-table-scroll", shiny::tags$table(
      class = "table table-sm table-striped ara-result-table",
      shiny::tags$thead(shiny::tags$tr(lapply(names(shown), shiny::tags$th))),
      shiny::tags$tbody(lapply(seq_len(nrow(shown)), function(i) {
        shiny::tags$tr(lapply(shown[i, , drop = FALSE], function(col) {
          v <- col[[1L]]
          shiny::tags$td(if (length(v) == 0L || is.na(v)) "—" else as.character(v))
        }))
      }))
    )),
    if (n_all > limit) shiny::div(class = "ara-table-note",
      sprintf("Menampilkan %s dari %s baris. Ekspor Excel menyertakan seluruh baris.",
              format(limit, big.mark = "."), format(n_all, big.mark = ".")))
  )
}

ara_collapsible_result_tables <- function(items, max_rows = 50L,
                                          empty_message = "Belum ada tabel hasil.") {
  if (is.null(items) || !length(items)) return(shiny::p(empty_message))
  items <- items[vapply(items, is.data.frame, logical(1))]
  if (!length(items)) return(shiny::p(empty_message))
  shiny::div(class = "ara-result-stack", lapply(seq_along(items), function(i) {
    nm <- names(items)[i]
    if (is.null(nm) || is.na(nm) || !nzchar(nm)) nm <- paste("Tabel", i)
    shiny::tags$details(class = "ara-result-section", open = if (i == 1L) "open" else NULL,
      shiny::tags$summary(gsub("_", " ", nm, fixed = TRUE)),
      ara_html_result_table(items[[i]], max_rows = max_rows))
  }))
}
