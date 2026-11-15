init_async_backend <- function() {
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("promises", quietly = TRUE)) {
    stop(
      "Paket 'future' dan 'promises' diperlukan untuk analisis latar belakang. ",
      "Jalankan scripts/install_dependencies.R."
    )
  }

  # Two worker processes are enough for an interactive desktop app:
  # one analysis can run while Shiny remains responsive, with one spare queue slot.
  workers <- 1L
  future::plan(future::multisession, workers = workers)

  # Large public datasets can exceed future's default globals limit.
  options(future.globals.maxSize = 3 * 1024^3)

  invisible(workers)
}

background_start_notification <- function(label) {
  shiny::showNotification(
    paste0(
      label,
      " sedang berjalan di latar belakang. ",
      "Anda tetap dapat membuka menu lain sementara analisis diproses."
    ),
    type = "message",
    duration = 7
  )
}

background_success_notification <- function(label) {
  shiny::showNotification(
    paste0(label, " selesai. Output sudah tersedia."),
    type = "message",
    duration = 10
  )
}

background_error_notification <- function(label, error) {
  shiny::showNotification(
    paste0(label, " gagal: ", conditionMessage(error)),
    type = "error",
    duration = NULL
  )
}


try_acquire_analysis <- function(shared, label) {
  if (isTRUE(shared$analysis_busy)) {
    running <- shared$analysis_name %||% "analisis lain"
    shiny::showNotification(
      paste0(
        "Analisis '", running, "' masih berjalan. ",
        "Untuk menjaga stabilitas dan penggunaan RAM, tunggu sampai selesai sebelum menjalankan '",
        label, "'. Anda tetap dapat membuka menu lain."
      ),
      type = "warning",
      duration = 10
    )
    return(FALSE)
  }

  shared$analysis_busy <- TRUE
  shared$analysis_name <- label
  shared$analysis_started_at <- Sys.time()
  TRUE
}

release_analysis <- function(shared) {
  shared$analysis_busy <- FALSE
  shared$analysis_name <- NULL
  shared$analysis_started_at <- NULL
  invisible(TRUE)
}
