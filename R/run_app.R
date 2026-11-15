#' Run the Advanced Research Analytics application
#'
#' Runs the bundled Shiny application from the installed package.
#' Optional analysis and export packages are only necessary when their
#' corresponding features are used.
#'
#' @param launch.browser Whether to open a web browser automatically.
#' @param port Optional local port. NULL lets Shiny select an available port.
#' @param host Loopback address. Keep at 127.0.0.1 for local use.
#' @return Invisibly returns the result of shiny::runApp().
#' @export
run_app <- function(launch.browser = interactive(), port = NULL,
                    host = "127.0.0.1") {
  app_dir <- system.file("app", package = "AdvancedResearchAnalytics")
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop("Berkas aplikasi tidak ditemukan. Instal ulang paket AdvancedResearchAnalytics.", call. = FALSE)
  }
  needed <- c("shiny", "shinyjs", "bslib", "DT", "future", "promises")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Dependensi startup belum terpasang: ", paste(missing, collapse = ", "),
         ". Instal ulang paket beserta dependencies=TRUE.", call. = FALSE)
  }
  if (!is.null(port) && (!is.numeric(port) || length(port) != 1L ||
                        is.na(port) || port < 1 || port > 65535)) {
    stop("port harus NULL atau angka antara 1 dan 65535.", call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, port = port,
                host = host, launch.browser = launch.browser,
                display.mode = "normal")
}

#' Check package dependencies
#' @param include_optional Include additional packages used in optional features.
#' @return A data.frame with package availability.
#' @export
check_dependencies <- function(include_optional = TRUE) {
  core <- c("shiny", "shinyjs", "bslib", "DT", "future", "promises",
            "data.table", "readxl", "haven", "lme4", "lmerTest",
            "performance", "parameters", "metafor", "lavaan",
            "semPlot", "MASS", "cluster", "openxlsx")
  optional <- c("clubSandwich", "flextable", "officer", "rstudioapi")
  pkgs <- if (isTRUE(include_optional)) c(core, optional) else core
  data.frame(package = pkgs,
             role = ifelse(pkgs %in% core, "Required", "Optional"),
             installed = vapply(pkgs, requireNamespace, logical(1), quietly = TRUE),
             row.names = NULL)
}
