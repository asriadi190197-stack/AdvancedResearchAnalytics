# AdvancedResearchAnalytics 1.1.4 - Windows installation and launch
# Run this entire file with source("install_and_run.R") in RStudio.
local({
  repo <- "asriadi190197-stack/AdvancedResearchAnalytics"
  name <- "AdvancedResearchAnalytics"
  if (getRversion() < "4.2.0") stop("Diperlukan R 4.2.0 atau lebih baru.", call. = FALSE)
  options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 600)
  if (.Platform$OS.type == "windows") options(pkgType = "win.binary", install.packages.check.source = "no")
  packages <- c("shiny", "shinyjs", "bslib", "DT", "future", "promises", "data.table",
                "readxl", "haven", "lme4", "lmerTest", "performance", "parameters",
                "metafor", "lavaan", "semPlot", "MASS", "cluster", "openxlsx")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Memasang dependensi R: ", paste(missing, collapse = ", "))
    utils::install.packages(missing, dependencies = c("Depends", "Imports", "LinkingTo"),
                            type = if (.Platform$OS.type == "windows") "binary" else "source")
  }
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Dependensi belum dapat dimuat: ", paste(missing, collapse = ", "),
                            ". Jika DLL diblokir oleh Application Control, hubungi admin Windows.", call. = FALSE)

  # Download the source ZIP directly via HTTPS; Git, pak and remotes not needed.
  work <- tempfile("ARA_install_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  branches <- c("main", "master")
  source_dir <- NULL
  for (branch in branches) {
    archive <- file.path(work, paste0(branch, ".zip"))
    url <- sprintf("https://github.com/%s/archive/refs/heads/%s.zip", repo, branch)
    ok <- tryCatch({
      utils::download.file(url, archive, mode = "wb", quiet = TRUE)
      utils::unzip(archive, exdir = file.path(work, branch))
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    folder <- file.path(work, branch, paste0(name, "-", branch))
    if (ok && file.exists(file.path(folder, "DESCRIPTION"))) {
      source_dir <- folder
      break
    }
  }
  if (is.null(source_dir)) stop("Gagal mengunduh kode sumber dari GitHub. Periksa jaringan dan nama branch.", call. = FALSE)

  # R CMD INSTALL uses base R's package installer. The package itself contains no C/C++.
  r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  result <- suppressWarnings(system2(r_bin, c("CMD", "INSTALL", shQuote(source_dir)),
                                      stdout = TRUE, stderr = TRUE))
  cat(paste(result, collapse = "\n"), "\n")
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) stop("R CMD INSTALL gagal, kode: ", status, call. = FALSE)
  if (!requireNamespace(name, quietly = TRUE)) stop("Paket tidak dapat dimuat setelah instalasi.", call. = FALSE)
  message("AdvancedResearchAnalytics terpasang, versi ", as.character(utils::packageVersion(name)))
  AdvancedResearchAnalytics::run_app()
})
