# Dependencies needed at startup versus optional procedures.
# clubSandwich is optional until the user requests CR2 cluster-robust inference.
ARA_CORE_PACKAGES <- c(
  "shiny", "shinyjs", "bslib", "DT", "future", "promises", "data.table",
  "readxl", "haven", "lme4", "lmerTest", "performance", "parameters",
  "metafor", "lavaan", "semPlot", "MASS", "cluster", "openxlsx"
)
ARA_OPTIONAL_PACKAGES <- c("clubSandwich")
ARA_REQUIRED_PACKAGES <- unique(c(ARA_CORE_PACKAGES, ARA_OPTIONAL_PACKAGES))

dependency_status <- function() {
  ok <- vapply(ARA_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  data.frame(Package = ARA_REQUIRED_PACKAGES,
             Type = ifelse(ARA_REQUIRED_PACKAGES %in% ARA_CORE_PACKAGES, "Core", "Optional"),
             Installed = ok, Status = ifelse(ok, "Ready", "Missing"),
             stringsAsFactors = FALSE)
}

missing_dependencies <- function(include_optional = TRUE) {
  x <- dependency_status()
  if (!isTRUE(include_optional)) x <- x[x$Type == "Core", , drop = FALSE]
  x$Package[!x$Installed]
}

# A separate writable user library works with ordinary RStudio as well as
# portable builds whose bundled R/library may be read-only.
ara_user_package_library <- function() {
  version <- paste(R.version$major, strsplit(R.version$minor, "\\.")[[1L]][1L], sep = ".")
  configured <- Sys.getenv("ARA_PACKAGE_LIBRARY", unset = "")
  root <- if (nzchar(configured)) configured else
    file.path(path.expand("~"), "AdvancedResearchAnalytics", "R_packages", paste0("R-", version))
  if (!dir.exists(root) && !dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
    stop("Tidak dapat membuat folder paket pengguna: ", root)
  }
  if (file.access(root, 2) != 0) stop("Folder paket tidak dapat ditulis: ", root)
  root
}

ara_activate_package_library <- function() {
  lib <- ara_user_package_library()
  .libPaths(unique(c(lib, .libPaths())))
  # Future multisession workers launch in fresh R processes: expose the same
  # user library through the environment before the workers are created.
  existing <- Sys.getenv("R_LIBS_USER", unset = "")
  existing <- if (nzchar(existing)) strsplit(existing, .Platform$path.sep, fixed = TRUE)[[1L]] else character()
  Sys.setenv(R_LIBS_USER = paste(unique(c(lib, existing[nzchar(existing)])),
                                collapse = .Platform$path.sep))
  invisible(lib)
}

ara_install_packages <- function(packages, repos = "https://cloud.r-project.org") {
  packages <- intersect(unique(as.character(packages)), ARA_REQUIRED_PACKAGES)
  if (!length(packages)) return(list(ok = TRUE, installed = character(), missing = character(), message = "Tidak ada paket dipilih."))
  lib <- ara_activate_package_library()
  before <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  error_message <- NULL
  if (length(before)) {
    tryCatch(utils::install.packages(before, lib = lib, repos = repos,
      dependencies = c("Depends", "Imports", "LinkingTo")),
      error = function(e) { error_message <<- conditionMessage(e) })
  }
  # install.packages may only warn and return; always verify actual loadability.
  still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  list(ok = !length(still_missing), installed = setdiff(before, still_missing),
       missing = still_missing,
       message = if (length(still_missing)) {
         paste0("Paket belum dapat digunakan: ", paste(still_missing, collapse = ", "),
           if (!is.null(error_message)) paste0(". Penyebab: ", error_message) else
           ". Periksa koneksi internet, izin folder paket, dan kompatibilitas versi R.")
       } else "Semua paket yang dipilih berhasil diverifikasi.")
}
