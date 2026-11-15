load_public_catalog <- function() {
  utils::read.csv(
    "data/public_dataset_catalog.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

safe_dataset_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  gsub("_+", "_", x)
}

download_public_dataset <- function(row, data_dir = APP_DATA_DIR) {
  if (is.null(row$download_url) || !nzchar(row$download_url)) {
    stop("Dataset ini tidak memiliki direct-download URL. Gunakan Open Source Page.")
  }

  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  ext <- tools::file_ext(row$download_url)
  if (!nzchar(ext)) ext <- "dat"

  out <- file.path(
    data_dir,
    paste0(safe_dataset_filename(row$id), ".", ext)
  )

  utils::download.file(
    row$download_url,
    destfile = out,
    mode = "wb",
    quiet = FALSE,
    method = "libcurl"
  )

  out
}

extract_downloaded_zip <- function(zip_path, data_dir = APP_DATA_DIR) {
  if (tolower(tools::file_ext(zip_path)) != "zip") {
    return(dirname(zip_path))
  }

  target <- file.path(
    data_dir,
    tools::file_path_sans_ext(basename(zip_path))
  )

  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = target)

  target
}

list_cached_datasets <- function(data_dir = APP_DATA_DIR) {
  if (!dir.exists(data_dir)) return(data.frame())

  files <- list.files(
    data_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(files) == 0) return(data.frame())

  info <- file.info(files)
  data.frame(
    File = basename(files),
    Path = files,
    Size_MB = round(info$size / (1024^2), 2),
    Modified = info$mtime,
    stringsAsFactors = FALSE
  )
}

open_external_url <- function(url) {
  if (!nzchar(url)) stop("URL tidak tersedia.")
  utils::browseURL(url)
}

detect_pisa_columns <- function(df) {
  nms <- names(df)

  list(
    plausible_values = grep("^PV[0-9]+", nms, value = TRUE, ignore.case = TRUE),
    weights = grep("W_FSTUWT|W_FSCHWT|W_FTCHWT|W_FSTURWT", nms, value = TRUE, ignore.case = TRUE),
    school_id = grep("CNTSCHID|SCHOOLID", nms, value = TRUE, ignore.case = TRUE),
    country_id = grep("^CNT$|CNTRYID|CNTSTUID", nms, value = TRUE, ignore.case = TRUE)
  )
}
