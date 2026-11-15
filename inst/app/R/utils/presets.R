save_analysis_preset <- function(name, analysis_type, settings, dir = APP_PRESET_DIR) {
  if (!nzchar(name)) stop("Preset name is required.")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  safe <- gsub("[^A-Za-z0-9._-]+", "_", name)
  obj <- list(
    name = name,
    analysis_type = analysis_type,
    saved_at = as.character(Sys.time()),
    settings = settings
  )

  path <- file.path(dir, paste0(safe, ".rds"))
  saveRDS(obj, path)
  path
}

list_analysis_presets <- function(dir = APP_PRESET_DIR) {
  if (!dir.exists(dir)) return(data.frame())
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) return(data.frame())

  rows <- lapply(files, function(f) {
    x <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(x)) return(NULL)
    data.frame(
      Name = x$name,
      Analysis = x$analysis_type,
      Saved = x$saved_at,
      Path = f,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

load_analysis_preset <- function(path) {
  readRDS(path)
}
