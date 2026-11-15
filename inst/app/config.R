APP_NAME <- "Advanced Research Analytics"
APP_SUBTITLE <- "Advanced Statistical Modeling for Education & Social Research"
APP_VERSION <- "1.1.3"

# Path lintas-platform: Mac/Linux menggunakan HOME, Windows USERPROFILE bila tersedia.
ara_home <- path.expand("~")
APP_ROOT_DIR <- file.path(ara_home, "Documents", "AdvancedResearchAnalytics")
if (!dir.exists(file.path(ara_home, "Documents"))) {
  APP_ROOT_DIR <- file.path(ara_home, "AdvancedResearchAnalytics")
}

APP_DATA_DIR <- file.path(APP_ROOT_DIR, "Datasets")
APP_PRESET_DIR <- file.path(APP_ROOT_DIR, "Presets")
APP_EXPORT_DIR <- file.path(APP_ROOT_DIR, "Exports")

for (p in c(APP_ROOT_DIR, APP_DATA_DIR, APP_PRESET_DIR, APP_EXPORT_DIR)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
}


# Maximum Shiny upload size.
# 2048 MB is intentionally generous for large educational datasets.
# Users should still prefer reduced extracts for very large PISA files.
options(shiny.maxRequestSize = 2048 * 1024^2)
