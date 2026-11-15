library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(future)
library(promises)

source("config.R")
source("R/utils/data_utils.R")
source("R/utils/public_data.R")
source("R/utils/help_ui.R")
source("R/utils/ui_helpers.R")
source("R/utils/excel_export.R")
source("R/utils/structured_output.R")
source("R/utils/quick_start.R")
source("R/utils/dependencies.R")
source("R/utils/provenance.R")
source("R/utils/presets.R")
source("R/utils/large_data.R")
source("R/utils/async_utils.R")
source("R/engines/engine_hlm.R")
source("R/engines/engine_hlm_advanced.R")
source("R/engines/engine_ipd.R")
source("R/engines/engine_ipd_advanced.R")
source("R/engines/engine_sem.R")
source("R/engines/engine_multivariate.R")
source("R/engines/engine_assumptions.R")

source("R/modules/mod_home.R")
source("R/modules/mod_data.R")
source("R/modules/mod_hlm.R")
source("R/modules/mod_assumptions.R")
source("R/modules/mod_ipd.R")
source("R/modules/mod_multivariate.R")
source("R/modules/mod_sem.R")
source("R/modules/mod_learn.R")
source("R/modules/mod_reports.R")
source("R/modules/mod_about.R")
source("R/modules/mod_pisa.R")
source("R/modules/mod_presets.R")


ASYNC_WORKERS <- init_async_backend()

ui <- page_navbar(
  title = div(
    class = "brand-wrap",
    div(class = "brand-mark", tags$img(src = "app_icon_64.png", class = "brand-logo-img", alt = "Advanced Research Analytics")),
    div(
      div(class = "brand-title", "Advanced Research Analytics"),
      div(class = "brand-subtitle", paste0("v", APP_VERSION))
    )
  ),
  id = "main_nav",
  collapsible = TRUE,
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles_v091.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles_v092.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles_v1212.css"),
    tags$script(src = "tables_v1212.js"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", href = "app_icon_256.png"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    )
  ),

  nav_panel("Beranda", mod_home_ui("home")),
  nav_panel("Data", mod_data_ui("data")),

  nav_menu(
    "Analisis",
    nav_panel("Multilevel / HLM", mod_hlm_ui("hlm")),
    nav_panel("IPD Meta-Analysis", mod_ipd_ui("ipd")),
    nav_panel("Multivariate", mod_multivariate_ui("multi")),
    nav_panel("Uji Asumsi Lanjutan", mod_assumptions_ui("assumptions")),
    nav_panel("SEM / CFA", mod_sem_ui("sem"))
  ),

  nav_menu(
    "Panduan",
    nav_panel("Panduan Analisis", mod_learn_ui("learn")),
    nav_panel("PISA Tools", mod_pisa_ui("pisa"))
  ),

  nav_spacer(),
  nav_item(uiOutput("global_analysis_status")),
  nav_panel("Tentang", mod_about_ui("about"))
)

server <- function(input, output, session) {
  shared <- reactiveValues(
    data = NULL,
    data_name = NULL,
    provenance = NULL,
    codebook = NULL,
    import_meta = NULL,
    auto_map = NULL,
    mapping_token = 0,
    analysis_busy = FALSE,
    analysis_name = NULL,
    analysis_started_at = NULL
  )

  mod_home_server("home", shared)
  mod_data_server("data", shared)
  mod_hlm_server("hlm", shared)
  mod_assumptions_server("assumptions", shared)
  mod_ipd_server("ipd", shared)
  mod_multivariate_server("multi", shared)
  mod_sem_server("sem", shared)
  mod_learn_server("learn", shared)
  mod_pisa_server("pisa", shared)
  mod_about_server("about", shared)
}

shinyApp(ui, server)
