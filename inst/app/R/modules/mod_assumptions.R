mod_assumptions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    div(class = "page-heading", h2("Laboratorium Uji Asumsi"),
      p("Pemeriksaan tambahan untuk analisis multivariat, regresi, dan penyiapan SEM/CFA.")),
    bslib::layout_columns(col_widths = c(4,8),
      bslib::card(
        bslib::card_header("Konfigurasi"),
        shiny::selectizeInput(ns("variables"), "Variabel numerik", choices = NULL, multiple=TRUE,
          options = list(plugins=list("remove_button"))),
        shiny::checkboxGroupInput(ns("tests"), "Pilih uji", choices = c(
          "Statistik deskriptif, missing, skewness, kurtosis"="descriptive",
          "Korelasi"="correlation", "KMO dan Bartlett"="kmo",
          "VIF dan tolerance"="vif", "Shapiro-Wilk sampel"="normality"),
          selected = c("descriptive","correlation","kmo","vif")),
        shiny::actionButton(ns("run"), "Hitung Uji", class="btn btn-primary w-100"),
        shiny::downloadButton(ns("export"), "Ekspor Excel", class="btn btn-outline-secondary w-100")
      ),
      bslib::card(
        bslib::card_header("Hasil dan status pengujian"),
        bslib::navset_card_tab(
          bslib::nav_panel("Tabel Hasil", shiny::uiOutput(ns("result_ui"))),
          bslib::nav_panel("Sintaks R", shiny::verbatimTextOutput(ns("syntax")))
        )
      )
    )
  )
}
mod_assumptions_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    current <- shiny::reactiveVal(NULL)
    shiny::observeEvent(shared$mapping_token, {
      shiny::req(shared$data)
      nums <- numeric_vars(shared$data)
      shiny::updateSelectizeInput(session, "variables", choices=nums,
        selected=head(nums, 4L), server=TRUE)
    }, ignoreInit=FALSE)
    shiny::observeEvent(input$run, {
      shiny::req(shared$data)
      vars <- input$variables %||% character()
      if (!length(vars)) { shiny::showNotification("Pilih variabel.", type="warning"); return() }
      if (!try_acquire_analysis(shared, "Uji Asumsi")) return()
      dat <- shared$data
      tests <- input$tests %||% character()
      shinyjs::disable("run")
      p <- promises::future_promise(ara_assumptions(dat, vars, tests), seed=TRUE)
      promises::then(p, onFulfilled=function(z) {
        current(z); shinyjs::enable("run"); release_analysis(shared)
        shiny::showNotification("Uji asumsi selesai.", type="message"); NULL
      }, onRejected=function(e) {
        shinyjs::enable("run"); release_analysis(shared)
        background_error_notification("Uji asumsi", e); NULL
      })
    })
    output$syntax <- shiny::renderText({
      vars <- input$variables %||% character()
      tests <- input$tests %||% character()
      paste0("# Jalankan dengan data aktif (data.frame bernama data)\n",
        "source('R/engines/engine_assumptions.R')\n",
        "hasil <- ara_assumptions(data, variables = c(",
        paste(sprintf("'%s'", vars), collapse = ", "),
        "), tests = c(", paste(sprintf("'%s'", tests), collapse = ", "), "))")
    })
    output$result_ui <- shiny::renderUI({
      z <- current()
      if (is.null(z)) return(shiny::p("Jalankan uji untuk melihat hasil."))
      ara_collapsible_result_tables(z, max_rows = 50L)
    })

    output$export <- shiny::downloadHandler(
      filename=function() paste0("ARA_Uji_Asumsi_",Sys.Date(),".xlsx"),
      content=function(file) {
        z <- current(); shiny::req(z)
        tabs <- z[vapply(z, is.data.frame, logical(1))]
        export_analysis_excel(tabs, file)
      })
  })
}
