mod_home_ui <- function(id) {
  ns <- NS(id)

  feature_card <- function(input_id, icon_name, icon_class, title, text) {
    tags$a(
      href = "#",
      class = "rsl3-card",
      onclick = sprintf(
        "Shiny.setInputValue('%s', Date.now(), {priority: 'event'}); return false;",
        ns(input_id)
      ),
      div(class = paste("rsl3-icon", icon_class), shiny::icon(icon_name)),
      div(
        class = "rsl3-copy",
        h3(title),
        p(text)
      ),
      div(class = "rsl3-arrow", shiny::icon("arrow-right"))
    )
  }

  div(
    class = "rsl3-home",

    div(
      class = "rsl3-hero-row",
      div(
        class = "rsl3-hero",
        div(
          class = "rsl3-ready",
          shiny::icon("circle-check"),
          "Offline Ready • Advanced Analysis"
        ),
        h1("Advanced Research Analytics"),
        p(
          "Analisis statistik tingkat lanjut untuk penelitian pendidikan dan ilmu sosial ",
          "dengan alur kerja yang sederhana, terarah, dan mudah digunakan."
        ),
        div(
          class = "rsl3-actions",
          actionButton(
            ns("go_analysis"),
            tagList(shiny::icon("play"), "Mulai Analisis"),
            class = "btn rsl3-primary"
          ),
          actionButton(
            ns("go_data"),
            tagList(shiny::icon("table-cells"), "Buka Data"),
            class = "btn rsl3-secondary"
          )
        )
      ),
      div(
        class = "rsl3-logo-panel",
        tags$img(src = "app_icon_256.png", class = "home-app-icon", alt = "App Icon")
      )
    ),

    div(
      class = "rsl3-grid rsl3-grid-4",
      feature_card("go_hlm", "chart-column", "blue", "Multilevel / HLM",
                   "ICC, random effects, cross-level interaction, dan BLUP."),
      feature_card("go_ipd", "database", "green", "IPD Meta-Analysis",
                   "Two-Stage, One-Stage, heterogeneity, dan meta-regression."),
      feature_card("go_multi", "share-nodes", "red", "Multivariate",
                   "MANOVA, MANCOVA, discriminant analysis, dan cluster analysis."),
      feature_card("go_sem", "diagram-project", "purple", "SEM / CFA",
                   "CFA, SEM, multi-group, dan measurement invariance.")
    ),

    div(
      class = "rsl3-grid rsl3-grid-3",
      feature_card("go_public", "database", "amber", "Data & Dataset Publik",
                   "Asesmen Nasional, OECD/PISA, data contoh, dan template."),
      feature_card("go_guide", "book-open", "cyan", "Panduan Analisis",
                   "Langkah penggunaan, contoh kasus, dan alur analisis."),
      feature_card("go_export", "file-excel", "slate", "Template & Ekspor",
                   "Template data dan seluruh tabel hasil dalam Excel.")
    ),

    div(
      class = "rsl3-bottom",
      div(
        class = "rsl3-dataset",
        div(class = "rsl3-dataset-icon", shiny::icon("database")),
        div(
          class = "rsl3-dataset-content",
          h3("Dataset Aktif"),
          uiOutput(ns("dataset_status"))
        )
      ),
      div(
        class = "rsl3-quick",
        h3("Mulai Cepat"),
        div(
          class = "rsl3-quick-row",
          div(class="rsl3-qstep", div(class="rsl3-qnum","1"), div(strong("Buka Data"), span("Muat dataset"))),
          div(class="rsl3-qstep", div(class="rsl3-qnum","2"), div(strong("Pilih Analisis"), span("Tentukan variabel"))),
          div(class="rsl3-qstep", div(class="rsl3-qnum","3"), div(strong("Ekspor Hasil"), span("Simpan ke Excel")))
        )
      )
    )
  )
}

mod_home_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    nav_to <- function(x) {
      updateNavbarPage(session = session$rootScope(), inputId = "main_nav", selected = x)
    }
    observeEvent(input$go_analysis, nav_to("Multilevel / HLM"))
    observeEvent(input$go_data, nav_to("Data"))
    observeEvent(input$go_hlm, nav_to("Multilevel / HLM"))
    observeEvent(input$go_ipd, nav_to("IPD Meta-Analysis"))
    observeEvent(input$go_multi, nav_to("Multivariate"))
    observeEvent(input$go_sem, nav_to("SEM / CFA"))
    observeEvent(input$go_public, nav_to("Data"))
    observeEvent(input$go_guide, nav_to("Panduan Analisis"))
    observeEvent(input$go_export, {
      showNotification(
        "Ekspor tersedia langsung di setiap modul analisis melalui tombol 'Ekspor Semua Tabel ke Excel'.",
        type = "message",
        duration = 7
      )
    })

    output$dataset_status <- renderUI({
      if (is.null(shared$data)) {
        div(class = "rsl3-no-data", "Belum ada dataset. Buka menu Data untuk memulai.")
      } else {
        tagList(
          div(class = "rsl3-dataset-name", shared$data_name),
          div(
            class = "rsl3-dataset-meta",
            span(paste(format(nrow(shared$data), big.mark=","), "baris")),
            span("•"), span(paste(ncol(shared$data), "variabel")),
            span("•"), span(paste0(round(as.numeric(object.size(shared$data))/(1024^2),1), " MB"))
          )
        )
      }
    })
  })
}
