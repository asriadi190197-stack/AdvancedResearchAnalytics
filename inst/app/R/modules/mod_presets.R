mod_presets_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class="page-heading",
      tags$span(class="eyebrow","REPRODUCIBLE WORKFLOWS"),

    usage_guide(
      "Cara menggunakan Preset Analisis",
      c(
        "Masukkan nama preset yang mudah dikenali.",
        "Pilih jenis analisis.",
        "Tuliskan konfigurasi penting seperti outcome, cluster, prediktor, atau syntax model.",
        "Klik Simpan Preset.",
        "Gunakan daftar preset untuk mendokumentasikan dan mengulang konfigurasi analisis."
      )
    ),

      h2("Preset Analisis"),
      p("Save reusable analysis configurations without saving the full dataset.")
    ),
    layout_columns(
      col_widths=c(5,7),
      card(
        card_header("Simpan Preset"),
        textInput(ns("name"),"Nama preset"),
        selectInput(
          ns("type"),
          "Jenis analisis",
          choices=c("HLM","Two-Stage IPD","One-Stage IPD","CFA","SEM","MANOVA/MANCOVA","Discriminant","Cluster")
        ),
        textAreaInput(
          ns("settings"),
          "Pengaturan / catatan",
          rows=10,
          placeholder="Example:\noutcome = GD.2.5\ncluster = kd_sekolah\nL1 = GD.2.4, GE.3.3\nL2 = GE.3.4"
        ),
        actionButton(ns("save"),"Simpan Preset",class="btn btn-primary w-100"),
        verbatimTextOutput(ns("status"))
      ),
      card(
        card_header("Preset Tersimpan"),
        actionButton(ns("refresh"),"Muat Ulang",class="btn btn-outline-secondary"),
        br(),br(),
        DTOutput(ns("table"))
      )
    )
  )
}

mod_presets_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    status <- reactiveVal("")
    ver <- reactiveVal(0)

    observeEvent(input$save, {
      p <- tryCatch(
        save_analysis_preset(
          input$name,
          input$type,
          list(text = input$settings)
        ),
        error=function(e)e
      )
      if (inherits(p,"error")) {
        status(paste("Save failed:",p$message))
      } else {
        status(paste("Saved:",p))
        ver(ver()+1)
      }
    })

    observeEvent(input$refresh, ver(ver()+1))

    output$status <- renderText(status())

    output$table <- renderDT({
      ver()
      x <- list_analysis_presets()
      if (is.null(x) || nrow(x)==0) return(ara_datatable(data.frame(Status="No presets saved."),rownames=FALSE))
      ara_datatable(x,rownames=FALSE,options=list(scrollX=TRUE,pageLength=10))
    })
  })
}
