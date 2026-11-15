mod_reports_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      class="page-heading",
      tags$span(class="eyebrow","PUBLICATION OUTPUT"),

    usage_guide(
      "Cara menggunakan Laporan",
      c(
        "Pastikan dataset aktif memiliki provenance yang benar.",
        "Periksa sumber data, waktu impor, dan catatan dataset.",
        "Export hasil utama dilakukan dari masing-masing modul analisis.",
        "Gunakan halaman ini untuk memastikan informasi asal data terdokumentasi sebelum publikasi."
      )
    ),

      h2("Laporan & Provenance"),
      p("Review active-dataset provenance before exporting analysis results.")
    ),
    layout_columns(
      col_widths=c(7,5),
      card(
        card_header("Provenance Dataset"),
        DTOutput(ns("provenance"))
      ),
      card(
        card_header("Folder Ekspor"),
        verbatimTextOutput(ns("export_path")),
        p(class="text-muted","Analisis-specific Word export buttons remain inside each statistical module.")
      )
    )
  )
}

mod_reports_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    output$provenance <- renderDT({
      x <- provenance_table(shared$provenance)
      if (nrow(x)==0) return(ara_datatable(data.frame(Status="No active dataset provenance."),rownames=FALSE))
      ara_datatable(x,rownames=FALSE,options=list(dom="t"))
    })

    output$export_path <- renderText({
      normalizePath(APP_EXPORT_DIR,winslash="/",mustWork=FALSE)
    })
  })
}
