quick_start_card <- function(ns, title, description, source_text) {
  card(
    class = "quick-start-card",
    card_header(title),
    p(description),
    div(class="quick-start-source", source_text),
    div(class="quick-start-actions",
      actionButton(ns("load_example"), "Gunakan Data Contoh", class="btn btn-primary"),
      downloadButton(ns("download_template"), "Unduh Template", class="btn btn-outline-secondary"),
      actionButton(ns("open_public"), "Dataset Publik", class="btn btn-outline-primary")
    ),
    uiOutput(ns("quick_status"))
  )
}
load_example_csv <- function(path, shared, name, source, notes) {
  x <- utils::read.csv(path, check.names=FALSE, stringsAsFactors=FALSE, na.strings=c("", "NA", "NULL"))
  names(x) <- make.names(names(x), unique=TRUE)
  shared$data <- x; shared$data_name <- name
  shared$provenance <- new_dataset_provenance(name=name, source=source, local_path=path, notes=notes)
  invisible(x)
}
quick_start_server <- function(input, output, session, shared, example_path, example_name, source, notes, template_path, preset_fun=NULL) {
  status <- reactiveVal("")
  observeEvent(input$load_example, {
    x <- tryCatch(load_example_csv(example_path, shared, example_name, source, notes), error=function(e)e)
    if (inherits(x,"error")) status(paste("Gagal memuat contoh:",x$message)) else {
      status(paste("Data contoh aktif:",example_name,"—",nrow(x),"baris ×",ncol(x),"variabel."))
      if (!is.null(preset_fun)) preset_fun(x)
    }
  })
  observeEvent(input$open_public, { updateNavbarPage(session=session$rootScope(), inputId="main_nav", selected="Data") })
  output$quick_status <- renderUI({ if (!nzchar(status())) return(NULL); div(class="success-box",status()) })
  output$download_template <- downloadHandler(filename=function() basename(template_path), content=function(file) { if (!file.exists(template_path)) stop("Template tidak ditemukan: ", template_path); if (!isTRUE(file.copy(template_path,file,overwrite=TRUE))) stop("Gagal menyalin template") })
}
