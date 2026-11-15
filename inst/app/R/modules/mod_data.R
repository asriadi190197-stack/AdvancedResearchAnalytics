mod_data_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "page-heading",
      tags$span(class = "eyebrow", "DATA WORKSPACE"),

    usage_guide(
      "Cara menggunakan Manajemen Data",
      c(
        "Klik Pilih file data untuk memilih CSV, Excel, SPSS, Stata, atau RDS.",
        "Setelah data masuk, periksa jumlah baris, variabel, missing values, dan preview data.",
        "Gunakan Kamus Variabel untuk memeriksa tipe setiap variabel.",
        "Jika perlu, ubah tipe variabel atau lakukan complete-case cleaning.",
        "Untuk file PISA besar, gunakan Data Latihan Publik atau Alat PISA agar dapat membuat extract yang lebih kecil."
      ),
      "Batas upload aplikasi saat ini dinaikkan hingga 2 GB. Untuk file yang sangat besar, extract variabel yang dibutuhkan tetap lebih disarankan."
    ),

      h2("Manajemen Data"),
      p("Import local data, access public training datasets, and manage the local large-data cache.")
    ),


    section_header(
      "SUMBER DATA",
      "Pilih cara memulai",
      "Gunakan data Anda sendiri atau mulai dari contoh/template agar alur analisis lebih mudah dipahami."
    ),

    div(
      class = "ara-card-grid ara-card-grid-3",
      quick_action_card(
        "Data Anda",
        "Impor CSV, Excel, SPSS, Stata, atau RDS.",
        badge = "IMPORT"
      ),
      quick_action_card(
        "Data Contoh & Template",
        "Gunakan subset kecil dan template struktur variabel untuk berlatih.",
        badge = "DEMO"
      ),
      quick_action_card(
        "Dataset Publik",
        "Akses katalog Asesmen Nasional dan OECD/PISA tanpa membebani installer.",
        badge = "PUBLIC"
      )
    ),

    navset_card_tab(
      nav_panel(
        "Impor & Persiapan",
        layout_columns(
          col_widths = c(4, 8),

          card(
            card_header("Impor Dataset"),
            fileInput(
              ns("file"),
              "Pilih file data",
              accept = c(".csv", ".xlsx", ".xls", ".sav", ".dta", ".rds")
            ),
            p(class = "text-muted small", "Maksimum upload: 2 GB. File SPSS/PISA yang besar dapat menggunakan RAM yang cukup besar."),
            uiOutput(ns("import_status")),
            uiOutput(ns("smart_import_status")),
            hr(),

            selectInput(ns("var_to_convert"), "Variabel", choices = NULL),
            selectInput(
              ns("new_type"),
              "Ubah tipe menjadi",
              choices = c("Numerik" = "numeric", "Faktor" = "factor", "Karakter" = "character")
            ),
            actionButton(ns("convert"), "Ubah Tipe", class = "btn btn-outline-primary w-100"),

            hr(),

            checkboxInput(ns("drop_missing"), "Hapus baris yang memiliki missing value", FALSE),
            actionButton(ns("apply_clean"), "Terapkan Pembersihan", class = "btn btn-outline-secondary w-100")
          ),

          card(
            card_header("Ringkasan Dataset"),
            uiOutput(ns("overview")),
            DTOutput(ns("preview"))
          )
        ),

        card(
          card_header("Kamus Variabel"),
          DTOutput(ns("dictionary"))
        )
      ),

      nav_panel(
        "Data Latihan Publik",
        layout_columns(
          col_widths = c(7, 5),

          card(
            card_header("Katalog Dataset Publik Resmi"),
            DTOutput(ns("public_catalog"))
          ),

          card(
            card_header("Aksi Dataset"),
            selectInput(ns("public_id"), "Dataset", choices = NULL),
            uiOutput(ns("public_details")),
            actionButton(ns("open_source"), "Open Official Source / Access Page", class = "btn btn-outline-primary w-100"),
            br(), br(),
            uiOutput(ns("download_action")),
            verbatimTextOutput(ns("download_status"))
          )
        )
      ),

      nav_panel(
        "Penyimpanan Lokal",
        card(
          card_header("Penyimpanan Data Besar"),
          p(class = "text-muted",
            paste0("Cached files are stored outside the application so updates do not delete them.")),
          verbatimTextOutput(ns("cache_path")),
          actionButton(ns("refresh_cache"), "Muat Ulang Daftar", class = "btn btn-outline-secondary"),
          br(), br(),
          selectInput(ns("cache_file"), "Load cached data file", choices = NULL),
          actionButton(ns("load_cache"), "Load Selected Cached File", class = "btn btn-primary"),
          br(), br(),
          DTOutput(ns("cache_table"))
        )
      ),

      nav_panel(
        "Mode PISA",
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("PISA / Complex Survey Detection"),
            p("When a PISA file is loaded, scan common column names for plausible values, weights, and IDs."),
            actionButton(ns("detect_pisa"), "Deteksi Struktur PISA", class = "btn btn-primary w-100")
          ),
          card(
            card_header("Struktur yang Terdeteksi"),
            verbatimTextOutput(ns("pisa_detect"))
          )
        )
      )
    )
  )
}

mod_data_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    catalog <- load_public_catalog()
    cache_version <- reactiveVal(0)
    download_msg <- reactiveVal("")

    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)

      dat <- tryCatch(
        read_user_data(input$file$datapath, ext),
        error = function(e) e
      )

      if (inherits(dat, "error")) {
        showNotification(dat$message, type = "error")
      } else {
        selected_sheet <- attr(dat, "ara_selected_sheet")
        codebook <- attr(dat, "ara_codebook")
        sheets <- attr(dat, "ara_excel_sheets")

        shared$data <- as.data.frame(dat)
        shared$data_name <- input$file$name
        shared$codebook <- codebook
        shared$import_meta <- list(
          selected_sheet = selected_sheet,
          sheets = sheets,
          codebook_sheet = attr(dat, "ara_codebook_sheet")
        )
        shared$auto_map <- infer_dataset_map(
          shared$data,
          filename = input$file$name,
          selected_sheet = selected_sheet
        )
        shared$mapping_token <- shared$mapping_token + 1

        shared$provenance <- new_dataset_provenance(
          name = input$file$name,
          source = "Impor pengguna",
          local_path = input$file$datapath,
          notes = if (!is.null(selected_sheet) && !is.na(selected_sheet)) {
            paste0("Smart Import memilih sheet: ", selected_sheet)
          } else {
            "Diimpor melalui Manajemen Data"
          }
        )

        showNotification(
          paste0(
            "Dataset berhasil dimuat",
            if (!is.null(selected_sheet) && !is.na(selected_sheet)) paste0(" dari sheet '", selected_sheet, "'") else "",
            ": ", format(nrow(shared$data), big.mark = ","), " baris × ", ncol(shared$data), " variabel."
          ),
          type = "message",
          duration = 8
        )
      }
    })

    observe({
      req(shared$data)
      updateSelectInput(session, "var_to_convert", choices = names(shared$data))
    })

    observe({
      choices <- setNames(catalog$id, catalog$title)
      updateSelectInput(session, "public_id", choices = choices)
    })

    selected_public <- reactive({
      req(input$public_id)
      catalog[catalog$id == input$public_id, , drop = FALSE]
    })

    observeEvent(input$convert, {
      req(shared$data, input$var_to_convert)

      dat <- shared$data
      v <- input$var_to_convert

      converted <- tryCatch({
        if (input$new_type == "numeric") {
          suppressWarnings(as.numeric(as.character(dat[[v]])))
        } else if (input$new_type == "factor") {
          safe_as_factor(dat[[v]])
        } else {
          as.character(dat[[v]])
        }
      }, error = function(e) e)

      if (inherits(converted, "error")) {
        showNotification(converted$message, type = "error")
      } else {
        dat[[v]] <- converted
        shared$data <- dat
        showNotification(paste(v, "converted to", input$new_type), type = "message")
      }
    })

    observeEvent(input$apply_clean, {
      req(shared$data)
      dat <- shared$data
      if (isTRUE(input$drop_missing)) {
        dat <- dat[stats::complete.cases(dat), , drop = FALSE]
      }
      shared$data <- dat
      showNotification("Cleaning applied.", type = "message")
    })

    output$import_status <- renderUI({
      if (is.null(shared$data)) {
        p(class = "text-muted", "Belum ada dataset aktif.")
      } else {
        div(class = "dataset-pill",
            span("Aktif: "), strong(shared$data_name), br(),
            span(class = "text-muted", paste("Perkiraan memori:", round(as.numeric(object.size(shared$data))/(1024^2), 1), "MB")))
      }
    })


    output$smart_import_status <- renderUI({
      req(shared$data)
      mp <- shared$auto_map
      meta <- shared$import_meta

      if (is.null(mp)) return(NULL)

      div(
        class = "success-box",
        strong("Pemetaan otomatis berhasil"), br(),
        if (!is.null(meta$selected_sheet) && !is.na(meta$selected_sheet))
          tags$span("Sheet data: ", tags$b(meta$selected_sheet), br()),
        tags$span("Tipe dataset: ", tags$b(mp$profile), br()),
        tags$span(
          "Saran awal: ",
          if (!is.null(mp$hlm$outcome)) paste0("HLM outcome = ", mp$hlm$outcome) else "HLM belum dipetakan",
          if (!is.null(mp$hlm$cluster)) paste0("; cluster = ", mp$hlm$cluster) else ""
        )
      )
    })

    output$overview <- renderUI({
      if (is.null(shared$data)) {
        div(class = "empty-state", p("Impor dataset untuk memulai."))
      } else {
        div(
          class = "metric-row",
          div(class = "metric-box",
              div(class = "metric-value", format(nrow(shared$data), big.mark = ",")),
              div(class = "metric-label", "Baris")),
          div(class = "metric-box",
              div(class = "metric-value", ncol(shared$data)),
              div(class = "metric-label", "Variabel")),
          div(class = "metric-box",
              div(class = "metric-value", format(sum(is.na(shared$data)), big.mark = ",")),
              div(class = "metric-label", "Missing cells"))
        )
      }
    })

    output$preview <- renderDT({
      req(shared$data)
      ara_datatable(
        head(shared$data, 100),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    output$dictionary <- renderDT({
      req(shared$data)
      ara_datatable(
        summarise_variables(shared$data, shared$codebook),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    output$public_catalog <- renderDT({
      show <- catalog[, c("source","title","year","format","access","analysis")]
      ara_datatable(show, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 8))
    })

    output$public_details <- renderUI({
      x <- selected_public()
      tagList(
        h5(x$title),
        p(strong("Source: "), x$source),
        p(strong("Recommended analyses: "), x$analysis),
        p(strong("Format: "), x$format),
        div(class = "warning-box", x$note)
      )
    })

    output$download_action <- renderUI({
      x <- selected_public()
      if (nzchar(x$download_url)) {
        tagList(
          actionButton(ns("download_public"), "Unduh ke Penyimpanan Lokal", class = "btn btn-primary w-100"),
          br(), br()
        )
      } else {
        div(
          class = "dataset-pill",
          "Direct download is intentionally disabled. Use the official source/access page, then load the obtained file locally."
        )
      }
    })

    observeEvent(input$open_source, {
      x <- selected_public()
      tryCatch(
        open_external_url(x$source_url),
        error = function(e) showNotification(e$message, type = "error")
      )
    })

    observeEvent(input$download_public, {
      x <- selected_public()

      if (!nzchar(x$download_url)) {
        download_msg("Direct download is not configured for this source. Use the official source/access page, follow the provider's access terms, then import or load the obtained file locally.")
        return()
      }

      download_msg("Downloading... large PISA files can take time depending on the connection.")

      path <- tryCatch(
        download_public_dataset(x, APP_DATA_DIR),
        error = function(e) e
      )

      if (inherits(path, "error")) {
        download_msg(paste("Download failed:", path$message))
        return()
      }

      extracted <- tryCatch(
        extract_downloaded_zip(path, APP_DATA_DIR),
        error = function(e) e
      )

      if (inherits(extracted, "error")) {
        download_msg(paste("Downloaded, but extraction failed:", extracted$message))
      } else {
        download_msg(paste("Saved and extracted to:", extracted))
        cache_version(cache_version() + 1)
      }
    })

    output$download_status <- renderText(download_msg())

    output$cache_path <- renderText({
      paste("Cache folder:", normalizePath(APP_DATA_DIR, winslash = "/", mustWork = FALSE))
    })

    cached <- reactive({
      cache_version()
      list_cached_datasets(APP_DATA_DIR)
    })

    observe({
      x <- cached()
      if (is.null(x) || nrow(x) == 0) {
        updateSelectInput(session, "cache_file", choices = character(0))
      } else {
        supported <- x$Path[vapply(x$Path, supported_data_file, logical(1))]
        choices <- setNames(supported, basename(supported))
        updateSelectInput(session, "cache_file", choices = choices)
      }
    })

    observeEvent(input$load_cache, {
      req(input$cache_file)

      dat <- tryCatch(
        read_data_path(input$cache_file),
        error = function(e) e
      )

      if (inherits(dat, "error")) {
        showNotification(dat$message, type = "error", duration = NULL)
      } else {
        shared$data <- as.data.frame(dat)
        shared$data_name <- basename(input$cache_file)
        shared$provenance <- new_dataset_provenance(
          name = basename(input$cache_file),
          source = "Local public-data cache",
          local_path = input$cache_file,
          notes = "Loaded directly from the Advanced Research Analytics cache; bypasses browser upload."
        )
        showNotification("Cached dataset berhasil dimuat.", type = "message")
      }
    })

    observeEvent(input$refresh_cache, {
      cache_version(cache_version() + 1)
    })

    output$cache_table <- renderDT({
      x <- cached()
      if (nrow(x) == 0) {
        return(ara_datatable(data.frame(Status = "Cache is empty."), rownames = FALSE))
      }
      ara_datatable(x, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 12))
    })

    observeEvent(input$detect_pisa, {
      req(shared$data)
      det <- detect_pisa_columns(shared$data)

      output$pisa_detect <- renderText({
        paste0(
          "Plausible values:\n",
          if (length(det$plausible_values)) paste(det$plausible_values, collapse = ", ") else "None detected",
          "\n\nWeights:\n",
          if (length(det$weights)) paste(det$weights, collapse = ", ") else "None detected",
          "\n\nSchool ID candidates:\n",
          if (length(det$school_id)) paste(det$school_id, collapse = ", ") else "None detected",
          "\n\nCountry / student ID candidates:\n",
          if (length(det$country_id)) paste(det$country_id, collapse = ", ") else "None detected"
        )
      })
    })
  })
}
