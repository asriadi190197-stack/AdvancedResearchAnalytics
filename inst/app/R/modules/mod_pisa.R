mod_pisa_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "page-heading",
      tags$span(class = "eyebrow", "PISA LARGE-DATA TOOLS"),

    usage_guide(
      "Cara menggunakan Alat PISA",
      c(
        "Muat terlebih dahulu file PISA atau extract PISA pada Manajemen Data.",
        "Gunakan Pembuat Subset untuk menyaring negara dan variabel yang diperlukan.",
        "Jalankan Deteksi Desain untuk mencari plausible values, weights, replicate weights, dan school ID.",
        "Gunakan PV Pooling hanya setelah memperoleh estimasi dan sampling variance untuk setiap plausible value.",
        "Simpan extract sebagai RDS agar analisis berikutnya lebih cepat."
      ),
      "Untuk inferensi populasi PISA, plausible values dan survey weights harus diperlakukan dengan benar."
    ),

      h2("PISA 2025 Builder"),
      p("Build reduced analysis extracts, detect complex-survey fields, and pool estimates across plausible values.")
    ),

    navset_card_tab(
      nav_panel(
        "Pembuat Subset",
        layout_columns(
          col_widths = c(4,8),
          card(
            card_header("Buat Subset Negara / Variabel"),
            uiOutput(ns("data_state")),
            selectInput(ns("country_var"), "Variabel negara", choices = NULL),
            textInput(ns("country_value"), "Nilai/kode negara", value = ""),
            selectizeInput(ns("vars"), "Variabel yang dipertahankan", choices = NULL, multiple = TRUE),
            textInput(ns("subset_name"), "Nama file extract", value = "pisa2025_subset"),
            actionButton(ns("build_subset"), "Buat Subset", class = "btn btn-primary w-100"),
            br(), br(),
            actionButton(ns("save_subset"), "Simpan Extract RDS", class = "btn btn-outline-secondary w-100")
          ),
          card(
            card_header("Hasil Subset"),
            uiOutput(ns("subset_metrics")),
            DTOutput(ns("subset_preview")),
            verbatimTextOutput(ns("subset_status"))
          )
        )
      ),

      nav_panel(
        "Deteksi Desain",
        layout_columns(
          col_widths = c(5,7),
          card(
            card_header("Deteksi Komponen Desain PISA"),
            actionButton(ns("detect"), "Pindai Dataset Aktif", class = "btn btn-primary w-100")
          ),
          card(
            card_header("Komponen yang Terdeteksi"),
            verbatimTextOutput(ns("detected"))
          )
        )
      ),

      nav_panel(
        "Pooling Plausible Values",
        layout_columns(
          col_widths = c(4,8),
          card(
            card_header("Pooling PV Manual"),
            p(class = "text-muted",
              "Enter one estimate and one sampling variance per plausible value. The app combines them using multiple-imputation style pooling."),
            textAreaInput(ns("pv_est"), "Estimasi (pisahkan dengan koma)", rows = 4),
            textAreaInput(ns("pv_var"), "Sampling variance (pisahkan dengan koma)", rows = 4),
            actionButton(ns("pool"), "Gabungkan Estimasi", class = "btn btn-primary w-100")
          ),
          card(
            card_header("Hasil Gabungan"),
            DTOutput(ns("pool_table")),
            div(class = "warning-box",
                "This pooling tool is a general framework. Population inference from PISA also requires correct final weights and replicate-weight variance estimation.")
          )
        )
      ),

      nav_panel(
        "Kerangka Survey Weight",
        card(
          card_header("Cakupan Saat Ini"),
          tags$ul(
            tags$li("Detect final weights and replicate weights."),
            tags$li("Compute a weighted mean with a BRR/Fay-style replicate variance framework."),
            tags$li("Keep plausible-value pooling separate from sampling variance."),
            tags$li("Do not treat unweighted estimates as population estimates.")
          ),
          div(class = "warning-box",
              "V0.7 provides a transparent framework, not an OECD-certified reproduction engine. Exact replicate-weight constants and analysis-specific formulas must match PISA documentation.")
        )
      )
    )
  )
}

mod_pisa_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    subset_data <- reactiveVal(NULL)
    subset_status <- reactiveVal("")
    detected <- reactiveVal(NULL)

    observe({
      req(shared$data)
      all_vars <- names(shared$data)
      det <- detect_pisa_design(shared$data)
      country_choices <- unique(c(det$country, all_vars))

      updateSelectInput(session, "country_var", choices = country_choices)
      updateSelectizeInput(session, "vars", choices = all_vars, server = TRUE)
    })

    output$data_state <- renderUI({
      if (is.null(shared$data)) {
        div(class = "warning-box", "Load a PISA dataset or extract first.")
      } else {
        div(class = "success-box",
            strong(shared$data_name), br(),
            paste(format(nrow(shared$data), big.mark=","), "rows ×", ncol(shared$data), "variables"))
      }
    })

    observeEvent(input$build_subset, {
      req(shared$data, input$country_var, input$country_value)

      x <- tryCatch(
        build_country_subset(
          shared$data,
          country_var = input$country_var,
          country_value = input$country_value,
          variables = input$vars
        ),
        error = function(e) e
      )

      if (inherits(x, "error")) {
        showNotification(x$message, type = "error", duration = NULL)
      } else {
        subset_data(x)
        subset_status(paste("Subset built:", nrow(x), "rows ×", ncol(x), "variables"))
      }
    })

    observeEvent(input$save_subset, {
      req(subset_data())
      p <- tryCatch(
        save_analysis_extract(subset_data(), input$subset_name),
        error = function(e) e
      )
      if (inherits(p, "error")) {
        subset_status(paste("Save failed:", p$message))
      } else {
        subset_status(paste("Saved:", p))
      }
    })

    output$subset_metrics <- renderUI({
      x <- subset_data()
      if (is.null(x)) return(div(class="empty-state","No subset built yet."))
      div(
        class="metric-row",
        div(class="metric-box",div(class="metric-value",format(nrow(x),big.mark=",")),div(class="metric-label","Baris")),
        div(class="metric-box",div(class="metric-value",ncol(x)),div(class="metric-label","Variabel")),
        div(class="metric-box",div(class="metric-value",round(as.numeric(object.size(x))/(1024^2),1)),div(class="metric-label","Approx. MB in memory"))
      )
    })

    output$subset_preview <- renderDT({
      req(subset_data())
      ara_datatable(head(subset_data(),100),rownames=FALSE,options=list(scrollX=TRUE,pageLength=10))
    })

    output$subset_status <- renderText(subset_status())

    observeEvent(input$detect, {
      req(shared$data)
      detected(detect_pisa_design(shared$data))
    })

    output$detected <- renderText({
      d <- detected()
      req(d)
      paste0(
        "Plausible values:\n", if(length(d$plausible_values)) paste(d$plausible_values,collapse=", ") else "None", "\n\n",
        "Final weights:\n", if(length(d$final_weights)) paste(d$final_weights,collapse=", ") else "None", "\n\n",
        "Replicate weights:\n", if(length(d$replicate_weights)) paste(d$replicate_weights,collapse=", ") else "None", "\n\n",
        "School IDs:\n", if(length(d$school_id)) paste(d$school_id,collapse=", ") else "None", "\n\n",
        "Variabel negaras:\n", if(length(d$country)) paste(d$country,collapse=", ") else "None"
      )
    })

    output$pool_table <- renderDT({
      input$pool
      isolate({
        est <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input$pv_est, ",")))))
        var <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input$pv_var, ",")))))
        est <- est[is.finite(est)]
        var <- var[is.finite(var)]

        if (length(est) < 2 || length(est) != length(var)) {
          return(ara_datatable(data.frame(Note="Provide the same number of valid estimates and variances; at least two each."),rownames=FALSE))
        }

        x <- pool_plausible_value_estimates(est,var)
        ara_datatable(x,rownames=FALSE,options=list(dom="t"))
      })
    })
  })
}
