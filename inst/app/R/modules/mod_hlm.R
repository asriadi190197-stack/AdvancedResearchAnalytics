mod_hlm_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "page-heading",
      tags$span(class = "eyebrow", "MULTILEVEL MODELING"),
      h2("Multilevel / HLM"),
      p("Analisis data bertingkat, misalnya guru atau siswa yang berada di dalam sekolah.")
    ),

    usage_guide(
      "Cara cepat menggunakan HLM",
      c(
        "Pilih Outcome: variabel hasil yang ingin dijelaskan.",
        "Pilih Cluster: unit pengelompokan, misalnya kd_sekolah.",
        "Pilih satu atau lebih Prediktor Level-1: variabel yang berbeda antar-individu dalam cluster.",
        "Pilih Prediktor Level-2 bila ada: karakteristik yang mewakili cluster/sekolah.",
        "Klik Jalankan HLM. Pengaturan teknis lanjutan dapat dibiarkan pada nilai default jika Anda belum membutuhkannya."
      ),
      "Untuk pengguna baru, cukup gunakan empat pilihan utama di atas. Pengaturan Lanjutan tidak wajib diubah."
    ),

    quick_start_card(
      ns,
      "HLM: Mulai Cepat",
      "Gunakan data contoh Asesmen Nasional untuk melihat alur HLM dari pemilihan variabel sampai output.",
      "Data contoh hanya untuk latihan dan tidak digunakan untuk membuat peringkat wilayah."
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        class = "hlm-control-card",
        card_header("Spesifikasi Model"),

        uiOutput(ns("data_state")),

        div(
          class = "hlm-field",
          selectInput(ns("outcome"), "1. Outcome", choices = NULL),
          div(class = "hlm-field-help",
              "Variabel hasil yang ingin dijelaskan. Harus berupa data numerik.")
        ),

        div(
          class = "hlm-field",
          selectInput(ns("cluster"), "2. Cluster / ID Level-2", choices = NULL),
          div(class = "hlm-field-help",
              "Unit pengelompokan. Untuk data sekolah biasanya pilih kd_sekolah, bukan kd_individu.")
        ),

        div(
          class = "hlm-field",
          selectizeInput(
            ns("l1"),
            "3. Prediktor Level-1",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Klik atau ketik nama variabel...",
              plugins = list("remove_button")
            )
          ),
          div(class = "hlm-field-help",
              "Variabel pada tingkat individu. Anda dapat memilih lebih dari satu.")
        ),

        div(
          class = "hlm-field",
          selectizeInput(
            ns("l2"),
            "4. Prediktor Level-2",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Opsional — klik atau ketik nama variabel...",
              plugins = list("remove_button")
            )
          ),
          div(class = "hlm-field-help",
              "Karakteristik cluster/sekolah. Boleh dikosongkan jika belum diperlukan.")
        ),

        uiOutput(ns("predictor_check")),
        uiOutput(ns("cluster_quality")),

        tags$details(
          class = "advanced-settings",
          tags$summary("Pengaturan Lanjutan"),
          div(
            class = "advanced-settings-body",

            div(
              class = "advanced-setting-block",
              h5("Centering Prediktor Level-1"),
              p("Centering mengubah titik nol prediktor agar intercept lebih mudah ditafsirkan."),
              radioButtons(
                ns("centering"),
                NULL,
                choices = c(
                  "Tidak ada — gunakan nilai asli" = "none",
                  "Grand mean — kurangi dengan rata-rata seluruh sampel" = "grand",
                  "Group mean — kurangi dengan rata-rata dalam masing-masing cluster" = "group"
                ),
                selected = "none"
              ),
              div(
                class = "setting-recommendation",
                strong("Saran: "),
                "Jika belum memiliki alasan metodologis khusus, gunakan Tidak ada. ",
                "Group mean biasanya dipakai ketika ingin memisahkan efek within-cluster dari between-cluster."
              )
            ),

            div(
              class = "advanced-setting-block",
              checkboxInput(
                ns("use_interaction"),
                "Aktifkan cross-level interaction",
                FALSE
              ),
              p(
                class = "advanced-help",
                "Gunakan jika Anda ingin menguji apakah pengaruh prediktor Level-1 berubah menurut karakteristik Level-2. ",
                "Contoh: apakah hubungan dukungan guru dengan outcome berbeda menurut status sekolah?"
              ),
              conditionalPanel(
                condition = sprintf("input['%s']", ns("use_interaction")),
                selectInput(ns("interaction_l1"), "Variabel Level-1 untuk interaksi", choices = NULL),
                selectInput(ns("interaction_l2"), "Variabel Level-2 untuk interaksi", choices = NULL)
              )
            ),

            div(
              class = "advanced-setting-block",
              checkboxInput(ns("use_random_slope"), "Aktifkan random slope", FALSE),
              p(
                class = "advanced-help",
                "Random slope digunakan ketika pengaruh suatu prediktor Level-1 diperkirakan berbeda antar-cluster/sekolah. ",
                "Tidak perlu diaktifkan pada analisis dasar."
              ),
              conditionalPanel(
                condition = sprintf("input['%s']", ns("use_random_slope")),
                selectInput(ns("random_slope"), "Prediktor dengan random slope", choices = NULL)
              )
            ),

            div(
              class = "advanced-setting-block",
              checkboxInput(ns("reml"), "Gunakan REML", TRUE),
              p(
                class = "advanced-help",
                "REML direkomendasikan untuk estimasi variance components pada model akhir. ",
                "Jika REML aktif, aplikasi tidak menghitung uji LR untuk perbandingan fixed effects. Pilih ML untuk perbandingan nested dengan struktur random effects yang sama."
              )
            )
          )
        ),

        br(),
        actionButton(ns("run"), "Jalankan HLM", class = "btn btn-primary w-100"),
        br(), br(),
        downloadButton(ns("download_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
      ),

      div(
        uiOutput(ns("metrics")),
        uiOutput(ns("model_warning")),
        navset_card_tab(
          nav_panel(
            "Tabel Model Berjenjang",
            uiOutput(ns("incremental_status")),
            actionButton(
              ns("run_incremental"),
              "Hitung Model 2 & 3",
              class = "btn btn-outline-primary btn-sm",
              disabled = "disabled"
            ),
            br(), br(),
            DTOutput(ns("publication_models"))
          ),
          nav_panel("Koefisien Model Penuh", DTOutput(ns("publication_coefficients"))),
          nav_panel("Tabel Koefisien APA", DTOutput(ns("apa"))),
          nav_panel("Perbandingan LR (jika valid)", DTOutput(ns("comparison"))),
          nav_panel("BLUP Top 20", DTOutput(ns("publication_blup"))),
          nav_panel("Uji Sensitivitas",
            p("Pemeriksaan ketahanan hasil dengan pembatasan cluster; dijalankan setelah model utama selesai."),
            numericInput(ns("sensitivity_min_n"), "Minimum observasi per cluster", value = 5, min = 2, max = 100),
            checkboxInput(ns("sensitivity_within"), "Pertahankan hanya cluster dengan variasi prediktor numerik Level-1", TRUE),
            checkboxInput(ns("sensitivity_slope"), "Bandingkan dengan random intercept saja (jika ada random slope)", TRUE),
            actionButton(ns("run_sensitivity"), "Hitung Uji Sensitivitas", class = "btn btn-outline-primary"),
            div(class = "hlm-sensitivity-results",
              div(class = "hlm-sensitivity-note", verbatimTextOutput(ns("sensitivity_note"))),
              div(class = "hlm-sensitivity-panel",
                h5("Perbandingan koefisien model"),
                p(class = "text-muted small", "Tabel koefisien dapat digeser secara horizontal tanpa mengganggu tabel lain."),
                div(class = "hlm-sensitivity-table-scroll", DTOutput(ns("sensitivity_table")))
              ),
              div(class = "hlm-sensitivity-panel hlm-sensitivity-status-panel",
                h5("Status dan catatan skenario"),
                tableOutput(ns("sensitivity_status"))
              )
            )
          ),
          nav_panel("Uji Lanjutan HLM",
            p("Pilih prosedur sesuai desain penelitian. Setiap uji dijalankan hanya atas permintaan dan kegagalannya dilaporkan secara terpisah."),
            checkboxGroupInput(ns("advanced_tests"), "Pilih uji",
              choices = c("Diagnostik residual dan cluster" = "diagnostics",
                          "SE robust cluster CR2 (clubSandwich)" = "robust",
                          "Dekomposisi within-between (Mundlak)" = "within_between",
                          "CI bootstrap parametrik koefisien" = "bootstrap"),
              selected = c("diagnostics")),
            p(class = "text-muted small",
              "Pada data besar, jalankan CR2, Mundlak, dan bootstrap hanya jika diperlukan. ",
              "CR2 bisa memerlukan memori besar; Mundlak mengestimasi model baru; ",
              "bootstrap mengestimasi ulang model pada setiap replikasi."),
            numericInput(ns("advanced_boot_n"), "Replikasi bootstrap (20–2000)", value = 100, min = 20, max = 2000),
            actionButton(ns("run_advanced"), "Jalankan Uji Lanjutan", class = "btn btn-outline-primary"),
            h5("Status masing-masing uji"), tableOutput(ns("advanced_status")),
            h5("Hasil uji"), uiOutput(ns("advanced_tables"))
          ),
          nav_panel("Ringkasan", DTOutput(ns("summary"))),
          nav_panel("Kecocokan Model", DTOutput(ns("fit"))),
          nav_panel("Efek Acak", DTOutput(ns("random"))),
          nav_panel("BLUP Plot", plotOutput(ns("blup"), height = "500px")),
          nav_panel("Diagnostik", plotOutput(ns("diagnostics"), height = "420px")),
          nav_panel("Sintaks R", verbatimTextOutput(ns("syntax")))
        )
      )
    )
  )
}

mod_hlm_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    quick_start_server(
      input, output, session, shared,
      example_path = "data/examples/demo_hlm_an_paud.csv",
      example_name = "Contoh HLM — AN 2025 Guru PAUD",
      source = "Kemendikdasmen — Asesmen Nasional 2025",
      notes = "Subset latihan; bukan untuk ranking wilayah.",
      template_path = "data/templates/hlm_template.csv"
    )

    result <- reactiveVal(NULL)

    # The data-to-input mapping is handled in one place only.
    # This prevents the general choice updater from erasing selected values.
    observeEvent(shared$mapping_token, {
      req(shared$data)

      vars <- names(shared$data)
      nums <- numeric_vars(shared$data)
      mp <- shared$auto_map$hlm %||% list()

      outcome_sel <- mp$outcome %||% if (length(nums)) nums[1] else NULL

      # Never use an individual ID as the default cluster when a school ID exists.
      cluster_sel <- mp$cluster
      if ("kd_sekolah" %in% vars) cluster_sel <- "kd_sekolah"
      if (is.null(cluster_sel) && length(vars)) cluster_sel <- vars[1]

      l1_sel <- intersect(mp$l1 %||% character(0), vars)
      l2_sel <- intersect(mp$l2 %||% character(0), vars)

      updateSelectInput(
        session, "outcome",
        choices = nums,
        selected = outcome_sel
      )

      updateSelectInput(
        session, "cluster",
        choices = vars,
        selected = cluster_sel
      )

      updateSelectizeInput(
        session, "l1",
        choices = vars,
        selected = l1_sel,
        server = TRUE
      )

      updateSelectizeInput(
        session, "l2",
        choices = vars,
        selected = l2_sel,
        server = TRUE
      )

      updateSelectInput(
        session, "random_slope",
        choices = intersect(l1_sel, nums),
        selected = if (length(intersect(l1_sel, nums))) intersect(l1_sel, nums)[1] else NULL
      )

      updateSelectInput(
        session, "interaction_l1",
        choices = intersect(l1_sel, nums)
      )

      updateSelectInput(
        session, "interaction_l2",
        choices = l2_sel
      )

      showNotification(
        paste0(
          "Pemetaan HLM siap. Outcome: ", outcome_sel %||% "-",
          "; Cluster: ", cluster_sel %||% "-",
          ". Silakan periksa prediktor sebelum menjalankan model."
        ),
        type = "message",
        duration = 7
      )
    }, ignoreInit = FALSE)

    # Keep advanced choices synchronized with the user's current selections,
    # without touching/erasing the Level-1 or Level-2 selections themselves.
    observeEvent(input$l1, {
      req(shared$data)
      nums <- numeric_vars(shared$data)
      ch <- intersect(input$l1 %||% character(0), nums)

      updateSelectInput(
        session, "random_slope",
        choices = ch,
        selected = if (!is.null(input$random_slope) && input$random_slope %in% ch)
          input$random_slope else if (length(ch)) ch[1] else NULL
      )

      updateSelectInput(
        session, "interaction_l1",
        choices = ch,
        selected = if (!is.null(input$interaction_l1) && input$interaction_l1 %in% ch)
          input$interaction_l1 else if (length(ch)) ch[1] else NULL
      )
    }, ignoreInit = TRUE)

    observeEvent(input$l2, {
      ch <- input$l2 %||% character(0)
      updateSelectInput(
        session, "interaction_l2",
        choices = ch,
        selected = if (!is.null(input$interaction_l2) && input$interaction_l2 %in% ch)
          input$interaction_l2 else if (length(ch)) ch[1] else NULL
      )
    }, ignoreInit = TRUE)

    observeEvent(input$load_example, {
      req(shared$data)
      vars <- names(shared$data)

      if ("GD.2.5" %in% vars)
        updateSelectInput(session, "outcome", choices = numeric_vars(shared$data), selected = "GD.2.5")

      if ("kd_sekolah" %in% vars)
        updateSelectInput(session, "cluster", choices = vars, selected = "kd_sekolah")

      updateSelectizeInput(
        session, "l1",
        choices = vars,
        selected = intersect(c("GD.2.3","GD.2.4","GE.3.3"), vars),
        server = TRUE
      )

      updateSelectizeInput(
        session, "l2",
        choices = vars,
        selected = intersect(c("GE.3.4","sts_sek","instansi_kewenangan"), vars),
        server = TRUE
      )
    })


    output$predictor_check <- renderUI({
      req(shared$data)
      vars <- unique(c(input$l1 %||% character(0), input$l2 %||% character(0)))
      if (!length(vars)) return(NULL)

      q <- predictor_quality_table(shared$data, vars)
      bad <- q[q$Status != "Dapat digunakan", , drop = FALSE]

      if (!nrow(bad)) {
        return(div(
          class="predictor-ok-box",
          shiny::icon("check-circle"),
          span(" Semua prediktor yang dipilih dapat digunakan.")
        ))
      }

      div(
        class="predictor-warning-box",
        div(class="predictor-warning-title",
            shiny::icon("exclamation-triangle"),
            " Variabel berikut tidak dapat diestimasi:"),
        tags$ul(lapply(seq_len(nrow(bad)), function(i) {
          tags$li(tags$b(bad$Variabel[i]),
                  paste0(" — hanya ", bad$Level_Nilai_Unik[i], " level/nilai unik."))
        })),
        p("Aplikasi akan melewati variabel tersebut secara otomatis.")
      )
    })

    output$data_state <- renderUI({
      if (is.null(shared$data)) {
        div(class = "warning-box", "Muat dataset terlebih dahulu melalui menu Data.")
      } else {
        div(
          class = "success-box",
          strong(shared$data_name), br(),
          paste(format(nrow(shared$data), big.mark = ","), "baris ×", ncol(shared$data), "variabel")
        )
      }
    })


    output$cluster_quality <- renderUI({
      req(shared$data, input$cluster)

      d <- tryCatch(
        hlm_cluster_diagnostics(shared$data, input$cluster),
        error = function(e) NULL
      )
      if (is.null(d)) return(NULL)

      cls <- if (isTRUE(d$suitable)) "sem-example-note" else "warning-box"

      div(
        class = cls,
        strong(
          paste0(
            "Cluster: ", format(d$clusters, big.mark=","),
            " | Rata-rata n/cluster: ", sprintf("%.2f", d$mean_n),
            " | Median: ", sprintf("%.0f", d$median_n),
            " | Singleton: ", sprintf("%.1f%%", d$singleton_pct)
          )
        ),
        br(),
        if (isTRUE(d$suitable)) {
          "Struktur cluster memadai untuk dijalankan."
        } else {
          paste0(
            "HLM tidak akan dijalankan dengan cluster ini: ", d$reason,
            " Pilih unit cluster yang memiliki observasi berulang."
          )
        }
      )
    })

    observeEvent(input$run, {
      if (!try_acquire_analysis(shared, "Multilevel / HLM")) return()

      req(shared$data, input$outcome, input$cluster)

      cluster_check <- hlm_cluster_diagnostics(shared$data, input$cluster)
      if (!isTRUE(cluster_check$suitable)) {
        release_analysis(shared)
        showNotification(
          paste0(
            "HLM dibatalkan sebelum fitting: ", cluster_check$reason,
            " Cluster=", format(cluster_check$clusters, big.mark=","),
            "; rata-rata n/cluster=", sprintf("%.2f", cluster_check$mean_n),
            "; singleton=", sprintf("%.1f%%", cluster_check$singleton_pct), "."
          ),
          type = "warning",
          duration = NULL
        )
        return()
      }

      if (identical(input$outcome, input$cluster)) {
        release_analysis(shared)
        showNotification("Outcome dan Cluster tidak boleh menggunakan variabel yang sama.", type = "error")
        return()
      }

      if (identical(input$cluster, "kd_individu")) {
        showNotification(
          "kd_individu adalah ID individu dan biasanya tidak tepat sebagai cluster HLM. Pilih kd_sekolah jika analisis Anda mengelompokkan individu di dalam sekolah.",
          type = "warning",
          duration = 10
        )
      }

      if (length(input$l1) == 0 && length(input$l2) == 0) {
        showNotification(
          "Tidak ada prediktor yang dipilih. Aplikasi akan menjalankan null model / random-intercept model.",
          type = "message",
          duration = 6
        )
      }

      if (isTRUE(input$use_interaction) &&
          (is.null(input$interaction_l1) || !nzchar(input$interaction_l1) ||
           is.null(input$interaction_l2) || !nzchar(input$interaction_l2))) {
        release_analysis(shared)
        showNotification(
          "Untuk cross-level interaction, pilih satu variabel Level-1 dan satu variabel Level-2.",
          type = "error"
        )
        return()
      }

      dat_bg <- shared$data
      outcome_bg <- input$outcome
      cluster_bg <- input$cluster
      l1_bg <- input$l1 %||% character(0)
      l2_bg <- input$l2 %||% character(0)
      centering_bg <- input$centering
      use_slope_bg <- isTRUE(input$use_random_slope)
      slope_bg <- input$random_slope
      use_interaction_bg <- isTRUE(input$use_interaction)
      int_l1_bg <- input$interaction_l1
      int_l2_bg <- input$interaction_l2
      reml_bg <- isTRUE(input$reml)

      shinyjs::disable("run")
      background_start_notification("Analisis HLM")

      p <- promises::future_promise({
        dat <- dat_bg

        if (length(l1_bg) > 0 && centering_bg != "none") {
          for (v in l1_bg) {
            if (is.numeric(dat[[v]])) {
              dat[[v]] <- if (centering_bg == "grand") {
                center_variable(dat[[v]], "grand")
              } else {
                center_variable(dat[[v]], "group", dat[[cluster_bg]])
              }
            }
          }
        }

        slope <- if (use_slope_bg && !is.null(slope_bg) && nzchar(slope_bg)) slope_bg else NULL
        int_l1 <- if (use_interaction_bg) int_l1_bg else NULL
        int_l2 <- if (use_interaction_bg) int_l2_bg else NULL

        fit_hlm_fast_core(
          data = dat,
          outcome = outcome_bg,
          cluster = cluster_bg,
          l1 = l1_bg,
          l2 = l2_bg,
          random_slope = slope,
          reml = reml_bg,
          interaction_l1 = int_l1,
          interaction_l2 = int_l2
        )
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          result(fit)
          shinyjs::enable("run")
          shinyjs::enable("run_incremental")
          release_analysis(shared)

          if (!is.null(fit$skipped_predictors) && nrow(fit$skipped_predictors) > 0) {
            showNotification(
              paste0(
                "HLM selesai. Variabel yang dilewati: ",
                paste(unique(fit$skipped_predictors$Variabel), collapse = ", ")
              ),
              type = "warning",
              duration = 12
            )
          } else {
            background_success_notification("Analisis HLM")
          }
          NULL
        },
        onRejected = function(err) {
          result(NULL)
          shinyjs::enable("run")
          shinyjs::disable("run_incremental")
          release_analysis(shared)
          background_error_notification("Analisis HLM", err)
          NULL
        }
      )
    })


    observeEvent(input$run_incremental, {
      x <- result()
      req(x)

      if (!isTRUE(x$fast_mode)) {
        showNotification("Model berjenjang lengkap sudah dihitung.", type = "message")
        return()
      }

      if (!try_acquire_analysis(shared, "Model Berjenjang HLM")) return()

      x_bg <- x
      shinyjs::disable("run_incremental")
      background_start_notification("Model Berjenjang HLM")

      p <- promises::future_promise({
        complete_hlm_incremental_models(x_bg)
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(x2) {
          result(x2)
          shinyjs::disable("run_incremental")
          release_analysis(shared)
          background_success_notification("Model Berjenjang HLM")
          NULL
        },
        onRejected = function(err) {
          shinyjs::enable("run_incremental")
          release_analysis(shared)
          background_error_notification("Model Berjenjang HLM", err)
          NULL
        }
      )
    })

    observeEvent(input$run_sensitivity, {
      x <- result(); req(x)
      if (!try_acquire_analysis(shared, "Sensitivitas HLM")) return()
      x_bg <- x
      min_n <- input$sensitivity_min_n
      within <- isTRUE(input$sensitivity_within)
      slope <- isTRUE(input$sensitivity_slope)
      shinyjs::disable("run_sensitivity")
      background_start_notification("Sensitivitas HLM")
      p <- promises::future_promise({
        hlm_sensitivity_analysis(x_bg, min_cluster_n = min_n,
                                 require_within_variance = within,
                                 drop_random_slope = slope)
      }, seed = TRUE)
      promises::then(p,
        onFulfilled = function(sens) {
          current <- result()
          if (!is.null(current) && identical(current$full_formula, x_bg$full_formula) &&
              identical(current$n, x_bg$n)) {
            current$sensitivity <- sens
            result(current)
          }
          shinyjs::enable("run_sensitivity")
          release_analysis(shared)
          background_success_notification("Sensitivitas HLM")
          NULL
        },
        onRejected = function(err) {
          shinyjs::enable("run_sensitivity")
          release_analysis(shared)
          background_error_notification("Sensitivitas HLM", err)
          NULL
        })
    })

    output$sensitivity_table <- renderDT({
      x <- result(); req(x, x$sensitivity)
      tab <- x$sensitivity$coefficients
      req(is.data.frame(tab), nrow(tab) > 0L)
      # Keep numeric values numeric in Excel; format only the on-screen view.
      numeric_columns <- names(tab)[vapply(tab, is.numeric, logical(1))]
      widget <- ara_datatable(
        tab, rownames = FALSE, filter = "none", class = "stripe hover compact nowrap",
        options = list(pageLength = 12, lengthMenu = c(12, 25, 50),
                       autoWidth = FALSE, scrollX = TRUE, scrollCollapse = TRUE,
                       dom = "tip", deferRender = TRUE)
      )
      if (length(numeric_columns)) {
        widget <- DT::formatRound(widget, columns = numeric_columns, digits = 4)
      }
      widget
    })
    output$sensitivity_status <- renderTable({
      x <- result(); req(x, x$sensitivity)
      tab <- x$sensitivity$status
      req(is.data.frame(tab), nrow(tab) > 0L)
      tab
    }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", rownames = FALSE,
    sanitize.text.function = htmltools::htmlEscape)
    output$sensitivity_note <- renderText({
      x <- result(); if (is.null(x) || is.null(x$sensitivity))
        return("Uji sensitivitas belum dijalankan.")
      x$sensitivity$note
    })

    observeEvent(input$run_advanced, {
      x <- result(); req(x)
      if (!try_acquire_analysis(shared, "Uji Lanjutan HLM")) return()
      selected <- input$advanced_tests %||% character(0)
      if (!length(selected)) {
        release_analysis(shared)
        showNotification("Pilih minimal satu uji.", type = "warning")
        return()
      }
      x_bg <- x
      b <- input$advanced_boot_n
      shinyjs::disable("run_advanced")
      background_start_notification("Uji Lanjutan HLM")
      p <- promises::future_promise({
        ara_hlm_advanced(x_bg, selected, bootstrap_n = b)
      }, seed = TRUE)
      promises::then(p, onFulfilled = function(z) {
        cur <- result()
        if (!is.null(cur) && identical(cur$full_formula, x_bg$full_formula) &&
            identical(cur$n, x_bg$n)) {
          cur$advanced <- z
          result(cur)
        }
        shinyjs::enable("run_advanced")
        release_analysis(shared)
        background_success_notification("Uji Lanjutan HLM")
        NULL
      }, onRejected = function(e) {
        shinyjs::enable("run_advanced")
        release_analysis(shared)
        background_error_notification("Uji Lanjutan HLM", e)
        NULL
      })
    })
    output$advanced_status <- renderTable({
      x <- result(); req(x, x$advanced)
      x$advanced$Status
    }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$advanced_tables <- renderUI({
      x <- result(); req(x, x$advanced)
      items <- ara_hlm_advanced_excel(x$advanced)
      items <- items[names(items) != "Lanjutan_Status"]
      ara_collapsible_result_tables(items, max_rows = 50L,
        empty_message = "Belum ada tabel hasil yang berhasil dihitung.")
    })

    output$metrics <- renderUI({
      x <- result()
      if (is.null(x)) {
        return(card(
          div(
            class = "analysis-placeholder",
            h4("Belum ada model yang dijalankan"),
            p("Pilih variabel di sebelah kiri lalu klik Jalankan HLM.")
          )
        ))
      }

      z <- tail(x$fit, 1)
      div(
        class = "metric-row hlm-metrics",
        div(class = "metric-box",
            div(class = "metric-value", format(x$n, big.mark = ",")),
            div(class = "metric-label", "Observasi")),
        div(class = "metric-box",
            div(class = "metric-value", x$clusters),
            div(class = "metric-label", "Cluster")),
        div(class = "metric-box",
            div(class = "metric-value", sprintf("%.2f", x$icc)),
            div(class = "metric-label", "ICC")),
        div(class = "metric-box",
            div(class = "metric-value", sprintf("%.2f", z$Marginal_R2)),
            div(class = "metric-label", "Marginal R²")),
        div(class = "metric-box",
            div(class = "metric-value", sprintf("%.2f", z$Conditional_R2)),
            div(class = "metric-label", "Conditional R²"))
      )
    })

    output$model_warning <- renderUI({
      x <- result()
      req(x)
      if (isTRUE(x$singular)) {
        div(
          class = "warning-box",
          strong("Model mengalami singular fit. "),
          "Struktur random effect mungkin terlalu kompleks. Coba nonaktifkan random slope atau sederhanakan model."
        )
      }
    })



    output$incremental_status <- renderUI({
      x <- result()
      req(x)

      if (isTRUE(x$fast_mode)) {
        div(
          class = "fast-analysis-note",
          strong("Mode Cepat aktif. "),
          "Model 1 (Null) dan Model 4 (Model Penuh) sudah dihitung. ",
          "Model 2 (Level-1) dan Model 3 (Level-2) belum dihitung agar hasil utama muncul lebih cepat. ",
          "Klik tombol “Hitung Model 2 & 3” untuk melengkapi tabel tanpa mengulang Model 1 dan Model 4."
        )
      } else {
        div(
          class = "sem-example-note",
          strong("Tabel lengkap. "),
          "Model 1, Model 2, Model 3, dan Model 4 sudah dihitung."
        )
      }
    })

    output$publication_models <- renderDT({
      x <- result(); req(x)
      ara_datatable(
        hlm_model_comparison_publication(x),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 25, dom = "tip")
      )
    })

    output$publication_coefficients <- renderDT({
      x <- result(); req(x)
      ara_datatable(
        hlm_full_coefficients_publication(x),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 15, dom = "tip")
      )
    })

    output$publication_blup <- renderDT({
      x <- result(); req(x)
      ara_datatable(
        hlm_blup_top20_publication(x, 20),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 20, dom = "tip")
      )
    })

    output$summary <- renderDT({
      x <- result(); req(x)
      ara_output_table(hlm_excel_tables(x)[["Ringkasan_Model"]])
    })

    output$apa <- renderDT({
      x <- result(); req(x)
      ara_datatable(hlm_apa_table(x), rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    output$fit <- renderDT({
      x <- result(); req(x)
      ara_datatable(x$fit, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    output$comparison <- renderDT({
      x <- result(); req(x)
      ara_datatable(
        if (is.null(x$comparison))
          data.frame(Catatan = x$comparison_note %||% "Perbandingan model tidak tersedia.")
        else as.data.frame(x$comparison),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$random <- renderDT({
      x <- result(); req(x)
      ara_datatable(x$random, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    output$blup <- renderPlot({
      x <- result(); req(x)
      b <- x$blup[order(x$blup$Random_Intercept), ]
      if (nrow(b) > 60)
        b <- b[round(seq(1, nrow(b), length.out = 60)), ]

      par(mar = c(5, 8, 4, 2))
      plot(
        b$Random_Intercept, seq_len(nrow(b)),
        yaxt = "n", ylab = "",
        xlab = "Random intercept (BLUP)",
        pch = 19,
        main = "Efek Acak Cluster"
      )
      axis(2, at = seq_len(nrow(b)), labels = b$Cluster,
           las = 2, cex.axis = .55)
      abline(v = 0, lty = 2)
    })

    output$diagnostics <- renderPlot({
      x <- result(); req(x)
      plot(
        fitted(x$full_model), residuals(x$full_model),
        xlab = "Nilai prediksi",
        ylab = "Residual",
        main = "Residual vs Nilai Prediksi"
      )
      abline(h = 0, lty = 2)
    })

    output$syntax <- renderText({
      x <- result(); req(x)

      txt <- vapply(names(x$formulas), function(nm) {
        paste0(
          "# ", nm, "\n",
          "lmerTest::lmer(",
          paste(deparse(x$formulas[[nm]]), collapse = ""),
          ", data = data, REML = ",
          ifelse(isTRUE(input$reml), "TRUE", "FALSE"),
          ")"
        )
      }, character(1))

      paste(txt, collapse = "\n\n")
    })

    output$download_excel <- downloadHandler(
      filename = function() paste0("HLM_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- result(); req(x)
        export_analysis_excel(
          prepend_analysis_info(
            hlm_excel_tables(x),
            "Multilevel / HLM",
            provenance = shared$provenance,
            extra = list(
              Outcome = input$outcome,
              Cluster = input$cluster,
              Prediktor_Level_1 = paste(input$l1 %||% character(0), collapse = ", "),
              Prediktor_Level_2 = paste(input$l2 %||% character(0), collapse = ", ")
            )
          ),
          file
        )
      }
    )
  })
}
