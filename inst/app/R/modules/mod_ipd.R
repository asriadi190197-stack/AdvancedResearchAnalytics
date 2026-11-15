mod_ipd_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "page-heading",
      tags$span(class = "eyebrow", "IPD META-ANALYSIS"),

    usage_guide(
      "Cara menggunakan IPD Meta-Analisis",
      c(
        "Pilih Two-Stage atau One-Stage sesuai strategi analisis.",
        "Tentukan Outcome, Predictor utama, Cluster/Study ID, dan covariates.",
        "Untuk Two-Stage, periksa jumlah cluster valid sebelum membaca pooled effect.",
        "Periksa Q, I², τ², forest plot; jalankan leave-one-out dan meta-regression saat diperlukan.",
        "Untuk One-Stage, periksa fixed effects, random effects, R², serta residual diagnostics."
      ),
      "Cluster harus memiliki ukuran sampel memadai agar estimasi Stage-1 stabil."
    ),

      h2("IPD Meta-Analisis"),
      p("Run two-stage cluster-specific pooling or one-stage mixed-effects IPD models.")
    ),

    navset_card_tab(
      nav_panel(
        "Two-Stage IPD",
        quick_start_card(ns, "IPD Meta-Analisis: Mulai Cepat", "Contoh nyata untuk Two-Stage IPD dengan cluster kabupaten/kota, outcome, predictor, dan covariate.", "Sumber contoh: subset ringan dari Rapor Publik AN 2025 Guru PAUD."),

    layout_columns(
          col_widths = c(4, 8),

          card(
            card_header("Pengaturan Two-Stage"),
            uiOutput(ns("data_state")),
            selectInput(ns("outcome"), "Outcome (Y)", choices = NULL),
            selectInput(ns("predictor"), "Prediktor Utama (X)", choices = NULL),
            selectInput(ns("cluster"), "Cluster / Study ID", choices = NULL),
            selectizeInput(ns("covariates"), "Kovariat", choices = NULL, multiple = TRUE),
            selectInput(ns("moderator"), "Moderator Numerik Opsional", choices = c("None" = "")),
            numericInput(ns("min_n"), "Minimum kasus per cluster", value = 10, min = 5, step = 1),
            selectInput(
              ns("method"),
              "Estimator meta-analysis",
              choices = c("REML" = "REML", "ML" = "ML", "DL" = "DL", "HE" = "HE"),
              selected = "REML"
            ),
            uiOutput(ns("ipd_cluster_quality")),
            actionButton(ns("run"), "Jalankan Two-Stage IPD", class = "btn btn-primary w-100"),
            div(
              class = "fast-analysis-note",
              "Hasil utama dihitung terlebih dahulu agar lebih cepat. Diagnostik lanjutan dapat dihitung setelah model selesai."
            ),
            selectInput(
              ns("loo_mode"), "Metode Leave-One-Out",
              choices = c("Cepat: tau² tetap (skrining)" = "fast",
                          "Penuh: refit setiap cluster (lebih lama)" = "exact"),
              selected = "fast"
            ),
            checkboxInput(ns("ipd_influence"),
              "Hitung influence diagnostics tambahan (berat)", FALSE),
            actionButton(
              ns("run_diagnostics"),
              "Hitung Diagnostik Terpilih",
              class = "btn btn-outline-secondary w-100",
              disabled = "disabled"
            ),
            br(), br(),
            downloadButton(ns("download_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),

          div(
            uiOutput(ns("metrics")),
            navset_card_tab(
              nav_panel("Efek Gabungan", DTOutput(ns("summary_table"))),
              nav_panel("Regresi per Klaster", DTOutput(ns("stage1"))),
              nav_panel(
                "Forest Plot",
                div(
                  class = "plot-control-strip",
                  selectInput(
                    ns("forest_n"),
                    "Jumlah cluster pada grafik",
                    choices = c("20" = 20, "30" = 30, "50" = 50, "100" = 100),
                    selected = 30,
                    width = "180px"
                  ),
                  selectInput(
                    ns("forest_sort"),
                    "Prioritas tampilan",
                    choices = c(
                      "Bobot terbesar" = "weight",
                      "Efek terbesar absolut" = "abs_effect",
                      "SE terkecil" = "precision"
                    ),
                    selected = "weight",
                    width = "220px"
                  )
                ),
                div(
                  class = "fast-analysis-note",
                  "Forest plot adalah ringkasan visual. Pooled effect tetap dihitung dari seluruh cluster. Seluruh estimasi tersedia pada tab Regresi per Klaster."
                ),
                uiOutput(ns("forest_plot_ui"))
              ),
              nav_panel(
                "Funnel Plot",
                div(
                  class = "fast-analysis-note",
                  "Funnel plot menggunakan standard error (SE) pada sumbu vertikal dan seluruh cluster valid."
                ),
                plotOutput(ns("funnel"), height = "520px")
              ),
              nav_panel("Leave-One-Out",
                uiOutput(ns("loo_method_note")), DTOutput(ns("leave1out"))),
              nav_panel("Meta-Regression",
                div(class = "fast-analysis-note",
                  "Meta-regression dihitung dari hasil Stage-1 yang sudah tersedia; tidak menjalankan ulang regresi per klaster."),
                actionButton(ns("run_meta_reg"), "Hitung Meta-Regression",
                  class = "btn btn-outline-primary"),
                DTOutput(ns("meta_reg"))),
              nav_panel("Uji Tambahan IPD",
                p("Analisis lanjutan diestimasi pada efek klaster tahap pertama. Uji Egger dan trim-and-fill bersifat eksploratif; asumsi studi independen harus dinilai peneliti."),
                checkboxGroupInput(ns("extra_options"), "Pilih prosedur",
                  choices = c("Prediction interval" = "prediction",
                              "Egger / small-study effects" = "egger",
                              "Trim and fill" = "trimfill"), selected = "prediction"),
                actionButton(ns("run_extra"), "Hitung Uji Tambahan IPD", class = "btn btn-outline-primary"),
                uiOutput(ns("extra_tables"))
              ),
              nav_panel("R Syntax", verbatimTextOutput(ns("syntax")))
            )
          )
        )
      ),

      nav_panel(
        "One-Stage IPD",
        layout_columns(
          col_widths = c(4, 8),

          card(
            card_header("Pengaturan One-Stage"),
            selectInput(ns("os_outcome"), "Outcome (Y)", choices = NULL),
            selectInput(ns("os_predictor"), "Prediktor Utama (X)", choices = NULL),
            selectInput(ns("os_cluster"), "Cluster / Study ID", choices = NULL),
            selectizeInput(ns("os_covariates"), "Kovariat", choices = NULL, multiple = TRUE),
            checkboxInput(ns("os_random_slope"), "Random slope for predictor", FALSE),
            checkboxInput(ns("os_reml"), "Use REML", TRUE),
            actionButton(ns("run_one"), "Jalankan One-Stage IPD", class = "btn btn-primary w-100"),
            br(), br(),
            downloadButton(ns("download_one_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),

          div(
            uiOutput(ns("one_metrics")),
            navset_card_tab(
              nav_panel("Ringkasan", DTOutput(ns("one_summary"))),
              nav_panel("Efek Tetap", DTOutput(ns("one_fixed"))),
              nav_panel("Diagnostik", plotOutput(ns("one_diag"), height = "440px")),
              nav_panel("R Syntax", verbatimTextOutput(ns("one_syntax")))
            )
          )
        )
      )
    )
  )
}

mod_ipd_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    quick_start_server(
      input, output, session, shared,
      example_path = "data/examples/demo_ipd_an_paud.csv", example_name = "Contoh IPD — AN 2025 Guru PAUD",
      source = "Kemendikdasmen — Asesmen Nasional 2025", notes = "Subset latihan untuk memahami alur IPD.",
      template_path = "data/templates/ipd_template.csv"
    )



    observeEvent(shared$mapping_token, {
      req(shared$data, shared$auto_map)
      mp <- shared$auto_map$ipd
      vars <- names(shared$data)
      nums <- numeric_vars(shared$data)

      if (!is.null(mp$outcome) && mp$outcome %in% nums) {
        updateSelectInput(session, "outcome", choices = nums, selected = mp$outcome)
        updateSelectInput(session, "os_outcome", choices = nums, selected = mp$outcome)
      }

      if (!is.null(mp$predictor) && mp$predictor %in% nums) {
        updateSelectInput(session, "predictor", choices = nums, selected = mp$predictor)
        updateSelectInput(session, "os_predictor", choices = nums, selected = mp$predictor)
      }

      if (!is.null(mp$cluster) && mp$cluster %in% vars) {
        updateSelectInput(session, "cluster", choices = vars, selected = mp$cluster)
        updateSelectInput(session, "os_cluster", choices = vars, selected = mp$cluster)
      }

      updateSelectizeInput(
        session, "covariates",
        choices = vars,
        selected = intersect(mp$covariates %||% character(0), vars),
        server = TRUE
      )
      updateSelectizeInput(
        session, "os_covariates",
        choices = vars,
        selected = intersect(mp$covariates %||% character(0), vars),
        server = TRUE
      )

      showNotification(
        "Variabel IPD Meta-Analysis telah dipetakan otomatis. Silakan periksa sebelum menjalankan analisis.",
        type = "message", duration = 6
      )
    }, ignoreInit = TRUE)

    result <- reactiveVal(NULL)
    observeEvent(input$load_example, {
      updateSelectInput(session, "outcome", choices=names(shared$data), selected="GD.4.1")
      updateSelectInput(session, "predictor", choices=names(shared$data), selected="GD.4.2")
      updateSelectInput(session, "cluster", choices=names(shared$data), selected="kd_kokab")
      updateSelectizeInput(session, "covariates", choices=names(shared$data), selected=c("GE.4.2"), server=TRUE)
      updateSelectInput(session, "os_outcome", choices=names(shared$data), selected="GD.4.1")
      updateSelectInput(session, "os_predictor", choices=names(shared$data), selected="GD.4.2")
      updateSelectInput(session, "os_cluster", choices=names(shared$data), selected="kd_kokab")
    })

    one_result <- reactiveVal(NULL)

    # Prevent showing a previously fitted meta-regression for a newly selected moderator.
    observeEvent(input$moderator, {
      current <- result()
      if (!is.null(current) && !is.null(current$meta_reg) &&
          !identical(current$meta_reg_moderator, input$moderator)) {
        current$meta_reg <- NULL
        current$meta_reg_moderator <- NULL
        result(current)
      }
    }, ignoreInit = TRUE)

    observe({
      req(shared$data)
      all_vars <- names(shared$data)
      num_vars <- numeric_vars(shared$data)

      updateSelectInput(session, "outcome", choices = num_vars)
      updateSelectInput(session, "predictor", choices = num_vars)
      updateSelectInput(session, "cluster", choices = all_vars)
      updateSelectizeInput(session, "covariates", choices = all_vars, server = TRUE)
      updateSelectInput(session, "moderator", choices = c("None" = "", num_vars))

      updateSelectInput(session, "os_outcome", choices = num_vars)
      updateSelectInput(session, "os_predictor", choices = num_vars)
      updateSelectInput(session, "os_cluster", choices = all_vars)
      updateSelectizeInput(session, "os_covariates", choices = all_vars, server = TRUE)
    })

    output$data_state <- renderUI({
      if (is.null(shared$data)) {
        div(class = "warning-box", "Load a dataset first from the Data menu.")
      } else {
        div(class = "success-box",
            strong(shared$data_name), br(),
            paste(format(nrow(shared$data), big.mark = ","), "rows ×", ncol(shared$data), "variables"))
      }
    })


    output$ipd_cluster_quality <- renderUI({
      req(shared$data, input$cluster)

      d <- tryCatch(
        ipd_cluster_diagnostics(shared$data, input$cluster, input$min_n),
        error = function(e) NULL
      )
      if (is.null(d)) return(NULL)

      cls <- if (isTRUE(d$suitable)) "sem-example-note" else "warning-box"

      div(
        class = cls,
        strong(
          paste0(
            "Cluster: ", format(d$clusters, big.mark=","),
            " | Memenuhi minimum N: ", format(d$valid_clusters, big.mark=","),
            " | Median n: ", sprintf("%.0f", d$median_n)
          )
        ),
        br(),
        if (d$clusters > 2000) {
          "Terlalu banyak cluster untuk mode interaktif. Pilih unit cluster/agregasi yang lebih tinggi."
        } else if (d$valid_clusters < 2) {
          "Tidak cukup cluster yang memenuhi minimum sampel."
        } else {
          "Struktur cluster siap untuk Two-Stage IPD."
        }
      )
    })

    observeEvent(input$run, {
      if (!try_acquire_analysis(shared, "Two-Stage IPD")) return()

      req(shared$data, input$outcome, input$predictor, input$cluster)

      ipd_check <- ipd_cluster_diagnostics(shared$data, input$cluster, input$min_n)
      if (!isTRUE(ipd_check$suitable)) {
        release_analysis(shared)
        msg <- if (ipd_check$clusters > 2000) {
          paste0(
            "Two-Stage IPD dibatalkan: ", format(ipd_check$clusters, big.mark=","),
            " cluster terlalu banyak untuk mode interaktif. Pilih unit cluster/agregasi lebih tinggi."
          )
        } else {
          paste0(
            "Two-Stage IPD dibatalkan: hanya ", ipd_check$valid_clusters,
            " cluster memenuhi minimum N=", input$min_n, "."
          )
        }
        showNotification(msg, type = "warning", duration = NULL)
        return()
      }

      dat_bg <- shared$data
      outcome_bg <- input$outcome
      predictor_bg <- input$predictor
      cluster_bg <- input$cluster
      covariates_bg <- input$covariates %||% character(0)
      method_bg <- input$method
      min_n_bg <- input$min_n
      moderator_bg <- input$moderator

      shinyjs::disable("run")
      shinyjs::disable("run_diagnostics")
      background_start_notification("Two-Stage IPD")

      p <- promises::future_promise({
        run_two_stage_ipd(
          data = dat_bg,
          outcome = outcome_bg,
          predictor = predictor_bg,
          cluster = cluster_bg,
          covariates = covariates_bg,
          method = method_bg,
          min_cluster_n = min_n_bg,
          moderator = moderator_bg,
          progress_callback = NULL,
          compute_diagnostics = FALSE
        )
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          result(fit)
          shinyjs::enable("run")
          shinyjs::enable("run_diagnostics")
          release_analysis(shared)
          showNotification(
            "Two-Stage IPD selesai. Output utama tersedia. Diagnostik lanjutan dapat dihitung bila diperlukan.",
            type = "message",
            duration = 10
          )
          NULL
        },
        onRejected = function(err) {
          result(NULL)
          shinyjs::enable("run")
          shinyjs::disable("run_diagnostics")
          release_analysis(shared)
          background_error_notification("Two-Stage IPD", err)
          NULL
        }
      )
    })

    observeEvent(input$run_diagnostics, {
      x_bg <- result(); req(x_bg)
      mode_bg <- input$loo_mode %||% "fast"
      influence_bg <- isTRUE(input$ipd_influence)
      if (isTRUE(x_bg$diagnostics_computed) &&
          identical(x_bg$diagnostics_mode, mode_bg) &&
          (!influence_bg || !is.null(x_bg$influence))) {
        showNotification("Diagnostik terpilih sudah dihitung; hasil lama digunakan.", type = "message")
        return()
      }
      if (!try_acquire_analysis(shared, "Diagnostik IPD")) return()
      shinyjs::disable("run_diagnostics")
      background_start_notification("Diagnostik IPD")
      # Transfer only what the calculation needs to the background worker.
      lightweight <- list(meta = x_bg$meta, stage1 = x_bg$stage1,
        leave1out = x_bg$leave1out, influence = x_bg$influence,
        diagnostics_computed = x_bg$diagnostics_computed)
      p <- promises::future_promise({
        compute_ipd_diagnostics(lightweight, mode = mode_bg,
          compute_influence = influence_bg)
      }, seed = TRUE)
      promises::then(p,
        onFulfilled = function(z) {
          current <- result()
          if (!is.null(current) && identical(current$meta$yi, x_bg$meta$yi) &&
              identical(current$meta$vi, x_bg$meta$vi)) {
            current$leave1out <- z$leave1out
            current$influence <- z$influence
            current$diagnostics_mode <- z$diagnostics_mode
            current$diagnostics_note <- z$diagnostics_note
            current$diagnostics_computed <- TRUE
            result(current)
          }
          shinyjs::enable("run_diagnostics")
          release_analysis(shared)
          background_success_notification("Diagnostik IPD")
          NULL
        },
        onRejected = function(err) {
          shinyjs::enable("run_diagnostics")
          release_analysis(shared)
          background_error_notification("Diagnostik IPD", err)
          NULL
        }
      )
    })

    observeEvent(input$run_meta_reg, {
      x <- result(); req(x)
      moderator_bg <- input$moderator %||% ""
      if (!nzchar(moderator_bg)) {
        showNotification("Pilih moderator numerik terlebih dahulu.", type = "warning")
        return()
      }
      if (!is.null(x$meta_reg) && identical(x$meta_reg_moderator, moderator_bg)) {
        showNotification("Meta-regression tersedia; hasil lama digunakan.", type = "message")
        return()
      }
      if (!try_acquire_analysis(shared, "Meta-Regression IPD")) return()
      shinyjs::disable("run_meta_reg")
      dat_bg <- shared$data[, c(x$cluster_var, moderator_bg), drop = FALSE]
      stage_bg <- x$stage1
      cluster_bg <- x$cluster_var
      method_bg <- x$method
      p <- promises::future_promise({
        fit_ipd_meta_regression(stage_bg, dat_bg, cluster_bg,
                                moderator_bg, method_bg)
      }, seed = TRUE)
      promises::then(p, onFulfilled = function(z) {
        current <- result()
        if (!is.null(current) && identical(current$stage1$beta, stage_bg$beta)) {
          current$meta_reg <- z$model
          current$meta_reg_moderator <- moderator_bg
          result(current)
        }
        shinyjs::enable("run_meta_reg")
        release_analysis(shared)
        background_success_notification("Meta-Regression IPD")
        NULL
      }, onRejected = function(e) {
        shinyjs::enable("run_meta_reg")
        release_analysis(shared)
        background_error_notification("Meta-Regression IPD", e)
        NULL
      })
    })

    observeEvent(input$run_extra, {
      x <- result(); req(x)
      opts <- input$extra_options %||% character(0)
      if (!length(opts)) {
        showNotification("Pilih satu atau lebih prosedur.", type = "warning")
        return()
      }
      map <- c(prediction = "Prediction interval",
               egger = "Small study effects (Egger)",
               trimfill = "Trim and fill")
      prev <- x$extra %||% list()
      done <- prev$Status
      if (!is.null(done) && nrow(done)) {
        completed <- as.character(done$Uji[done$Status == "OK"])
        opts <- opts[!(unname(map[opts]) %in% completed)]
      }
      if (!length(opts)) {
        showNotification("Semua uji terpilih sudah tersedia; hasil cache digunakan.", type = "message")
        return()
      }
      if (!try_acquire_analysis(shared, "Uji Tambahan IPD")) return()
      shinyjs::disable("run_extra")
      # Avoid sending the entire Stage-1 dataset back to the worker.
      slim <- list(meta = x$meta)
      p <- promises::future_promise(ara_ipd_extra(slim, opts), seed = TRUE)
      promises::then(p, onFulfilled = function(z) {
        current <- result()
        if (!is.null(current) && identical(current$meta$yi, x$meta$yi) &&
            identical(current$meta$vi, x$meta$vi)) {
          old <- current$extra %||% list()
          old_status <- old$Status
          new_status <- z$Status
          old$Status <- NULL
          z$Status <- NULL
          combined <- utils::modifyList(old, z)
          if (!is.null(old_status)) {
            new_status <- rbind(old_status[!old_status$Uji %in% new_status$Uji, , drop = FALSE],
              new_status)
          }
          combined$Status <- new_status
          current$extra <- combined
          result(current)
        }
        shinyjs::enable("run_extra")
        release_analysis(shared)
        showNotification("Uji tambahan IPD selesai.", type = "message")
        NULL
      }, onRejected = function(e) {
        shinyjs::enable("run_extra")
        release_analysis(shared)
        background_error_notification("Uji tambahan IPD", e)
        NULL
      })
    })
    output$extra_tables <- renderUI({
      x <- result(); req(x, x$extra)
      ara_collapsible_result_tables(x$extra, max_rows = 50L)
    })

    output$metrics <- renderUI({
      x <- result()
      if (is.null(x)) {
        return(card(div(class = "analysis-placeholder", h4("No IPD model has been run"), p("Choose variables and click Jalankan Two-Stage IPD."))))
      }

      div(
        class = "metric-row hlm-metrics",
        div(class = "metric-box", div(class = "metric-value", x$k), div(class = "metric-label", "Clusters")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", x$pooled)), div(class = "metric-label", "Pooled Effect")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f%%", x$I2)), div(class = "metric-label", "I²")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", x$tau2)), div(class = "metric-label", "τ²")),
        div(class = "metric-box", div(class = "metric-value", ifelse(x$p < .001, "<.001", sprintf("%.2f", x$p))), div(class = "metric-label", "Pooled p"))
      )
    })

    output$summary_table <- renderDT({
      x <- result(); req(x)
      ara_datatable(ipd_pooled_publication_table(x), rownames = FALSE, options = list(scrollX = TRUE, dom = "t"))
    })

    output$stage1 <- renderDT({
      x <- result(); req(x)
      ara_datatable(
        ipd_stage1_publication_table(x),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 15)
      )
    })


    forest_display_data <- reactive({
      x <- result()
      req(x, x$stage1)

      d <- as.data.frame(x$stage1, stringsAsFactors = FALSE)
      d <- d[
        is.finite(d$beta) &
          is.finite(d$se) &
          is.finite(d$ci_low) &
          is.finite(d$ci_high),
        ,
        drop = FALSE
      ]

      req(nrow(d) > 0)

      n_show <- min(as.integer(input$forest_n %||% 30), nrow(d))
      mode <- input$forest_sort %||% "weight"

      if (identical(mode, "abs_effect")) {
        idx <- order(abs(d$beta), decreasing = TRUE, na.last = NA)
      } else if (identical(mode, "precision")) {
        idx <- order(d$se, decreasing = FALSE, na.last = NA)
      } else {
        if ("weight_pct" %in% names(d) && any(is.finite(d$weight_pct))) {
          idx <- order(d$weight_pct, decreasing = TRUE, na.last = NA)
        } else {
          idx <- order(d$se, decreasing = FALSE, na.last = NA)
        }
      }

      d <- d[head(idx, n_show), , drop = FALSE]

      # After selecting the most informative clusters, order visually by effect.
      d <- d[order(d$beta, decreasing = FALSE), , drop = FALSE]
      rownames(d) <- NULL
      d
    })

    output$forest_plot_ui <- renderUI({
      d <- forest_display_data()
      h <- max(520, min(1150, 180 + 20 * nrow(d)))
      plotOutput(session$ns("forest"), height = paste0(h, "px"))
    })

    output$forest <- renderPlot({
      x <- result()
      d <- forest_display_data()
      req(x, nrow(d) > 0)

      n <- nrow(d)
      y <- seq_len(n)
      y <- rev(y)

      ci_min <- min(c(d$ci_low, x$ci_low), na.rm = TRUE)
      ci_max <- max(c(d$ci_high, x$ci_high), na.rm = TRUE)
      pad <- max((ci_max - ci_min) * .10, .05)
      xlim <- c(ci_min - pad, ci_max + pad)

      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)

      # Give room on the left for cluster labels.
      par(mar = c(5, 8.5, 3.5, 2))

      plot(
        NA,
        xlim = xlim,
        ylim = c(0, n + 1.4),
        xlab = "Estimasi efek (B) dengan 95% CI",
        ylab = "",
        yaxt = "n",
        bty = "n"
      )

      abline(v = 0, lty = 3, lwd = .9)
      abline(v = x$pooled, lty = 2, lwd = 1.15)

      segments(
        x0 = d$ci_low, y0 = y,
        x1 = d$ci_high, y1 = y,
        lwd = .9
      )

      if ("weight_pct" %in% names(d) && any(is.finite(d$weight_pct))) {
        w <- d$weight_pct
        w[!is.finite(w)] <- min(w[is.finite(w)], na.rm = TRUE)
        cex_pts <- .75 + 1.15 * sqrt(w / max(w, na.rm = TRUE))
      } else {
        cex_pts <- rep(1, n)
      }

      points(d$beta, y, pch = 15, cex = cex_pts)

      axis(
        2,
        at = y,
        labels = as.character(d$cluster),
        las = 1,
        tick = FALSE,
        cex.axis = if (n <= 30) .72 else if (n <= 50) .62 else .52
      )

      # Full-analysis pooled estimate shown as a diamond at the bottom.
      py <- .55
      diamond_y <- .16
      polygon(
        x = c(x$ci_low, x$pooled, x$ci_high, x$pooled),
        y = c(py, py + diamond_y, py, py - diamond_y),
        border = "black",
        col = "white",
        lwd = 1.1
      )

      mtext(
        paste0(
          "Ditampilkan ", n, " dari ", x$k,
          " cluster | Pooled B = ", sprintf("%.2f", x$pooled),
          " [", sprintf("%.2f", x$ci_low), ", ", sprintf("%.2f", x$ci_high), "]"
        ),
        side = 3,
        line = .5,
        adj = 0,
        cex = .78
      )
    })

    output$funnel <- renderPlot({
      x <- result()
      req(x, x$meta)

      ok <- tryCatch({
        metafor::funnel(
          x$meta,
          yaxis = "sei",
          xlab = "Estimasi efek (B)",
          ylab = "Standard Error",
          pch = 21,
          bg = "white"
        )
        TRUE
      }, error = function(e) {
        FALSE
      })

      if (!isTRUE(ok)) {
        d <- as.data.frame(x$stage1, stringsAsFactors = FALSE)
        d <- d[is.finite(d$beta) & is.finite(d$se), , drop = FALSE]

        plot(
          d$beta,
          d$se,
          pch = 21,
          bg = "white",
          xlab = "Estimasi efek (B)",
          ylab = "Standard Error",
          bty = "n"
        )
        abline(v = x$pooled, lty = 2, lwd = 1)
        axis(2)
      }
    })

    output$loo_method_note <- renderUI({
      x <- result(); req(x)
      if (is.null(x$diagnostics_note)) return(NULL)
      div(class = "fast-analysis-note", x$diagnostics_note)
    })
    output$leave1out <- renderDT({
      x <- result(); req(x)
      if (is.null(x$leave1out)) {
        return(ara_datatable(
          data.frame(Catatan = "Klik 'Hitung Diagnostik Lanjutan' untuk menghitung leave-one-out."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      ara_datatable(as.data.frame(x$leave1out), rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 15))
    })

    output$meta_reg <- renderDT({
      x <- result(); req(x)
      if (is.null(x$meta_reg))
        return(ara_output_table(NULL,
          "Klik Hitung Meta-Regression setelah memilih moderator numerik."))
      tab <- tryCatch(as.data.frame(coef(summary(x$meta_reg))),
        error = function(e) NULL)
      if (!is.null(tab)) {
        tab$Parameter <- rownames(tab)
        rownames(tab) <- NULL
        tab <- tab[, c("Parameter", setdiff(names(tab), "Parameter")), drop = FALSE]
      }
      ara_output_table(tab, "Hasil meta-regresi belum tersedia.")
    })

    output$syntax <- renderText({
      req(input$outcome, input$predictor, input$cluster)
      rhs <- paste(c(input$predictor, input$covariates), collapse = " + ")
      paste0(
        "# Stage 1\n",
        "lm(", input$outcome, " ~ ", rhs, ", data = cluster_data)\n\n",
        "# Stage 2\n",
        "metafor::rma(yi = beta, sei = se, method = \"", input$method, "\")"
      )
    })

    output$download_excel <- downloadHandler(
      filename = function() paste0("Two_Stage_IPD_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- result(); req(x)
        export_analysis_excel(
          prepend_analysis_info(
            ipd_two_stage_excel_tables(x),
            "Two-Stage IPD Meta-Analysis",
            provenance = shared$provenance,
            extra = list(
              Outcome = input$outcome,
              Predictor = input$predictor,
              Cluster = input$cluster
            )
          ),
          file
        )
      }
    )

    observeEvent(input$run_one, {
      if (!try_acquire_analysis(shared, "One-Stage IPD")) return()

      req(shared$data, input$os_outcome, input$os_predictor, input$os_cluster)

      dat_bg <- shared$data
      outcome_bg <- input$os_outcome
      predictor_bg <- input$os_predictor
      cluster_bg <- input$os_cluster
      covariates_bg <- input$os_covariates %||% character(0)
      random_slope_bg <- isTRUE(input$os_random_slope)
      reml_bg <- isTRUE(input$os_reml)

      shinyjs::disable("run_one")
      background_start_notification("One-Stage IPD")

      p <- promises::future_promise({
        run_one_stage_ipd(
          data = dat_bg,
          outcome = outcome_bg,
          predictor = predictor_bg,
          cluster = cluster_bg,
          covariates = covariates_bg,
          random_slope = random_slope_bg,
          reml = reml_bg
        )
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          one_result(fit)
          shinyjs::enable("run_one")
          release_analysis(shared)
          background_success_notification("One-Stage IPD")
          NULL
        },
        onRejected = function(err) {
          one_result(NULL)
          shinyjs::enable("run_one")
          release_analysis(shared)
          background_error_notification("One-Stage IPD", err)
          NULL
        }
      )
    })

    output$one_metrics <- renderUI({
      x <- one_result()
      if (is.null(x)) {
        return(card(div(class = "analysis-placeholder", h4("No one-stage model has been run"), p("Choose variables and click Jalankan One-Stage IPD."))))
      }

      div(
        class = "metric-row",
        div(class = "metric-box", div(class = "metric-value", format(x$n, big.mark = ",")), div(class = "metric-label", "Observations")),
        div(class = "metric-box", div(class = "metric-value", x$clusters), div(class = "metric-label", "Clusters")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", x$r2_marginal)), div(class = "metric-label", "Marginal R²"))
      )
    })

    output$one_summary <- renderDT({
      x <- one_result(); req(x)
      tabs <- ipd_one_stage_excel_tables(x)
      ara_output_table(tabs[["Ringkasan_Model"]] %||% tabs[[1L]])
    })

    output$one_fixed <- renderDT({
      x <- one_result(); req(x)
      ara_datatable(x$coefficients, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 12))
    })

    output$one_diag <- renderPlot({
      x <- one_result(); req(x)
      plot(
        fitted(x$model), residuals(x$model),
        xlab = "Fitted values",
        ylab = "Residuals",
        main = "Residuals vs Fitted"
      )
      abline(h = 0, lty = 2)
    })

    output$one_syntax <- renderText({
      req(input$os_outcome, input$os_predictor, input$os_cluster)
      fixed <- paste(c(input$os_predictor, input$os_covariates), collapse = " + ")
      random <- if (isTRUE(input$os_random_slope)) {
        paste0("(1 + ", input$os_predictor, " | ", input$os_cluster, ")")
      } else {
        paste0("(1 | ", input$os_cluster, ")")
      }

      paste0(
        "lmerTest::lmer(",
        input$os_outcome, " ~ ", fixed, " + ", random,
        ", data = data, REML = ", ifelse(isTRUE(input$os_reml), "TRUE", "FALSE"), ")"
      )
    })

    output$download_one_excel <- downloadHandler(
      filename = function() paste0("One_Stage_IPD_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- one_result(); req(x)
        export_analysis_excel(
          prepend_analysis_info(
            ipd_one_stage_excel_tables(x),
            "One-Stage IPD Meta-Analysis",
            provenance = shared$provenance,
            extra = list(
              Outcome = input$os_outcome,
              Predictor = input$os_predictor,
              Cluster = input$os_cluster
            )
          ),
          file
        )
      }
    )
  })
}
