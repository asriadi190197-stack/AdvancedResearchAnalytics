# Helper di lingkup global: dipanggil oleh server dan UI dinamis.
# Panel hanya menampilkan tabel; angka analisis dan ekspor tidak diubah.
mod_multivariate_section_accordion <- function(id, items, prefix = "tbl", empty_message = "Hasil belum tersedia.") {
  if (!length(items)) return(shiny::p(empty_message))
  panels <- lapply(seq_along(items), function(i) {
    nm <- names(items)[i]
    bslib::accordion_panel(
      title = gsub("_", " ", nm, fixed = TRUE),
      shiny::div(class = "ara-result-table-wrap", DT::DTOutput(paste0(prefix, "_", i)))
    )
  })
  # do.call memastikan panel benar-benar menjadi argumen accordion.
  do.call(bslib::accordion, c(list(id = id, open = FALSE, multiple = FALSE), panels))
}

mod_multivariate_ui <- function(id) {
  ns <- NS(id)


  fluidPage(
    div(
      class = "page-heading",
      tags$span(class = "eyebrow", "MULTIVARIATE ANALYSIS"),

    usage_guide(
      "Cara menggunakan Multivariate Analisis",
      c(
        "Pilih tab MANOVA/MANCOVA, Multivariate Regression, Discriminant, atau Cluster Analisis.",
        "Masukkan hanya variabel yang sesuai dengan metode yang dipilih.",
        "Jalankan analisis dan periksa multivariate tests atau classification/clustering diagnostics.",
        "Untuk Discriminant Analisis, bandingkan training accuracy dengan cross-validation.",
        "Untuk Cluster Analisis, gunakan silhouette dan interpretasi substantif, bukan hanya jumlah cluster."
      )
    ),

      h2("Multivariate & Classification"),
      p("MANOVA, MANCOVA, multivariate regression, discriminant analysis, and cluster analysis.")
    ),

    navset_card_tab(
      nav_panel(
        "MANOVA / MANCOVA",
        quick_start_card(ns, "Multivariate: Mulai Cepat", "Contoh yang sama dapat digunakan untuk MANOVA/MANCOVA, Discriminant Analisis, dan Cluster Analisis.", "Sumber contoh: subset ringan dari dataset Indonesia yang Anda berikan."),

    layout_columns(
          col_widths = c(4,8),
          card(
            card_header("Pengaturan Model"),
            uiOutput(ns("data_state")),
            selectizeInput(ns("mv_outcomes"), "Variabel Dependen", choices = NULL, multiple = TRUE),
            selectInput(ns("mv_factor"), "Variabel Faktor / Kelompok", choices = NULL),
            selectizeInput(ns("mv_covariates"), "Kovariat (opsional)", choices = NULL, multiple = TRUE),
            actionButton(ns("run_manova"), "Jalankan MANOVA / MANCOVA", class = "btn btn-primary w-100"),
            br(), br(),
            downloadButton(ns("download_manova_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          navset_card_tab(
            nav_panel("Uji Multivariat", DTOutput(ns("mv_tests"))),
            nav_panel("Uji Lanjutan Univariat", uiOutput(ns("mv_uni_tables"))),
            nav_panel("R Syntax", verbatimTextOutput(ns("mv_syntax")))
          )
        )
      ),

      nav_panel(
        "Multivariate Regression",
        layout_columns(
          col_widths = c(4,8),
          card(
            card_header("Pengaturan Regresi"),
            selectizeInput(ns("mr_outcomes"), "Variabel Outcome", choices = NULL, multiple = TRUE),
            selectizeInput(ns("mr_predictors"), "Prediktor", choices = NULL, multiple = TRUE),
            actionButton(ns("run_mr"), "Jalankan Multivariate Regression", class = "btn btn-primary w-100"),
            br(), br(),
            downloadButton(ns("download_mr_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          navset_card_tab(
            nav_panel("Uji Multivariat", DTOutput(ns("mr_tests"))),
            nav_panel("Koefisien", uiOutput(ns("mr_coef_tables"))),
            nav_panel("Sintaks R", verbatimTextOutput(ns("mr_syntax")))
          )
        )
      ),

      nav_panel(
        "Discriminant Analisis",
        layout_columns(
          col_widths = c(4,8),
          card(
            card_header("Pengaturan Discriminant Analisis"),
            selectInput(ns("da_group"), "Variabel Kelompok", choices = NULL),
            selectizeInput(ns("da_predictors"), "Prediktor", choices = NULL, multiple = TRUE),
            radioButtons(ns("da_method"), "Metode", choices = c("LDA"="lda","QDA"="qda"), inline = TRUE),
            checkboxInput(ns("da_cv"), "Cross-validation", TRUE),
            actionButton(ns("run_da"), "Jalankan Discriminant Analisis", class = "btn btn-primary w-100"),
            br(), br(),
            downloadButton(ns("download_da_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          div(
            uiOutput(ns("da_metrics")),
            navset_card_tab(
              nav_panel("Matriks Klasifikasi", DTOutput(ns("da_conf"))),
              nav_panel("Cross-Validation", DTOutput(ns("da_cv_conf"))),
              nav_panel("Parameter Model", uiOutput(ns("da_model_tables"))),
              nav_panel("Canonical Plot", plotOutput(ns("da_plot"), height = "500px")),
              nav_panel("Sintaks R", verbatimTextOutput(ns("da_syntax")))
            )
          )
        )
      ),

      nav_panel(
        "Cluster Analisis",
        layout_columns(
          col_widths = c(4,8),
          card(
            card_header("Pengaturan Cluster Analisis"),
            selectizeInput(ns("cl_vars"), "Variabel Numerik", choices = NULL, multiple = TRUE),
            selectInput(ns("cl_method"), "Metode", choices = c("K-Means"="kmeans","PAM / K-Medoids"="pam","Hierarchical"="hierarchical")),
            numericInput(ns("cl_k"), "Jumlah Cluster", value = 3, min = 2, max = 12),
            checkboxInput(ns("cl_scale"), "Standarkan variabel", TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'hierarchical'", ns("cl_method")),
              selectInput(ns("cl_distance"), "Distance", choices = c("Euclidean"="euclidean","Manhattan"="manhattan")),
              selectInput(ns("cl_linkage"), "Linkage", choices = c("Ward"="ward.D2","Complete"="complete","Average"="average"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'hierarchical' || input['%s'] == 'pam'", ns("cl_method"), ns("cl_method")),
              checkboxInput(ns("cl_sample_large"), "Gunakan sampel aman jika data terlalu besar", TRUE),
              numericInput(ns("cl_max_cases"), "Maksimum kasus metode berbasis jarak", value = 2000, min = 100, max = 3000, step = 100),
              helpText("Hierarchical dan PAM memerlukan memori kuadratik. Sampel acak dengan seed tetap akan dicatat dalam hasil dan Excel.")
            ),
            actionButton(ns("run_cluster"), "Jalankan Cluster Analisis", class = "btn btn-primary w-100"),
            br(), br(),
            downloadButton(ns("download_cluster_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          div(
            uiOutput(ns("cl_metrics")),
            navset_card_tab(
              nav_panel("Ukuran Cluster", DTOutput(ns("cl_sizes"))),
              nav_panel("Centers / Medoids", DTOutput(ns("cl_centers"))),
              nav_panel("Visualisasi", plotOutput(ns("cl_plot"), height = "520px")),
              nav_panel("Dendrogram", plotOutput(ns("cl_dend"), height = "520px")),
              nav_panel("Sintaks R", verbatimTextOutput(ns("cl_syntax")))
            )
          )
        )
      )
    )
  )
}

mod_multivariate_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    quick_start_server(
      input, output, session, shared,
      example_path = "data/examples/demo_sem_multivariate_idn.csv", example_name = "Contoh Multivariate — Data Indonesia",
      source = "Dataset latihan Indonesia", notes = "Subset latihan multivariate/classification.",
      template_path = "data/templates/multivariate_template.csv"
    )



    observeEvent(shared$mapping_token, {
      req(shared$data, shared$auto_map)
      mp <- shared$auto_map$multivariate
      vars <- names(shared$data)
      nums <- numeric_vars(shared$data)

      updateSelectizeInput(
        session, "mv_outcomes",
        choices = nums,
        selected = intersect(mp$outcomes %||% character(0), nums),
        server = TRUE
      )
      if (!is.null(mp$group) && mp$group %in% vars)
        updateSelectInput(session, "mv_factor", choices = vars, selected = mp$group)

      updateSelectizeInput(
        session, "mr_outcomes",
        choices = nums,
        selected = intersect(mp$outcomes %||% character(0), nums),
        server = TRUE
      )
      updateSelectizeInput(
        session, "mr_predictors",
        choices = vars,
        selected = intersect(mp$predictors %||% character(0), vars),
        server = TRUE
      )

      if (!is.null(mp$group) && mp$group %in% vars)
        updateSelectInput(session, "da_group", choices = vars, selected = mp$group)

      updateSelectizeInput(
        session, "da_predictors",
        choices = nums,
        selected = intersect(mp$predictors %||% character(0), nums),
        server = TRUE
      )

      updateSelectizeInput(
        session, "cl_vars",
        choices = nums,
        selected = intersect(mp$cluster_vars %||% character(0), nums),
        server = TRUE
      )

      showNotification(
        "Variabel Multivariate telah dipetakan otomatis sebagai titik awal.",
        type = "message", duration = 6
      )
    }, ignoreInit = TRUE)

    man_res <- reactiveVal(NULL)
    observeEvent(input$load_example, {
      nm <- names(shared$data)
      updateSelectizeInput(session, "mv_outcomes", choices=nm, selected=c("DCR_1","DCR_2","DCR_3"), server=TRUE)
      updateSelectInput(session, "mv_factor", choices=nm, selected="Gender")
      updateSelectizeInput(session, "mr_outcomes", choices=nm, selected=c("DCR_1","DCR_2","DCR_3"), server=TRUE)
      updateSelectizeInput(session, "mr_predictors", choices=nm, selected=c("AIS_1","AIS_2","AIS_3"), server=TRUE)
      updateSelectInput(session, "da_group", choices=nm, selected="Gender")
      updateSelectizeInput(session, "da_predictors", choices=nm, selected=c("DCR_1","DCR_2","DCR_3","DCR_4","DCR_5"), server=TRUE)
      updateSelectizeInput(session, "cl_vars", choices=nm, selected=c("DCR_1","DCR_2","DCR_3","AIS_1","AIS_2"), server=TRUE)
    })

    mr_res <- reactiveVal(NULL)
    da_res <- reactiveVal(NULL)
    cl_res <- reactiveVal(NULL)

    observe({
      req(shared$data)
      all_vars <- names(shared$data)
      num_vars <- numeric_vars(shared$data)

      updateSelectizeInput(session, "mv_outcomes", choices = num_vars, server = TRUE)
      updateSelectInput(session, "mv_factor", choices = all_vars)
      updateSelectizeInput(session, "mv_covariates", choices = num_vars, server = TRUE)

      updateSelectizeInput(session, "mr_outcomes", choices = num_vars, server = TRUE)
      updateSelectizeInput(session, "mr_predictors", choices = all_vars, server = TRUE)

      updateSelectInput(session, "da_group", choices = all_vars)
      updateSelectizeInput(session, "da_predictors", choices = num_vars, server = TRUE)

      updateSelectizeInput(session, "cl_vars", choices = num_vars, server = TRUE)
    })

    output$data_state <- renderUI({
      if (is.null(shared$data)) {
        div(class = "warning-box", "Load a dataset first from the Data menu.")
      } else {
        div(class = "success-box",
            strong(shared$data_name), br(),
            paste(format(nrow(shared$data), big.mark=","), "rows ×", ncol(shared$data), "variables"))
      }
    })

    observeEvent(input$run_manova, {
      if (!try_acquire_analysis(shared, "MANOVA / MANCOVA")) return()

      req(shared$data, input$mv_outcomes, input$mv_factor)

      dat_bg <- shared$data
      outcomes_bg <- input$mv_outcomes
      factor_bg <- input$mv_factor
      covariates_bg <- input$mv_covariates %||% character(0)

      shinyjs::disable("run_manova")
      background_start_notification("MANOVA/MANCOVA")

      p <- promises::future_promise({
        fit_manova_model(dat_bg, outcomes_bg, factor_bg, covariates_bg)
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          man_res(fit)
          shinyjs::enable("run_manova")
          release_analysis(shared)
          background_success_notification("MANOVA/MANCOVA")
          NULL
        },
        onRejected = function(err) {
          man_res(NULL)
          shinyjs::enable("run_manova")
          release_analysis(shared)
          background_error_notification("MANOVA/MANCOVA", err)
          NULL
        }
      )
    })

    output$mv_tests <- renderDT({
      x <- man_res(); req(x)
      ara_datatable(x$multivariate_tests, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$mv_uni_tables <- renderUI({
      x <- man_res(); req(x)
      tabs <- manova_univariate_tables(x)
      tabs <- tabs[grepl("^Univariat_", names(tabs))]
      if (!length(tabs)) return(p("Hasil uji univariat belum tersedia."))
      names(tabs) <- sub("^Univariat_", "Outcome: ", names(tabs))
      lapply(seq_along(tabs), function(i) {
        local({
          j <- i
          output[[paste0("mv_univariate_", j)]] <- renderDT(ara_output_table(tabs[[j]]))
        })
      })
      mod_multivariate_section_accordion(
        id = session$ns("mv_uni_acc"),
        items = tabs,
        prefix = session$ns("mv_univariate"),
        empty_message = "Hasil uji univariat belum tersedia."
      )
    })

    output$mv_syntax <- renderText({
      req(input$mv_outcomes, input$mv_factor)
      lhs <- paste0("cbind(", paste(input$mv_outcomes, collapse=", "), ")")
      rhs <- paste(c(input$mv_factor, input$mv_covariates), collapse=" + ")
      paste0("manova(", lhs, " ~ ", rhs, ", data = data)")
    })

    output$download_manova_excel <- downloadHandler(
      filename = function() paste0("MANOVA_MANCOVA_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- man_res(); req(x)
        export_analysis_excel(prepend_analysis_info(manova_univariate_tables(x), "MANOVA / MANCOVA", shared$provenance), file)
      }
    )

    observeEvent(input$run_mr, {
      if (!try_acquire_analysis(shared, "Multivariate Regression")) return()

      req(shared$data, input$mr_outcomes, input$mr_predictors)

      dat_bg <- shared$data
      outcomes_bg <- input$mr_outcomes
      predictors_bg <- input$mr_predictors

      shinyjs::disable("run_mr")
      background_start_notification("Multivariate Regression")

      p <- promises::future_promise({
        fit_multivar_regression(dat_bg, outcomes_bg, predictors_bg)
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          mr_res(fit)
          shinyjs::enable("run_mr")
          release_analysis(shared)
          background_success_notification("Multivariate Regression")
          NULL
        },
        onRejected = function(err) {
          mr_res(NULL)
          shinyjs::enable("run_mr")
          release_analysis(shared)
          background_error_notification("Multivariate Regression", err)
          NULL
        }
      )
    })

    output$mr_tests <- renderDT({
      x <- mr_res(); req(x)
      ara_datatable(x$multivariate_tests, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$mr_coef_tables <- renderUI({
      x <- mr_res(); req(x)
      tabs <- multivar_reg_excel_tables(x)
      tabs <- tabs[grepl("^Koef_", names(tabs))]
      if (!length(tabs)) return(p("Koefisien belum tersedia."))
      names(tabs) <- sub("^Koef_", "Outcome: ", names(tabs))
      lapply(seq_along(tabs), function(i) {
        local({
          j <- i
          output[[paste0("mr_coeff_", j)]] <- renderDT(ara_output_table(tabs[[j]]))
        })
      })
      mod_multivariate_section_accordion(
        id = session$ns("mr_coef_acc"),
        items = tabs,
        prefix = session$ns("mr_coeff"),
        empty_message = "Koefisien belum tersedia."
      )
    })

    output$mr_syntax <- renderText({
      req(input$mr_outcomes, input$mr_predictors)
      lhs <- paste0("cbind(", paste(input$mr_outcomes, collapse=", "), ")")
      rhs <- paste(input$mr_predictors, collapse=" + ")
      paste0("lm(", lhs, " ~ ", rhs, ", data = data)")
    })

    observeEvent(input$run_da, {
      if (!try_acquire_analysis(shared, "Discriminant Analysis")) return()

      req(shared$data, input$da_group, input$da_predictors)

      dat_bg <- shared$data
      group_bg <- input$da_group
      predictors_bg <- input$da_predictors
      method_bg <- input$da_method
      cv_bg <- isTRUE(input$da_cv)

      shinyjs::disable("run_da")
      background_start_notification("Discriminant Analysis")

      p <- promises::future_promise({
        fit_discriminant(dat_bg, group_bg, predictors_bg, method_bg, cv_bg)
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          da_res(fit)
          shinyjs::enable("run_da")
          release_analysis(shared)
          background_success_notification("Discriminant Analysis")
          NULL
        },
        onRejected = function(err) {
          da_res(NULL)
          shinyjs::enable("run_da")
          release_analysis(shared)
          background_error_notification("Discriminant Analysis", err)
          NULL
        }
      )
    })

    output$da_metrics <- renderUI({
      x <- da_res()
      if (is.null(x)) return(card(div(class="analysis-placeholder", h4("No discriminant model has been run"))))
      div(
        class="metric-row",
        div(class="metric-box", div(class="metric-value", format(x$n, big.mark=",")), div(class="metric-label","N")),
        div(class="metric-box", div(class="metric-value", sprintf("%.2f%%", 100*x$accuracy)), div(class="metric-label","Training Accuracy")),
        div(class="metric-box", div(class="metric-value", ifelse(is.na(x$cv_accuracy), "—", sprintf("%.2f%%",100*x$cv_accuracy))), div(class="metric-label","CV Accuracy"))
      )
    })

    output$da_conf <- renderDT({
      x <- da_res(); req(x)
      ara_datatable(as.data.frame.matrix(x$confusion), options=list(dom="t"))
    })

    output$da_cv_conf <- renderDT({
      x <- da_res(); req(x)
      if (is.null(x$cv_confusion)) return(ara_datatable(data.frame(Note="Cross-validation disabled."), rownames=FALSE))
      ara_datatable(as.data.frame.matrix(x$cv_confusion), options=list(dom="t"))
    })

    output$da_model_tables <- renderUI({
      x <- da_res(); req(x)
      tabs <- discriminant_excel_tables(x)
      tabs <- tabs[names(tabs) %in% c("Prior_Probability", "Jumlah_per_Kelompok",
        "Rata_Rata_Kelompok", "Koefisien_Diskriminan")]
      if (!length(tabs)) return(p("Parameter model belum tersedia."))
      names(tabs) <- gsub("_", " ", names(tabs))
      lapply(seq_along(tabs), function(i) {
        local({
          j <- i
          output[[paste0("da_param_", j)]] <- renderDT(ara_output_table(tabs[[j]]))
        })
      })
      mod_multivariate_section_accordion(
        id = session$ns("da_param_acc"),
        items = tabs,
        prefix = session$ns("da_param"),
        empty_message = "Parameter model belum tersedia."
      )
    })
    output$da_syntax <- renderText({
      req(input$da_group, input$da_predictors)
      f <- paste0("`", input$da_group, "` ~ ",
        paste(sprintf("`%s`", input$da_predictors), collapse = " + "))
      paste0("MASS::", input$da_method, "(", f, ", data = data, CV = ",
        ifelse(isTRUE(input$da_cv), "TRUE", "FALSE"), ")")
    })
    output$cl_syntax <- renderText({
      req(input$cl_vars)
      vars <- paste(sprintf("`%s`", input$cl_vars), collapse = ", ")
      preprocess <- paste0("x <- data[, c(", vars, "), drop = FALSE]\n",
        "x <- na.omit(x)\n", if (isTRUE(input$cl_scale)) "x <- scale(x)\n" else "")
      procedure <- switch(input$cl_method,
        kmeans = paste0("stats::kmeans(x, centers = ", input$cl_k, ", nstart = 25)"),
        pam = paste0("cluster::pam(x, k = ", input$cl_k, ")"),
        hierarchical = paste0("stats::cutree(stats::hclust(stats::dist(x, method = '",
          input$cl_distance, "'), method = '", input$cl_linkage,
          "'), k = ", input$cl_k, ")"))
      sampling <- if (input$cl_method %in% c("pam", "hierarchical") && isTRUE(input$cl_sample_large))
        paste0("set.seed(2026)\nif (nrow(x) > ", input$cl_max_cases %||% 2000,
               ") x <- x[sort(sample.int(nrow(x), ", input$cl_max_cases %||% 2000, ")), , drop = FALSE]\n") else ""
      paste0(preprocess, sampling, procedure,
             "\n# Untuk K-Means besar, silhouette dihitung pada sampel hingga 1200 kasus.")
    })

    output$da_plot <- renderPlot({
      x <- da_res(); req(x)
      if (is.null(x$scores) || !is.data.frame(x$scores)) {
        plot.new(); text(.5, .5, "Skor kanonik hanya tersedia untuk metode LDA; QDA tidak memiliki sumbu diskriminan linier.", cex = .9)
      } else {
        ld <- grep("^LD[0-9]+$", names(x$scores), value = TRUE)
        if (!length(ld)) {
          plot.new(); text(.5, .5, "Skor diskriminan tidak tersedia.")
        } else {
          scores <- x$scores
          grp <- factor(scores$Group)
          if (length(ld) == 1L) {
            # Dua kategori hanya menghasilkan satu fungsi LD1, bukan dua sumbu.
            boxplot(scores[[ld[1]]] ~ grp, col = "grey85", border = "grey30",
                    xlab = "Kelompok", ylab = "Skor LD1",
                    main = "Distribusi skor diskriminan LD1 (dua kelompok)", outline = FALSE)
            abline(h = 0, col = "grey60", lty = 3)
          } else {
            set.seed(2026)
            idx <- if (nrow(scores) > 5000L) sort(sample.int(nrow(scores), 5000L)) else seq_len(nrow(scores))
            grp_show <- droplevels(grp[idx])
            plot(scores[[ld[1]]][idx], scores[[ld[2]]][idx],
                 col = as.integer(grp_show), pch = 16, cex = .45,
                 xlab = ld[1], ylab = ld[2], main = "Canonical Discriminant Scores")
            legend("topright", legend = levels(grp_show),
                   col = seq_along(levels(grp_show)), pch = 16, bty = "n")
          }
        }
      }
    })

    observeEvent(input$run_cluster, {
      if (!try_acquire_analysis(shared, "Cluster Analysis")) return()

      req(shared$data, input$cl_vars)

      dat_bg <- shared$data
      vars_bg <- input$cl_vars
      method_bg <- input$cl_method
      k_bg <- input$cl_k
      scale_bg <- isTRUE(input$cl_scale)
      distance_bg <- input$cl_distance
      linkage_bg <- input$cl_linkage
      sample_large_bg <- isTRUE(input$cl_sample_large)
      max_cases_bg <- input$cl_max_cases %||% 2000L

      shinyjs::disable("run_cluster")
      background_start_notification("Cluster Analysis")

      p <- promises::future_promise({
        fit_cluster_model(
          dat_bg, vars_bg, method_bg, k_bg,
          scale_bg, distance_bg, linkage_bg,
          sample_large = sample_large_bg, max_cases = max_cases_bg
        )
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          cl_res(fit)
          shinyjs::enable("run_cluster")
          release_analysis(shared)
          background_success_notification("Cluster Analysis")
          NULL
        },
        onRejected = function(err) {
          cl_res(NULL)
          shinyjs::enable("run_cluster")
          release_analysis(shared)
          background_error_notification("Cluster Analysis", err)
          NULL
        }
      )
    })

    output$cl_metrics <- renderUI({
      x <- cl_res()
      if (is.null(x)) return(card(div(class="analysis-placeholder", h4("No cluster analysis has been run"))))
      tagList(
        div(class = if (isTRUE(x$sampled)) "warning-box" else "success-box", x$sample_note),
        div(class="metric-row",
        div(class="metric-box", div(class="metric-value", x$k), div(class="metric-label","Clusters")),
        div(class="metric-box", div(class="metric-value", format(nrow(x$data), big.mark=",")), div(class="metric-label", if (isTRUE(x$sampled)) "Kasus (sampel)" else "Kasus")),
        div(class="metric-box", div(class="metric-value", ifelse(is.na(x$average_silhouette),"—",sprintf("%.2f",x$average_silhouette))), div(class="metric-label","Avg. Silhouette"))
        )
      )
    })

    output$cl_sizes <- renderDT({
      x <- cl_res(); req(x)
      ara_datatable(x$sizes, rownames=FALSE, options=list(dom="t"))
    })

    output$cl_centers <- renderDT({
      x <- cl_res(); req(x)
      ctab <- x$centers
      if (is.matrix(ctab)) ctab <- as.data.frame(ctab)
      ara_datatable(ctab, rownames=TRUE, options=list(scrollX=TRUE, pageLength=10))
    })

    output$cl_plot <- renderPlot({
      x <- cl_res(); req(x)
      if (ncol(x$scaled) < 2) {
        plot.new(); text(.5,.5,"At least two variables are required for a 2D cluster plot.")
      } else {
        set.seed(2026)
        idx <- if (nrow(x$scaled) > 5000L) sort(sample.int(nrow(x$scaled), 5000L)) else seq_len(nrow(x$scaled))
        plot(x$scaled[idx,1], x$scaled[idx,2],
             pch=as.integer(factor(x$cluster[idx])),
             xlab=colnames(x$scaled)[1],
             ylab=colnames(x$scaled)[2],
             main=paste("Cluster Solution:", x$method))
      }
    })

    output$cl_dend <- renderPlot({
      x <- cl_res(); req(x)
      if (x$method != "hierarchical") {
        plot.new(); text(.5,.5,"Dendrogram is available for hierarchical clustering.")
      } else {
        plot(x$fit, labels=FALSE, main="Hierarchical Dendrogram", xlab="", sub="")
        rect.hclust(x$fit, k=x$k)
      }
    })

    output$download_mr_excel <- downloadHandler(
      filename = function() paste0("Multivariate_Regression_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- mr_res(); req(x)
        export_analysis_excel(prepend_analysis_info(multivar_reg_excel_tables(x), "Multivariate Regression", shared$provenance), file)
      }
    )

    output$download_da_excel <- downloadHandler(
      filename = function() paste0("Discriminant_Analysis_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- da_res(); req(x)
        export_analysis_excel(prepend_analysis_info(discriminant_excel_tables(x), "Discriminant Analysis", shared$provenance), file)
      }
    )

    output$download_cluster_excel <- downloadHandler(
      filename = function() paste0("Cluster_Analysis_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- cl_res(); req(x)
        export_analysis_excel(prepend_analysis_info(cluster_excel_tables(x), "Cluster Analysis", shared$provenance), file)
      }
    )
  })
}
