
sem_safe_construct_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) x <- "Konstruk"
  if (grepl("^[0-9]", x)) x <- paste0("F_", x)
  x
}

sem_indicator_group_key <- function(v) {
  # AN-style variables: GD.1.1 -> GD.1 ; GE.3.4 -> GE.3
  if (grepl("^[A-Za-z]+\\.[0-9]+\\.[0-9]+", v)) {
    return(sub("^([A-Za-z]+\\.[0-9]+)\\..*$", "\\1", v))
  }

  # DCR_1 / AIS_2 / item_01 -> DCR / AIS / item
  if (grepl("^.+[_\\.-][0-9]+$", v)) {
    return(sub("([_\\.-])[0-9]+$", "", v))
  }

  # A1 / A2 / B1 / B2 -> A / B
  if (grepl("^[A-Za-z]+[0-9]+$", v)) {
    return(sub("[0-9]+$", "", v))
  }

  NA_character_
}

infer_sem_syntax_from_data <- function(data, max_constructs = 3, max_items = 5) {
  if (is.null(data) || !ncol(data)) {
    return(list(
      cfa = "",
      sem = "",
      constructs = list(),
      note = "Belum ada dataset aktif."
    ))
  }

  vars <- names(data)
  numeric_vars <- vars[vapply(data, is.numeric, logical(1))]

  # Exclude obvious IDs, codes, weights, and administrative variables from indicator candidates.
  numeric_vars <- numeric_vars[
    !grepl(
      "^(kd_|id$|id_|no_|npsn|weight|w_fst|senwt|repwt|pv[0-9]|tahun$)",
      numeric_vars,
      ignore.case = TRUE
    )
  ]

  if (length(numeric_vars) < 3) {
    return(list(
      cfa = "",
      sem = "",
      constructs = list(),
      note = "Dataset aktif belum memiliki cukup variabel numerik untuk membentuk contoh CFA."
    ))
  }

  keys <- vapply(numeric_vars, sem_indicator_group_key, character(1))
  grouped <- split(numeric_vars[!is.na(keys)], keys[!is.na(keys)])
  grouped <- grouped[vapply(grouped, length, integer(1)) >= 3]

  # Prefer meaningful repeated-prefix groups.
  if (length(grouped)) {
    grouped <- grouped[order(vapply(grouped, length, integer(1)), decreasing = TRUE)]
    grouped <- grouped[seq_len(min(length(grouped), max_constructs))]
    grouped <- lapply(grouped, function(x) head(x, max_items))
  }

  # Fallback: create 1–2 example constructs from numeric variables.
  if (!length(grouped)) {
    if (length(numeric_vars) >= 6) {
      grouped <- list(
        Konstruk_1 = numeric_vars[1:3],
        Konstruk_2 = numeric_vars[4:6]
      )
    } else {
      grouped <- list(Konstruk_1 = head(numeric_vars, min(5, length(numeric_vars))))
    }
  }

  # Ensure unique lavaan-safe construct names.
  c_names <- make.unique(vapply(names(grouped), sem_safe_construct_name, character(1)))
  names(grouped) <- c_names

  measurement_lines <- vapply(
    names(grouped),
    function(k) paste0(k, " =~ ", paste(grouped[[k]], collapse = " + ")),
    character(1)
  )

  cfa_syntax <- paste(measurement_lines, collapse = "\n")

  sem_lines <- measurement_lines
  if (length(grouped) >= 2) {
    # Simple demonstrative structural path; user can edit based on theory.
    sem_lines <- c(
      measurement_lines,
      paste0(names(grouped)[2], " ~ ", names(grouped)[1])
    )
    if (length(grouped) >= 3) {
      sem_lines <- c(
        sem_lines,
        paste0(names(grouped)[3], " ~ ", names(grouped)[1], " + ", names(grouped)[2])
      )
    }
  }

  list(
    cfa = cfa_syntax,
    sem = paste(sem_lines, collapse = "\n"),
    constructs = grouped,
    note = paste0(
      "Contoh dibuat otomatis dari ", length(grouped),
      " kelompok indikator pada dataset aktif. ",
      "Gunakan sebagai titik awal dan sesuaikan dengan konstruk serta hipotesis penelitian."
    )
  )
}

mod_sem_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "page-heading",
      tags$span(class = "eyebrow", "STRUCTURAL EQUATION MODELING"),
      h2("SEM / CFA"),
      p("CFA, structural SEM, path diagrams, multi-group SEM, and measurement invariance.")
    ),

    usage_guide(
      "Cara menggunakan SEM / CFA",
      c(
        "Mulai dari CFA untuk memeriksa measurement model.",
        "Masukkan model menggunakan syntax lavaan, misalnya Konstruk =~ item1 + item2 + item3.",
        "Pilih estimator sesuai skala data; untuk indikator ordinal gunakan WLSMV dan tandai indikator ordinal.",
        "Periksa loading terstandar, CFI, TLI, RMSEA, SRMR, dan modification indices.",
        "Setelah measurement model memadai, lanjutkan ke Structural SEM.",
        "Untuk perbandingan kelompok, jalankan Measurement Invariance sebelum membandingkan parameter struktural."
      ),
      "Pada WLSMV, aplikasi otomatis menghindari kombinasi FIML yang tidak sesuai dan menggunakan penanganan missing yang kompatibel."
    ),

    navset_card_tab(
      nav_panel(
        "CFA",
        quick_start_card(ns, "SEM / CFA: Mulai Cepat", "Contoh data Indonesia dengan item DCR dan AIS untuk latihan CFA, SEM, serta measurement model.", "Sumber contoh: subset ringan dari dataset Indonesia yang Anda berikan."),

    layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pengaturan CFA"),
            uiOutput(ns("data_state")),
            div(
              class = "sem-syntax-example",
              div(
                class = "sem-syntax-example-head",
                strong("Contoh Sintaks dari Dataset Aktif"),
                span("Dibuat otomatis berdasarkan nama variabel yang sedang dimuat.")
              ),
              verbatimTextOutput(ns("cfa_example_syntax")),
              div(
                class = "sem-syntax-actions",
                actionButton(ns("use_cfa_example"), "Gunakan Sintaks CFA", class = "btn btn-outline-primary btn-sm")
              ),
              uiOutput(ns("sem_example_note"))
            ),
            textAreaInput(
              ns("cfa_syntax"), "Sintaks model", rows = 12,
              placeholder = "Motivation =~ M1 + M2 + M3\nSupport =~ S1 + S2 + S3"
            ),
            selectInput(ns("cfa_estimator"), "Estimator", choices = c("MLR", "ML", "WLSMV"), selected = "MLR"),
            selectInput(ns("cfa_missing"), "Penanganan missing data", choices = c("FIML" = "fiml", "Listwise" = "listwise"), selected = "fiml"),
            selectizeInput(ns("cfa_ordered"), "Ordinal indicators (WLSMV)", choices = NULL, multiple = TRUE),
            div(
              class = "diagram-control-box",
              strong("Tampilan Diagram"),
              div(class = "diagram-control-note", "Label loading dibuat lebih ringkas, garis lebih ramping, dan tata letak horizontal/vertikal dioptimalkan agar lebih mudah dibaca."),
              div(class = "diagram-control-note", "Label loading dibuat lebih ringkas, garis lebih ramping, dan tata letak horizontal/vertikal dioptimalkan agar lebih mudah dibaca."),
              layout_columns(
                col_widths = c(6, 6),
                selectInput(
                  ns("cfa_orientation"), "Orientasi",
                  choices = c("Vertikal" = "vertical", "Horizontal" = "horizontal"),
                  selected = "vertical"
                ),
                selectInput(
                  ns("cfa_diagram_style"), "Gaya",
                  choices = c(
                    "Monokrom — AMOS" = "amos",
                    "Biru-Abu — SmartPLS" = "smartpls"
                  ),
                  selected = "amos"
                )
              ),
              selectInput(
                ns("cfa_line_style"), "Garis",
                choices = c("Tipis solid" = "solid", "Putus-putus" = "dashed"),
                selected = "solid"
              )
            ),
            actionButton(ns("run_cfa"), "Jalankan CFA", class = "btn btn-primary w-100"),
            div(
              class = "fast-analysis-note",
              "Mode cepat: hasil inti ditampilkan terlebih dahulu. Modification Indices dan parameter lengkap dihitung hanya bila diperlukan."
            ),
            actionButton(
              ns("run_cfa_diagnostics"),
              "Hitung Diagnostik CFA",
              class = "btn btn-outline-secondary w-100",
              disabled = "disabled"
            ),
            br(), br(),
            downloadButton(ns("download_cfa_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          div(
            uiOutput(ns("cfa_metrics")),
            uiOutput(ns("cfa_notice")),
            navset_card_tab(
              nav_panel("Kecocokan Model", uiOutput(ns("cfa_fit_ui"))),
              nav_panel("Muatan Faktor", uiOutput(ns("cfa_loadings_ui"))),
              nav_panel("Diagram Jalur", plotOutput(ns("cfa_path"), height = "560px")),
              nav_panel("Modification Indices", uiOutput(ns("cfa_mi_ui"))),
              nav_panel("R² Indikator", uiOutput(ns("cfa_r2_ui"))),
              nav_panel("Ringkasan Model", DTOutput(ns("cfa_summary"))),
              nav_panel("Sintaks R", verbatimTextOutput(ns("cfa_syntax_view")))
            )
          )
        )
      ),

      nav_panel(
        "Structural SEM",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pengaturan SEM"),
            div(
              class = "sem-syntax-example",
              div(
                class = "sem-syntax-example-head",
                strong("Contoh Sintaks dari Dataset Aktif"),
                span("Measurement model dan jalur struktural awal dibuat otomatis.")
              ),
              verbatimTextOutput(ns("sem_example_syntax")),
              div(
                class = "sem-syntax-actions",
                actionButton(ns("use_sem_example"), "Gunakan Sintaks SEM", class = "btn btn-outline-primary btn-sm")
              )
            ),
            textAreaInput(
              ns("sem_syntax"), "Sintaks model", rows = 14,
              placeholder = "Motivation =~ M1 + M2 + M3\nEngagement =~ E1 + E2 + E3\nEngagement ~ Motivation\nAchievement ~ Engagement + Motivation"
            ),
            selectInput(ns("sem_estimator"), "Estimator", choices = c("MLR", "ML", "WLSMV"), selected = "MLR"),
            selectizeInput(ns("sem_ordered"), "Ordinal indicators (WLSMV)", choices = NULL, multiple = TRUE),
            numericInput(ns("bootstrap"), "Jumlah bootstrap (khusus ML)", value = 0, min = 0, step = 500),
            div(
              class = "diagram-control-box",
              strong("Tampilan Diagram"),
              layout_columns(
                col_widths = c(6, 6),
                selectInput(
                  ns("sem_orientation"), "Orientasi",
                  choices = c("Vertikal" = "vertical", "Horizontal" = "horizontal"),
                  selected = "vertical"
                ),
                selectInput(
                  ns("sem_diagram_style"), "Gaya",
                  choices = c(
                    "Monokrom — AMOS" = "amos",
                    "Biru-Abu — SmartPLS" = "smartpls"
                  ),
                  selected = "amos"
                )
              ),
              selectInput(
                ns("sem_line_style"), "Garis",
                choices = c("Tipis solid" = "solid", "Putus-putus" = "dashed"),
                selected = "solid"
              )
            ),
            actionButton(ns("run_sem"), "Jalankan SEM", class = "btn btn-primary w-100"),
            div(
              class = "fast-analysis-note",
              "Mode cepat: fit indices, jalur, loading, dan R² ditampilkan lebih dulu. Diagnostik berat dihitung terpisah."
            ),
            actionButton(
              ns("run_sem_diagnostics"),
              "Hitung Diagnostik SEM",
              class = "btn btn-outline-secondary w-100",
              disabled = "disabled"
            ),
            br(), br(),
            downloadButton(ns("download_sem_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          div(
            uiOutput(ns("sem_metrics")),
            uiOutput(ns("sem_notice")),
            navset_card_tab(
              nav_panel("Kecocokan Model", uiOutput(ns("sem_fit_ui"))),
              nav_panel("Jalur Struktural", uiOutput(ns("sem_paths_ui"))),
              nav_panel("Muatan Faktor", uiOutput(ns("sem_loadings_ui"))),
              nav_panel("R² Endogen", uiOutput(ns("sem_r2_ui"))),
              nav_panel("Diagram Jalur", plotOutput(ns("sem_path"), height = "600px")),
              nav_panel("Modification Indices", uiOutput(ns("sem_mi_ui"))),
              nav_panel("Ringkasan Model", DTOutput(ns("sem_summary"))),
              nav_panel("Sintaks R", verbatimTextOutput(ns("sem_syntax_view")))
            )
          )
        )
      ),

      nav_panel(
        "Multi-Group SEM",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pengaturan Multi-Group"),
            div(
              class = "sem-syntax-example compact",
              strong("Measurement model dari dataset aktif"),
              verbatimTextOutput(ns("mg_example_syntax")),
              actionButton(ns("use_mg_example"), "Gunakan Contoh", class = "btn btn-outline-primary btn-sm")
            ),
            textAreaInput(ns("mg_syntax"), "Sintaks model", rows = 12),
            selectInput(ns("mg_group"), "Variabel Kelompok", choices = NULL),
            radioButtons(
              ns("mg_mode"),
              "Mode Multi-Group",
              choices = c(
                "Cepat — ML covariance (disarankan)" = "fast",
                "Robust — data mentah / MLR" = "robust"
              ),
              selected = "fast"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'robust'", ns("mg_mode")),
              selectInput(ns("mg_estimator"), "Estimator robust", choices = c("MLR", "ML", "WLSMV"), selected = "MLR")
            ),
            uiOutput(ns("mg_group_preview")),
            selectizeInput(ns("mg_ordered"), "Ordinal indicators (WLSMV)", choices = NULL, multiple = TRUE),
            actionButton(ns("run_mg"), "Jalankan Multi-Group SEM", class = "btn btn-primary w-100"),
            div(
              class = "fast-analysis-note",
              "Mode cepat aktif: aplikasi memprioritaskan fit model dan parameter utama agar hasil muncul lebih cepat."
            ),
            br(), br(),
            downloadButton(ns("download_mg_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          navset_card_tab(
            nav_panel("Kecocokan Model", DTOutput(ns("mg_fit"))),
            nav_panel("Parameters", DTOutput(ns("mg_params"))),
            nav_panel("Ukuran Kelompok", DTOutput(ns("mg_group_counts"))),
            nav_panel("Ringkasan", DTOutput(ns("mg_summary"))),
            nav_panel("Sintaks R", verbatimTextOutput(ns("mg_syntax_view")))
          )
        )
      ),

      nav_panel(
        "Measurement Invariance",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Pengaturan Measurement Invariance"),
            div(
              class = "sem-syntax-example compact",
              strong("Measurement model dari dataset aktif"),
              verbatimTextOutput(ns("inv_example_syntax")),
              actionButton(ns("use_inv_example"), "Gunakan Contoh", class = "btn btn-outline-primary btn-sm")
            ),
            textAreaInput(ns("inv_syntax"), "Sintaks model CFA", rows = 12),
            selectInput(ns("inv_group"), "Variabel Kelompok", choices = NULL),
            radioButtons(
              ns("inv_mode"),
              "Mode Measurement Invariance",
              choices = c(
                "Cepat — ML covariance (disarankan)" = "fast",
                "Robust — data mentah / MLR/WLSMV" = "robust"
              ),
              selected = "fast"
            ),
            radioButtons(
              ns("inv_level"),
              "Tahapan",
              choices = c(
                "Standar — Configural + Metric + Scalar" = "standard",
                "Lengkap — tambah Strict" = "full"
              ),
              selected = "standard"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'robust'", ns("inv_mode")),
              selectInput(ns("inv_estimator"), "Estimator robust", choices = c("MLR", "ML", "WLSMV"), selected = "MLR"),
              selectizeInput(ns("inv_ordered"), "Ordinal indicators (WLSMV)", choices = NULL, multiple = TRUE)
            ),
            uiOutput(ns("inv_group_preview")),
            actionButton(ns("run_inv"), "Jalankan Measurement Invariance", class = "btn btn-primary w-100"),
            div(
              class = "fast-analysis-note",
              "Mode cepat memakai covariance matrix per kelompok. Tahapan Standar menjalankan Configural, Metric, dan Scalar terlebih dahulu agar hasil lebih cepat."
            ),
            br(), br(),
            downloadButton(ns("download_inv_excel"), "Ekspor Semua Tabel ke Excel", class = "btn btn-outline-secondary w-100")
          ),
          navset_card_tab(
            nav_panel("Tabel Perbandingan", DTOutput(ns("inv_table"))),
            nav_panel("Jumlah per Kelompok", DTOutput(ns("inv_group_counts"))),
            nav_panel("Interpretasi", verbatimTextOutput(ns("inv_text"))),
            nav_panel("Sintaks R", verbatimTextOutput(ns("inv_syntax_view")))
          )
        )
      )
    )
  )
}

mod_sem_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    sem_value_cell <- function(x, column_name = NULL) {
      if (length(x) == 0 || is.na(x)) return(tags$span("—"))
      if (is.numeric(x)) {
        if (!is.null(column_name) &&
            (column_name %in% analysis_p_columns() ||
             grepl("(^p$|p[_\\.]?value|Pr\\()", column_name, ignore.case = TRUE))) {
          return(format_p_2(x))
        }
        if (!is.null(column_name) && column_name %in% analysis_count_columns()) {
          if (abs(x - round(x)) < 1e-10) return(format(round(x), trim = TRUE))
        }
        return(sprintf("%.4f", x))
      }
      as.character(x)
    }



    sem_palette <- function(style = "amos") {
      if (identical(style, "smartpls")) {
        list(
          latent_fill = "#E8EEF7",
          manifest_fill = "#FFFFFF",
          border = "#475569",
          edge = "#64748B",
          text = "#1F2937",
          label_bg = "#FFFFFF"
        )
      } else {
        list(
          latent_fill = "#F3F4F6",
          manifest_fill = "#FFFFFF",
          border = "#374151",
          edge = "#4B5563",
          text = "#111827",
          label_bg = "#FFFFFF"
        )
      }
    }

    draw_ellipse <- function(x, y, rx, ry, fill, border, lwd = 1.1) {
      theta <- seq(0, 2*pi, length.out = 120)
      polygon(
        x + rx*cos(theta),
        y + ry*sin(theta),
        col = fill,
        border = border,
        lwd = lwd
      )
    }

    draw_box_node <- function(x, y, w, h, label, fill, border, text_col, cex = .72) {
      rect(
        x - w/2, y - h/2,
        x + w/2, y + h/2,
        col = fill, border = border, lwd = 1.0
      )
      text(x, y, labels = label, cex = cex, col = text_col)
    }

    draw_loading_edge <- function(x0, y0, x1, y1, value,
                                  col, lty = 1, label_bg = "#FFFFFF",
                                  label_cex = .68, label_pos = .58,
                                  perp_offset = 0, line_lwd = .92,
                                  arrow_length = .055) {
      arrows(
        x0, y0, x1, y1,
        length = arrow_length,
        angle = 20,
        code = 2,
        col = col,
        lwd = line_lwd,
        lty = lty
      )

      dx <- x1 - x0
      dy <- y1 - y0
      seg_len <- sqrt(dx^2 + dy^2)
      if (!is.finite(seg_len) || seg_len == 0) seg_len <- 1
      nx <- -dy / seg_len
      ny <- dx / seg_len

      lx <- x0 + label_pos * dx + perp_offset * nx
      ly <- y0 + label_pos * dy + perp_offset * ny
      lab <- sprintf("%.4f", value)

      sw <- strwidth(lab, cex = label_cex)
      sh <- strheight(lab, cex = label_cex)
      rect(
        lx - sw*.66, ly - sh*.70,
        lx + sw*.66, ly + sh*.70,
        col = label_bg, border = NA
      )
      text(lx, ly, labels = lab, cex = label_cex, col = col)
    }

    draw_structural_edge <- function(x0, y0, x1, y1, value,
                                     col, lty = 1, label_bg = "#FFFFFF",
                                     label_pos = .50, perp_offset = .03,
                                     line_lwd = 1.05) {
      arrows(
        x0, y0, x1, y1,
        length = .06, angle = 20,
        code = 2, col = col,
        lwd = line_lwd, lty = lty
      )
      dx <- x1 - x0
      dy <- y1 - y0
      seg_len <- sqrt(dx^2 + dy^2)
      if (!is.finite(seg_len) || seg_len == 0) seg_len <- 1
      nx <- -dy / seg_len
      ny <- dx / seg_len
      lx <- x0 + label_pos * dx + perp_offset * nx
      ly <- y0 + label_pos * dy + perp_offset * ny
      lab <- sprintf("%.4f", value)
      sw <- strwidth(lab, cex = .72)
      sh <- strheight(lab, cex = .72)
      rect(lx-sw*.72, ly-sh*.72, lx+sw*.72, ly+sh*.72, col=label_bg, border=NA)
      text(lx, ly, lab, cex=.72, font=2, col=col)
    }

    plot_sem_from_tables <- function(result,
                                     orientation = "vertical",
                                     style = "amos",
                                     line_style = "solid") {
      loadings <- result$loadings
      if (is.null(loadings) || !nrow(loadings)) {
        plot.new()
        text(.5, .5, "Muatan faktor tidak tersedia untuk membuat diagram.")
        return(invisible(NULL))
      }

      value_col <- if ("std.all" %in% names(loadings)) "std.all" else "est"
      loadings[[value_col]] <- as.numeric(loadings[[value_col]])
      loadings <- loadings[is.finite(loadings[[value_col]]), , drop = FALSE]

      constructs <- unique(as.character(loadings$lhs))
      groups <- split(loadings, factor(loadings$lhs, levels = constructs))
      pal <- sem_palette(style)
      lty <- if (identical(line_style, "dashed")) 2 else 1

      op <- par(no.readonly = TRUE)
      on.exit(par(op), add = TRUE)
      par(mar = c(2.5, 2.5, 2.5, 2.5), xpd = NA)
      plot.new()
      plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = NA)

      latent_pos <- list()

      if (identical(orientation, "horizontal")) {
        y_lat <- seq(.82, .18, length.out = length(constructs))
        x_lat <- .15
        x_item <- .84

        all_item_pos <- list()

        for (g in seq_along(constructs)) {
          con <- constructs[g]
          d <- groups[[con]]
          n <- nrow(d)
          center <- y_lat[g]
          span <- min(.25, .065 * max(n-1, 1))
          ys <- if (n == 1) center else seq(center + span, center - span, length.out = n)

          latent_pos[[con]] <- c(x_lat, center)

          draw_ellipse(
            x_lat, center, .050, .055,
            pal$latent_fill, pal$border
          )
          text(x_lat, center, con, cex=.88, col=pal$text)

          for (i in seq_len(n)) {
            item <- as.character(d$rhs[i])
            iy <- ys[i]
            all_item_pos[[paste(con, item, sep="::")]] <- c(x_item, iy)

            draw_box_node(
              x_item, iy,
              w=.128, h=.052,
              label=item,
              fill=pal$manifest_fill,
              border=pal$border,
              text_col=pal$text,
              cex=.64
            )

            # stop line before box border; give coefficient more space
            draw_loading_edge(
              x_lat+.056, center,
              x_item-.076, iy,
              d[[value_col]][i],
              col=pal$edge, lty=lty,
              label_bg=pal$label_bg,
              label_cex=.66,
              label_pos=.53 + ((i - 1) %% 3) * .05,
              perp_offset=c(-.016, .016, -.010, .010, 0)[((i - 1) %% 5) + 1],
              line_lwd=.90,
              arrow_length=.050
            )
          }
        }

      } else {
        x_lat <- seq(.16, .84, length.out = length(constructs))
        y_lat <- .82
        y_item <- .20

        for (g in seq_along(constructs)) {
          con <- constructs[g]
          d <- groups[[con]]
          n <- nrow(d)
          center <- x_lat[g]

          # each construct owns a local horizontal band; this prevents loading labels colliding
          band <- if (length(constructs) <= 1) .70 else min(.28, .78/length(constructs))
          xs <- if (n == 1) center else seq(center-band/2, center+band/2, length.out=n)

          latent_pos[[con]] <- c(center, y_lat)

          draw_ellipse(
            center, y_lat, .047, .060,
            pal$latent_fill, pal$border
          )
          text(center, y_lat, con, cex=.88, col=pal$text)

          for (i in seq_len(n)) {
            item <- as.character(d$rhs[i])
            ix <- xs[i]

            draw_box_node(
              ix, y_item,
              w=max(.072, .012*nchar(item)),
              h=.058,
              label=item,
              fill=pal$manifest_fill,
              border=pal$border,
              text_col=pal$text,
              cex=.66
            )

            # stagger label position vertically to avoid overlap
            label_pos <- .48 + ((i - 1) %% 3) * .05
            perp_shift <- c(-.014, .014, -.008, .008, 0)[((i - 1) %% 5) + 1]

            draw_loading_edge(
              center, y_lat-.064,
              ix, y_item+.036,
              d[[value_col]][i],
              col=pal$edge, lty=lty,
              label_bg=pal$label_bg,
              label_cex=.64,
              label_pos=label_pos,
              perp_offset=perp_shift,
              line_lwd=.90,
              arrow_length=.050
            )
          }
        }
      }

      # Add structural paths for SEM, if available
      if (!is.null(result$paths) && nrow(result$paths)) {
        p <- result$paths
        p <- p[p$op == "~", , drop=FALSE]
        if (nrow(p)) {
          path_val <- if ("std.all" %in% names(p)) "std.all" else "est"

          for (i in seq_len(nrow(p))) {
            outcome <- as.character(p$lhs[i])
            predictor <- as.character(p$rhs[i])
            if (!(outcome %in% names(latent_pos)) || !(predictor %in% names(latent_pos))) next

            a <- latent_pos[[predictor]]
            b <- latent_pos[[outcome]]
            if (identical(orientation, "horizontal")) {
              # offset slightly so structural arrows are visible beside measurement arrows
              draw_structural_edge(
                a[1], a[2]-.072,
                b[1], b[2]+.072,
                as.numeric(p[[path_val]][i]),
                pal$edge, lty, pal$label_bg,
                label_pos=.44,
                perp_offset=.028,
                line_lwd=.98
              )
            } else {
              draw_structural_edge(
                a[1]+.060, a[2],
                b[1]-.060, b[2],
                as.numeric(p[[path_val]][i]),
                pal$edge, lty, pal$label_bg,
                label_pos=.50,
                perp_offset=.022,
                line_lwd=.98
              )
            }
          }
        }
      }

      invisible(NULL)
    }

    sem_html_table <- function(df, empty_message = "Tidak ada hasil yang dapat ditampilkan.", max_rows = NULL) {
      if (is.null(df) || !is.data.frame(df) || !nrow(df) || !ncol(df))
        return(div(class = "sem-empty-state", empty_message))
      labels <- c(lhs = "Konstruk / Outcome", op = "Operator",
        rhs = "Indikator / Prediktor", est = "Estimate", se = "SE", z = "z",
        pvalue = "p", `ci.lower` = "CI Lower", `ci.upper` = "CI Upper",
        `std.lv` = "Std. LV", `std.all` = "Std. All", mi = "MI",
        epc = "EPC", `sepc.lv` = "Std. EPC (LV)",
        `sepc.all` = "Std. EPC (All)", R2 = "R²")
      names(df) <- vapply(names(df), function(nm)
        if (nm %in% names(labels)) labels[[nm]] else nm, character(1))
      ara_html_result_table(df, empty_message = empty_message,
        max_rows = if (is.null(max_rows)) 50L else max_rows)
    }

    quick_start_server(
      input, output, session, shared,
      example_path = "data/examples/demo_sem_multivariate_idn.csv", example_name = "Contoh SEM/CFA — Data Indonesia",
      source = "Dataset latihan Indonesia", notes = "Subset latihan CFA/SEM.",
      template_path = "data/templates/sem_template.csv"
    )


    cfa_result <- reactiveVal(NULL)
    observeEvent(input$load_example, {
      syntax <- paste0(
        "DCR =~ DCR_1 + DCR_2 + DCR_3 + DCR_4 + DCR_5\n",
        "AIS =~ AIS_1 + AIS_2 + AIS_3 + AIS_4 + AIS_5"
      )
      updateTextAreaInput(session, "cfa_syntax", value=syntax)
      updateTextAreaInput(session, "sem_syntax", value=paste0(syntax, "\nAIS ~ DCR"))
    })

    sem_result <- reactiveVal(NULL)
    mg_result <- reactiveVal(NULL)
    inv_result <- reactiveVal(NULL)

    sem_examples <- reactive({
      req(shared$data)
      infer_sem_syntax_from_data(shared$data)
    })

    output$cfa_example_syntax <- renderText({
      x <- sem_examples()
      if (!nzchar(x$cfa)) x$note else x$cfa
    })

    output$sem_example_syntax <- renderText({
      x <- sem_examples()
      if (!nzchar(x$sem)) x$note else x$sem
    })

    output$mg_example_syntax <- renderText({
      x <- sem_examples()
      if (!nzchar(x$cfa)) x$note else x$cfa
    })

    output$inv_example_syntax <- renderText({
      x <- sem_examples()
      if (!nzchar(x$cfa)) x$note else x$cfa
    })

    output$sem_example_note <- renderUI({
      x <- sem_examples()
      div(class = "sem-example-note", x$note)
    })

    observeEvent(input$use_cfa_example, {
      x <- sem_examples()
      req(nzchar(x$cfa))
      updateTextAreaInput(session, "cfa_syntax", value = x$cfa)
    })

    observeEvent(input$use_sem_example, {
      x <- sem_examples()
      req(nzchar(x$sem))
      updateTextAreaInput(session, "sem_syntax", value = x$sem)
    })

    observeEvent(input$use_mg_example, {
      x <- sem_examples()
      req(nzchar(x$cfa))
      updateTextAreaInput(session, "mg_syntax", value = x$cfa)
    })

    observeEvent(input$use_inv_example, {
      x <- sem_examples()
      req(nzchar(x$cfa))
      updateTextAreaInput(session, "inv_syntax", value = x$cfa)
    })

    # Auto-fill examples whenever a new dataset is loaded.
    observeEvent(shared$mapping_token, {
      req(shared$data)
      x <- infer_sem_syntax_from_data(shared$data)

      if (nzchar(x$cfa)) {
        updateTextAreaInput(session, "cfa_syntax", value = x$cfa)
        updateTextAreaInput(session, "mg_syntax", value = x$cfa)
        updateTextAreaInput(session, "inv_syntax", value = x$cfa)
      }
      if (nzchar(x$sem)) {
        updateTextAreaInput(session, "sem_syntax", value = x$sem)
      }
    }, ignoreInit = FALSE)

    observe({
      req(shared$data)
      all_vars <- names(shared$data)
      updateSelectInput(session, "mg_group", choices = all_vars)
      updateSelectInput(session, "inv_group", choices = all_vars)
      updateSelectizeInput(session, "cfa_ordered", choices = all_vars, server = TRUE)
      updateSelectizeInput(session, "sem_ordered", choices = all_vars, server = TRUE)
      updateSelectizeInput(session, "mg_ordered", choices = all_vars, server = TRUE)
      updateSelectizeInput(session, "inv_ordered", choices = all_vars, server = TRUE)
    })

    output$data_state <- renderUI({
      if (is.null(shared$data)) {
        div(class = "warning-box", "Load a dataset first from the Data menu.")
      } else {
        div(
          class = "success-box",
          strong(shared$data_name), br(),
          paste(format(nrow(shared$data), big.mark = ","), "rows ×", ncol(shared$data), "variables")
        )
      }
    })

    observeEvent(input$run_cfa, {
      if (!try_acquire_analysis(shared, "CFA")) return()

      req(shared$data)

      dat_bg <- shared$data
      syntax_bg <- input$cfa_syntax
      estimator_bg <- input$cfa_estimator
      missing_bg <- input$cfa_missing
      ordered_bg <- input$cfa_ordered %||% character(0)

      shinyjs::disable("run_cfa")
      shinyjs::disable("run_cfa_diagnostics")
      background_start_notification("CFA")

      p <- promises::future_promise({
        fit <- fit_cfa_model(
          data = dat_bg,
          model_syntax = syntax_bg,
          estimator = estimator_bg,
          missing = missing_bg,
          ordered = ordered_bg
        )
        freeze_cfa_result_fast(fit)
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          cfa_result(fit)
          shinyjs::enable("run_cfa")
          shinyjs::enable("run_cfa_diagnostics")
          release_analysis(shared)
          background_success_notification("CFA")
          NULL
        },
        onRejected = function(err) {
          cfa_result(NULL)
          shinyjs::enable("run_cfa")
          shinyjs::disable("run_cfa_diagnostics")
          release_analysis(shared)
          background_error_notification("CFA", err)
          NULL
        }
      )
    })


    observeEvent(input$run_cfa_diagnostics, {
      x <- cfa_result()
      req(x)

      if (isTRUE(x$diagnostics_computed)) {
        showNotification("Diagnostik CFA sudah dihitung.", type = "message")
        return()
      }

      if (!try_acquire_analysis(shared, "Diagnostik CFA")) return()

      dat_bg <- shared$data
      syntax_bg <- x$syntax
      estimator_bg <- x$estimator
      missing_bg <- x$missing
      ordered_bg <- x$ordered

      shinyjs::disable("run_cfa_diagnostics")
      background_start_notification("Diagnostik CFA")

      p <- promises::future_promise({
        compute_cfa_diagnostics_from_data(
          dat_bg, syntax_bg, estimator_bg, missing_bg, ordered_bg
        )
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(diag) {
          current <- cfa_result()
          current$modification_indices <- diag$modification_indices
          current$parameters <- diag$parameters
          current$diagnostics_computed <- TRUE
          cfa_result(current)

          shinyjs::enable("run_cfa_diagnostics")
          release_analysis(shared)
          background_success_notification("Diagnostik CFA")
          NULL
        },
        onRejected = function(err) {
          shinyjs::enable("run_cfa_diagnostics")
          release_analysis(shared)
          background_error_notification("Diagnostik CFA", err)
          NULL
        }
      )
    })

    output$cfa_metrics <- renderUI({
      x <- cfa_result()
      if (is.null(x)) {
        return(card(div(class = "analysis-placeholder", h4("No CFA model has been run"))))
      }
      fm <- setNames(x$fit_measures$Value, x$fit_measures$Measure)
      div(
        class = "metric-row",
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", fm["cfi"])), div(class = "metric-label", "CFI")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", fm["tli"])), div(class = "metric-label", "TLI")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", fm["rmsea"])), div(class = "metric-label", "RMSEA"))
      )
    })

    output$cfa_notice <- renderUI({
      x <- cfa_result()
      req(x)
      if (x$estimator == "WLSMV") {
        div(class = "warning-box", "WLSMV active: missing-data handling was set to a compatible mode; ordinal variables should be explicitly selected.")
      }
    })





    output$cfa_fit_ui <- renderUI({
      x <- cfa_result(); req(x)
      sem_html_table(x$fit_measures, "Kecocokan model belum tersedia.")
    })

    output$cfa_loadings_ui <- renderUI({
      x <- cfa_result(); req(x)
      sem_html_table(
        x$loadings,
        "Muatan faktor tidak tersedia. Periksa apakah sintaks CFA menggunakan operator '=~'."
      )
    })

    output$cfa_mi_ui <- renderUI({
      x <- cfa_result(); req(x)
      sem_html_table(
        x$modification_indices,
        "Klik tombol Hitung Diagnostik CFA untuk menghitung Modification Indices.",
        max_rows = 50
      )
    })

    output$cfa_r2_ui <- renderUI({
      x <- cfa_result(); req(x)
      sem_html_table(x$r2_table, "R² indikator tidak tersedia untuk model ini.")
    })

    output$cfa_summary <- renderDT({
      x <- cfa_result(); req(x)
      ara_output_table(ara_sem_summary_table(x))
    })
    output$cfa_syntax_view <- renderText({
      x <- cfa_result()
      if (!is.null(x$syntax)) return(x$syntax)
      input$cfa_syntax %||% ""
    })
    output$cfa_path <- renderPlot({
      x <- cfa_result(); req(x)
      plot_sem_from_tables(
        result = x,
        orientation = input$cfa_orientation %||% "vertical",
        style = input$cfa_diagram_style %||% "amos",
        line_style = input$cfa_line_style %||% "solid"
      )
    })
    output$download_cfa_excel <- downloadHandler(
      filename = function() paste0("CFA_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- cfa_result(); req(x)
        export_analysis_excel(prepend_analysis_info(cfa_excel_tables(x), "CFA", shared$provenance), file)
      }
    )

    observeEvent(input$run_sem, {
      if (!try_acquire_analysis(shared, "SEM")) return()

      req(shared$data)

      dat_bg <- shared$data
      syntax_bg <- input$sem_syntax
      estimator_bg <- input$sem_estimator
      bootstrap_bg <- input$bootstrap
      ordered_bg <- input$sem_ordered %||% character(0)

      shinyjs::disable("run_sem")
      shinyjs::disable("run_sem_diagnostics")
      background_start_notification("SEM")

      p <- promises::future_promise({
        fit <- fit_sem_model(
          data = dat_bg,
          model_syntax = syntax_bg,
          estimator = estimator_bg,
          bootstrap = bootstrap_bg,
          ordered = ordered_bg
        )
        freeze_sem_result_fast(fit)
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          sem_result(fit)
          shinyjs::enable("run_sem")
          shinyjs::enable("run_sem_diagnostics")
          release_analysis(shared)
          background_success_notification("SEM")
          NULL
        },
        onRejected = function(err) {
          sem_result(NULL)
          shinyjs::enable("run_sem")
          shinyjs::disable("run_sem_diagnostics")
          release_analysis(shared)
          background_error_notification("SEM", err)
          NULL
        }
      )
    })


    observeEvent(input$run_sem_diagnostics, {
      x <- sem_result()
      req(x)

      if (isTRUE(x$diagnostics_computed)) {
        showNotification("Diagnostik SEM sudah dihitung.", type = "message")
        return()
      }

      if (!try_acquire_analysis(shared, "Diagnostik SEM")) return()

      dat_bg <- shared$data
      syntax_bg <- x$syntax
      estimator_bg <- x$estimator
      bootstrap_bg <- if (isTRUE(x$bootstrap_applied)) input$bootstrap else 0
      ordered_bg <- x$ordered

      shinyjs::disable("run_sem_diagnostics")
      background_start_notification("Diagnostik SEM")

      p <- promises::future_promise({
        compute_sem_diagnostics_from_data(
          dat_bg, syntax_bg, estimator_bg, bootstrap_bg, ordered_bg
        )
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(diag) {
          current <- sem_result()
          current$modification_indices <- diag$modification_indices
          current$parameters <- diag$parameters
          current$diagnostics_computed <- TRUE
          sem_result(current)

          shinyjs::enable("run_sem_diagnostics")
          release_analysis(shared)
          background_success_notification("Diagnostik SEM")
          NULL
        },
        onRejected = function(err) {
          shinyjs::enable("run_sem_diagnostics")
          release_analysis(shared)
          background_error_notification("Diagnostik SEM", err)
          NULL
        }
      )
    })

    output$sem_metrics <- renderUI({
      x <- sem_result()
      if (is.null(x)) {
        return(card(div(class = "analysis-placeholder", h4("No SEM model has been run"))))
      }
      fm <- setNames(x$fit_measures$Value, x$fit_measures$Measure)
      div(
        class = "metric-row",
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", fm["cfi"])), div(class = "metric-label", "CFI")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", fm["rmsea"])), div(class = "metric-label", "RMSEA")),
        div(class = "metric-box", div(class = "metric-value", sprintf("%.2f", fm["srmr"])), div(class = "metric-label", "SRMR"))
      )
    })

    output$sem_notice <- renderUI({
      x <- sem_result()
      req(x)
      if (x$estimator == "WLSMV") {
        div(class = "warning-box", "WLSMV active: bootstrap is not applied and missing-data handling follows the estimator-compatible setting.")
      } else if (input$bootstrap > 0 && !isTRUE(x$bootstrap_applied)) {
        div(class = "warning-box", "Bootstrap was requested but not applied. Bootstrap is enabled only with estimator ML.")
      }
    })






    output$sem_fit_ui <- renderUI({
      x <- sem_result(); req(x)
      sem_html_table(x$fit_measures, "Kecocokan model belum tersedia.")
    })

    output$sem_paths_ui <- renderUI({
      x <- sem_result(); req(x)
      sem_html_table(x$paths, "Tidak ada jalur struktural pada sintaks SEM.")
    })

    output$sem_loadings_ui <- renderUI({
      x <- sem_result(); req(x)
      sem_html_table(
        x$loadings,
        "Muatan faktor tidak tersedia. Pastikan measurement model menggunakan operator '=~'."
      )
    })

    output$sem_r2_ui <- renderUI({
      x <- sem_result(); req(x)
      sem_html_table(x$r2_table, "R² variabel endogen tidak tersedia.")
    })

    output$sem_mi_ui <- renderUI({
      x <- sem_result(); req(x)
      sem_html_table(
        x$modification_indices,
        "Klik tombol Hitung Diagnostik SEM untuk menghitung Modification Indices.",
        max_rows = 50
      )
    })

    output$sem_summary <- renderDT({
      x <- sem_result(); req(x)
      ara_output_table(ara_sem_summary_table(x))
    })
    output$sem_syntax_view <- renderText({
      x <- sem_result()
      if (!is.null(x$syntax)) return(x$syntax)
      input$sem_syntax %||% ""
    })
    output$sem_path <- renderPlot({
      x <- sem_result(); req(x)
      plot_sem_from_tables(
        result = x,
        orientation = input$sem_orientation %||% "vertical",
        style = input$sem_diagram_style %||% "amos",
        line_style = input$sem_line_style %||% "solid"
      )
    })
    output$download_sem_excel <- downloadHandler(
      filename = function() paste0("SEM_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- sem_result(); req(x)
        export_analysis_excel(prepend_analysis_info(sem_excel_tables(x), "SEM", shared$provenance), file)
      }
    )


    output$mg_group_preview <- renderUI({
      req(shared$data, input$mg_group)

      info <- tryCatch(
        multigroup_group_info(shared$data, input$mg_group),
        error = function(e) NULL
      )

      if (is.null(info)) return(NULL)

      cls <- if (info$n_groups > 10) "warning-box" else "sem-example-note"

      div(
        class = cls,
        strong(paste0("Jumlah kelompok: ", info$n_groups)),
        br(),
        if (info$n_groups > 10) {
          paste0(
            "Terlalu banyak untuk Multi-Group SEM. Pilih variabel kategorikal dengan maksimal 10 kelompok."
          )
        } else {
          paste0(
            "Ukuran kelompok: ",
            paste(
              paste0(
                head(info$counts$Group, 6), " (N=", head(info$counts$N, 6), ")"
              ),
              collapse = "; "
            ),
            if (nrow(info$counts) > 6) " ..." else ""
          )
        }
      )
    })

    observeEvent(input$run_mg, {
      if (!try_acquire_analysis(shared, "Multi-Group SEM")) return()

      req(shared$data, input$mg_group)

      dat_bg <- shared$data
      syntax_bg <- input$mg_syntax
      group_bg <- input$mg_group
      mode_bg <- input$mg_mode %||% "fast"
      estimator_bg <- input$mg_estimator %||% "MLR"
      ordered_bg <- input$mg_ordered %||% character(0)

      # Validate group count before allocating a background worker.
      info <- tryCatch(
        multigroup_group_info(dat_bg, group_bg),
        error = function(e) e
      )

      if (inherits(info, "error")) {
        release_analysis(shared)
        showNotification(info$message, type = "error", duration = NULL)
        return()
      }

      if (info$n_groups > 10) {
        release_analysis(shared)
        showNotification(
          paste0(
            "Multi-Group SEM dibatalkan: variabel grouping memiliki ",
            info$n_groups,
            " kelompok. Gunakan variabel kategorikal dengan maksimal 10 kelompok."
          ),
          type = "error",
          duration = NULL
        )
        return()
      }

      shinyjs::disable("run_mg")
      background_start_notification("Multi-Group SEM")

      p <- promises::future_promise({
        if (identical(mode_bg, "fast")) {
          fit <- fit_multigroup_sem_fast(
            dat_bg,
            syntax_bg,
            group_bg,
            max_groups = 10,
            min_group_n = 30
          )
        } else {
          fit <- fit_multigroup_sem(
            dat_bg,
            syntax_bg,
            group_bg,
            estimator_bg,
            ordered = ordered_bg
          )

          # Standardize structure expected by the UI
          fit$group_counts <- multigroup_group_info(dat_bg, group_bg)$counts
          fit$fast_mode <- FALSE
        }

        freeze_multigroup_result_fast(fit)
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          mg_result(fit)
          shinyjs::enable("run_mg")
          release_analysis(shared)
          background_success_notification("Multi-Group SEM")
          NULL
        },
        onRejected = function(err) {
          mg_result(NULL)
          shinyjs::enable("run_mg")
          release_analysis(shared)
          background_error_notification("Multi-Group SEM", err)
          NULL
        }
      )
    })
    output$mg_fit <- renderDT({
      x <- mg_result(); req(x)
      ara_datatable(x$fit_measures, rownames = FALSE, options = list(dom = "t"))
    })
    output$mg_params <- renderDT({
      x <- mg_result(); req(x)
      ara_datatable(x$parameters, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$mg_group_counts <- renderDT({
      x <- mg_result(); req(x)
      if (is.null(x$group_counts) || !nrow(x$group_counts)) {
        return(ara_datatable(
          data.frame(Catatan = "Informasi jumlah kelompok tidak tersedia."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      ara_datatable(
        x$group_counts,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10)
      )
    })

    output$mg_syntax_view <- renderText({
      x <- mg_result()
      if (!is.null(x$syntax)) return(x$syntax)
      input$mg_syntax %||% ""
    })
    output$inv_syntax_view <- renderText({
      input$inv_syntax %||% ""
    })

    output$mg_summary <- renderDT({
      x <- mg_result(); req(x)
      mode_txt <- if (isTRUE(x$fast_mode)) "Cepat — ML berbasis covariance matrix"
        else paste0("Robust — ", x$estimator)
      n_groups <- if (is.data.frame(x$group_counts)) nrow(x$group_counts) else NA_integer_
      info <- data.frame(
        Informasi = c("Mode estimasi", "Variabel kelompok", "Jumlah kelompok"),
        Nilai = c(mode_txt, as.character(x$group_var), as.character(n_groups)),
        stringsAsFactors = FALSE
      )
      ara_datatable(info, rownames = FALSE, options = list(dom = "t"))
    })


    output$inv_group_preview <- renderUI({
      req(shared$data, input$inv_group)

      info <- tryCatch(
        multigroup_group_info(shared$data, input$inv_group),
        error = function(e) NULL
      )

      if (is.null(info)) return(NULL)

      cls <- if (info$n_groups > 10) "warning-box" else "sem-example-note"

      div(
        class = cls,
        strong(paste0("Jumlah kelompok: ", info$n_groups)),
        br(),
        if (info$n_groups > 10) {
          "Terlalu banyak untuk Measurement Invariance. Pilih variabel kategorikal dengan maksimal 10 kelompok."
        } else {
          paste0(
            "Ukuran kelompok: ",
            paste(
              paste0(
                head(info$counts$Group, 6),
                " (N=", head(info$counts$N, 6), ")"
              ),
              collapse = "; "
            ),
            if (nrow(info$counts) > 6) " ..." else ""
          )
        }
      )
    })

    observeEvent(input$run_inv, {
      if (!try_acquire_analysis(shared, "Measurement Invariance")) return()

      req(shared$data, input$inv_group)

      dat_bg <- shared$data
      syntax_bg <- input$inv_syntax
      group_bg <- input$inv_group
      mode_bg <- input$inv_mode %||% "fast"
      level_bg <- input$inv_level %||% "standard"
      estimator_bg <- input$inv_estimator %||% "MLR"
      ordered_bg <- input$inv_ordered %||% character(0)

      info <- tryCatch(
        multigroup_group_info(dat_bg, group_bg),
        error = function(e) e
      )

      if (inherits(info, "error")) {
        release_analysis(shared)
        showNotification(info$message, type = "error", duration = NULL)
        return()
      }

      if (info$n_groups > 10) {
        release_analysis(shared)
        showNotification(
          paste0(
            "Measurement Invariance dibatalkan: grouping variable memiliki ",
            info$n_groups,
            " kelompok. Gunakan variabel kategorikal dengan maksimal 10 kelompok."
          ),
          type = "error",
          duration = NULL
        )
        return()
      }

      shinyjs::disable("run_inv")
      background_start_notification("Measurement Invariance")

      p <- promises::future_promise({
        if (identical(mode_bg, "fast")) {
          fit_measurement_invariance_fast(
            data = dat_bg,
            model_syntax = syntax_bg,
            group_var = group_bg,
            level = level_bg,
            max_groups = 10,
            min_group_n = 30
          )
        } else {
          out <- fit_measurement_invariance(
            dat_bg,
            syntax_bg,
            group_bg,
            estimator_bg,
            ordered = ordered_bg
          )

          if (identical(level_bg, "standard") &&
              "Strict" %in% out$table$Model) {
            out$table <- out$table[out$table$Model != "Strict", , drop = FALSE]
          }

          out$group_counts <- multigroup_group_info(dat_bg, group_bg)$counts
          out$fast_mode <- FALSE
          out$level <- level_bg

          # Do not return the fitted lavaan model list to the Shiny process.
          out$models <- NULL
          out
        }
      }, seed = TRUE)

      promises::then(
        p,
        onFulfilled = function(fit) {
          inv_result(fit)
          shinyjs::enable("run_inv")
          release_analysis(shared)
          background_success_notification("Measurement Invariance")
          NULL
        },
        onRejected = function(err) {
          inv_result(NULL)
          shinyjs::enable("run_inv")
          release_analysis(shared)
          background_error_notification("Measurement Invariance", err)
          NULL
        }
      )
    })
    output$inv_table <- renderDT({
      x <- inv_result(); req(x)
      ara_datatable(x$table, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"))
    })

    output$inv_group_counts <- renderDT({
      x <- inv_result(); req(x)

      if (is.null(x$group_counts) || !nrow(x$group_counts)) {
        return(ara_datatable(
          data.frame(Catatan = "Informasi jumlah kelompok tidak tersedia."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      ara_datatable(
        x$group_counts,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10)
      )
    })

    output$inv_text <- renderText({
      x <- inv_result(); req(x)

      mode_txt <- if (isTRUE(x$fast_mode)) {
        "Mode: Cepat — ML berbasis covariance matrix"
      } else {
        paste0("Mode: Robust — ", x$estimator)
      }

      stage_txt <- if (identical(x$level, "full")) {
        "Tahapan: Configural → Metric → Scalar → Strict"
      } else {
        "Tahapan: Configural → Metric → Scalar"
      }

      paste0(
        mode_txt, "\n",
        stage_txt, "\n",
        "Grouping variable: ", x$group_var, "\n\n",
        "Evaluasi perubahan CFI, RMSEA, dan SRMR antar tahap. ",
        "Mode cepat memakai complete cases untuk variabel model dan covariance matrix per kelompok, ",
        "sehingga lebih ringan untuk dataset besar.\n\n",
        "Gunakan mode Robust bila analisis memerlukan MLR atau indikator ordinal WLSMV."
      )
    })

    output$download_mg_excel <- downloadHandler(
      filename = function() paste0("Multi_Group_SEM_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- mg_result(); req(x)
        export_analysis_excel(prepend_analysis_info(multigroup_excel_tables(x), "Multi-Group SEM", shared$provenance), file)
      }
    )

    output$download_inv_excel <- downloadHandler(
      filename = function() paste0("Measurement_Invariance_Semua_Tabel_", Sys.Date(), ".xlsx"),
      content = function(file) {
        x <- inv_result(); req(x)
        export_analysis_excel(prepend_analysis_info(invariance_excel_tables(x), "Measurement Invariance", shared$provenance), file)
      }
    )
  })
}
