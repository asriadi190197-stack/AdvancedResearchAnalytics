mod_learn_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "page-heading",
      tags$span(class = "eyebrow", "PUSAT PEMBELAJARAN"),
      h2("Analisis Lanjutan Terpandu"),
      p("Panduan langkah demi langkah untuk menjalankan analisis tingkat lanjut menggunakan data pendidikan publik.")
    ),

    usage_guide(
      "Cara menggunakan Analisis Lanjutan Terpandu",
      c(
        "Pilih jenis analisis yang ingin dipelajari.",
        "Klik Tampilkan Panduan untuk melihat urutan kerja.",
        "Ikuti tahapan mulai dari pemilihan dataset, pemilihan variabel, menjalankan model, sampai membaca hasil.",
        "Gunakan panduan ini bersama data Asesmen Nasional atau PISA yang tersedia melalui menu Data."
      ),
      "Istilah statistik utama tetap menggunakan istilah akademik internasional agar sesuai dengan buku, artikel, dan output perangkat lunak statistik."
    ),

    layout_columns(
      col_widths = c(4,8),

      card(
        card_header("Pilih Panduan Analisis"),
        selectInput(
          ns("workflow"),
          "Jenis analisis",
          choices = c(
            "HLM dengan Asesmen Nasional" = "an_hlm",
            "Two-Stage IPD Meta-Analisis dengan Asesmen Nasional" = "an_ipd",
            "CFA / SEM dengan Asesmen Nasional" = "an_sem",
            "HLM dengan PISA 2025" = "pisa_hlm",
            "SEM dengan PISA 2025" = "pisa_sem",
            "Discriminant Analisis dengan PISA 2025" = "pisa_da",
            "Cluster Analisis dengan PISA 2025" = "pisa_cluster"
          )
        ),
        actionButton(ns("show_workflow"), "Tampilkan Panduan", class = "btn btn-primary w-100")
      ),

      card(
        card_header("Langkah Analisis"),
        uiOutput(ns("guide"))
      )
    ),

    card(
      card_header("Prinsip Penting untuk Data Besar"),
      tags$ol(
        tags$li("Simpan file data publik asli tanpa perubahan dan gunakan salinan atau subset untuk analisis."),
        tags$li("Untuk file PISA yang sangat besar, pilih hanya variabel yang dibutuhkan agar penggunaan RAM lebih efisien."),
        tags$li("Pada PISA, identifikasi plausible values, sampling weights, dan replicate weights sebelum menafsirkan hasil capaian."),
        tags$li("Pada Asesmen Nasional, ikuti batasan interpretasi yang ditetapkan oleh pemilik data resmi."),
        tags$li("Gunakan cluster ID sebagai variabel pengelompokan statistik, bukan sebagai dasar membuat peringkat wilayah.")
      )
    )
  )
}

mod_learn_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    current <- reactiveVal("an_hlm")

    observeEvent(input$show_workflow, {
      current(input$workflow)
    })

    output$guide <- renderUI({
      w <- current()

      guides <- list(
        an_hlm = list(
          title = "HLM dengan Asesmen Nasional",
          steps = c(
            "Unduh dataset resmi Asesmen Nasional dari portal Kemendikdasmen.",
            "Impor file melalui menu Data.",
            "Tentukan outcome numerik dan ID sekolah/cluster.",
            "Jalankan null model dan periksa ICC.",
            "Tambahkan prediktor Level-1.",
            "Tambahkan prediktor Level-2 bila sesuai dengan desain penelitian.",
            "Bandingkan model dan periksa residual diagnostics.",
            "Ekspor tabel hasil dan laporan Word."
          )
        ),
        an_ipd = list(
          title = "Two-Stage IPD Meta-Analisis dengan Asesmen Nasional",
          steps = c(
            "Impor dataset Asesmen Nasional.",
            "Tentukan Outcome (Y), Predictor (X), cluster ID, dan covariates.",
            "Tetapkan minimum jumlah kasus per cluster.",
            "Jalankan regresi Stage-1 pada setiap cluster.",
            "Gabungkan koefisien menggunakan random-effects model.",
            "Periksa Q, I², τ², confidence interval, dan forest plot.",
            "Periksa leave-one-out diagnostics serta meta-regression bila diperlukan.",
            "Ekspor hasil analisis."
          )
        ),
        an_sem = list(
          title = "CFA / SEM dengan Asesmen Nasional",
          steps = c(
            "Identifikasi indikator yang membentuk konstruk penelitian.",
            "Periksa arah coding dan missing values.",
            "Susun measurement model CFA terlebih dahulu.",
            "Periksa standardized loading dan model fit.",
            "Setelah measurement model memadai, susun structural model.",
            "Periksa direct effect, indirect effect, serta modification indices secara hati-hati.",
            "Ekspor tabel hasil."
          )
        ),
        pisa_hlm = list(
          title = "HLM dengan PISA 2025",
          steps = c(
            "Dapatkan PISA Public Use File melalui prosedur resmi OECD.",
            "Muat file PISA atau buat analysis extract melalui Alat PISA.",
            "Deteksi plausible values, weights, dan school ID.",
            "Tentukan siswa sebagai Level-1 dan sekolah sebagai Level-2.",
            "Jalankan null model dan hitung ICC.",
            "Tambahkan prediktor tingkat siswa dan sekolah.",
            "Periksa model fit dan diagnostics.",
            "Gunakan desain survei dan plausible values secara tepat sebelum membuat inferensi populasi."
          )
        ),
        pisa_sem = list(
          title = "SEM dengan PISA 2025",
          steps = c(
            "Pilih konstruk kuesioner berdasarkan dokumentasi PISA.",
            "Buat subset variabel agar penggunaan memori lebih efisien.",
            "Jalankan CFA sebelum structural model.",
            "Periksa CFI, TLI, RMSEA, SRMR, dan standardized loading.",
            "Jika membandingkan kelompok, jalankan measurement invariance terlebih dahulu.",
            "Perhatikan desain survei PISA ketika membuat inferensi populasi."
          )
        ),
        pisa_da = list(
          title = "Discriminant Analisis dengan PISA 2025",
          steps = c(
            "Tentukan grouping variable yang memiliki dasar teoritis.",
            "Pilih prediktor numerik.",
            "Jalankan LDA atau QDA.",
            "Bandingkan training accuracy dengan cross-validation accuracy.",
            "Periksa confusion matrix.",
            "Gunakan hasil klasifikasi sebagai temuan eksploratif, bukan bukti kausal."
          )
        ),
        pisa_cluster = list(
          title = "Cluster Analisis dengan PISA 2025",
          steps = c(
            "Pilih variabel numerik yang memiliki dasar teoritis.",
            "Standarkan variabel sebelum clustering berbasis jarak.",
            "Bandingkan K-Means, PAM/K-Medoids, atau hierarchical clustering.",
            "Gunakan silhouette bersama interpretasi substantif.",
            "Deskripsikan profil cluster tanpa mengubahnya menjadi peringkat atau penilaian normatif."
          )
        )
      )

      g <- guides[[w]]

      tagList(
        h3(g$title),
        tags$ol(lapply(g$steps, tags$li))
      )
    })
  })
}
