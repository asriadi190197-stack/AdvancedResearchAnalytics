mod_about_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "about-hero",
      tags$span(class = "about-badge", paste0("VERSI ", APP_VERSION)),
      h2("Advanced Research Analytics"),
      p(
        "Aplikasi analisis statistik tingkat lanjut untuk penelitian pendidikan dan ilmu sosial, ",
        "dengan fokus pada HLM, IPD Meta-Analysis, Multivariate Analysis, serta SEM/CFA."
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Pengembang"),
        div(
          class = "profile-card",
          div(class = "profile-avatar", "MA"),
          div(
            h4("Dr. Muh. Asriadi AM, M.Pd."),
            p(class = "text-muted", "Pengembang Utama & Perancang Metodologi"),
            div(
              class = "about-meta",
              div(class = "label", "Afiliasi"),
              div("Dosen UPI Kampus Cibiru"),
              div(class = "label", "Email"),
              div("muhasriadi@upi.edu")
            )
          )
        )
      ),

      card(
        card_header("Ruang Lingkup"),
        tags$ul(
          class = "about-list",
          tags$li("Multilevel / HLM"),
          tags$li("IPD Meta-Analysis"),
          tags$li("MANOVA, MANCOVA, Discriminant, dan Cluster Analysis"),
          tags$li("CFA, SEM, Multi-Group SEM, dan Measurement Invariance")
        )
      )
    ),

    layout_columns(
      col_widths = c(7, 5),

      card(
        card_header("Tujuan Aplikasi"),
        p(
          "Advanced Research Analytics dikembangkan untuk membantu peneliti, dosen, mahasiswa, ",
          "dan praktisi dalam mengolah serta menganalisis data empiris yang bersumber dari data primer ",
          "maupun data sekunder menggunakan metode statistik tingkat lanjut."
        ),
        p(
          "Aplikasi ini mendukung analisis Multilevel/HLM, IPD Meta-Analysis, Multivariate Analysis, ",
          "serta SEM/CFA, termasuk pengolahan dataset penelitian berukuran besar dan dataset publik ",
          "seperti Asesmen Nasional dan OECD/PISA."
        ),
        p(
          "Melalui alur kerja yang terpandu, aplikasi membantu pengguna mulai dari impor dan pemetaan data, ",
          "pemilihan variabel, pelaksanaan analisis, pemeriksaan hasil, hingga ekspor seluruh tabel analisis ",
          "ke Excel."
        ),
        p(
          "Aplikasi dirancang sebagai perangkat bantu analisis penelitian yang praktis, sistematis, ",
          "dan mudah digunakan, tanpa menghilangkan kebutuhan pengguna untuk menyesuaikan model dengan ",
          "desain penelitian dan karakteristik data yang dianalisis."
        )
      ),

      card(
        card_header("Informasi Aplikasi"),
        div(
          class = "about-meta",
          div(class = "label", "Nama"),
          div(APP_NAME),
          div(class = "label", "Versi"),
          div(APP_VERSION),
          div(class = "label", "Platform"),
          div("R / Shiny"),
          div(class = "label", "Bahasa"),
          div("Bahasa Indonesia")
        )
      )
    ))
}

mod_about_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
})
}
