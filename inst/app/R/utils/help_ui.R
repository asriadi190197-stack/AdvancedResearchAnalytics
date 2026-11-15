usage_guide <- function(title, steps, note = NULL) {
  tags$details(
    class = "usage-guide",
    tags$summary(
      div(
        class = "usage-guide-summary",
        div(class = "usage-guide-icon", "?"),
        div(
          h4(title),
          p("Klik untuk membuka petunjuk penggunaan.")
        )
      )
    ),
    div(
      class = "usage-guide-body",
      tags$ol(
        class = "usage-steps",
        lapply(steps, tags$li)
      ),
      if (!is.null(note)) div(class = "usage-note", note)
    )
  )
}
