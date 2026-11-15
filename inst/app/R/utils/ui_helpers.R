metric_card <- function(value, label, note = NULL) {
  div(
    class = "ara-metric-card",
    div(class = "ara-metric-value", value),
    div(class = "ara-metric-label", label),
    if (!is.null(note)) div(class = "ara-metric-note", note)
  )
}

quick_action_card <- function(title, description, badge = NULL, footer = NULL, action = NULL) {
  div(
    class = "ara-action-card",
    if (!is.null(badge)) div(class = "ara-card-badge", badge),
    h4(title),
    p(description),
    if (!is.null(action)) action,
    if (!is.null(footer)) div(class = "ara-card-footer", footer)
  )
}

section_header <- function(eyebrow, title, subtitle = NULL) {
  div(
    class = "ara-section-header",
    tags$span(class = "eyebrow", eyebrow),
    h2(title),
    if (!is.null(subtitle)) p(subtitle)
  )
}
