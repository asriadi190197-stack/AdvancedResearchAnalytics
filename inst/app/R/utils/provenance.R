new_dataset_provenance <- function(
  name = NA_character_,
  source = NA_character_,
  source_url = NA_character_,
  local_path = NA_character_,
  notes = NA_character_
) {
  list(
    name = name,
    source = source,
    source_url = source_url,
    local_path = local_path,
    imported_at = as.character(Sys.time()),
    notes = notes
  )
}

provenance_table <- function(prov) {
  if (is.null(prov)) return(data.frame())
  data.frame(
    Field = c("Dataset", "Source", "Source URL", "Local Path", "Imported At", "Notes"),
    Value = c(
      prov$name %||% "",
      prov$source %||% "",
      prov$source_url %||% "",
      prov$local_path %||% "",
      prov$imported_at %||% "",
      prov$notes %||% ""
    ),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

add_provenance_to_docx <- function(doc, prov) {
  if (is.null(prov)) return(doc)
  doc <- officer::body_add_par(doc, "Dataset Provenance", style = "heading 2")
  ft <- flextable::flextable(provenance_table(prov))
  ft <- flextable::autofit(ft)
  flextable::body_add_flextable(doc, ft)
}
