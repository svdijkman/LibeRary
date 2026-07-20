#' Search the catalog
#'
#' Keyword search over title, compound, population, keywords, and abstract
#' (when present in manifest).
#'
#' @param query Free-text query (case-insensitive).
#' @param compound Filter by compound name (partial match).
#' @param population Filter by population (partial match).
#' @param status Filter by status.
#' @param min_confidence Minimum overall confidence score.
#' @param advan Optional ADVAN number.
#' @param model_type Optional model type (`pk`, `pd`, or `pkpd`).
#' @param assessment Optional assessment verdict.
#' @param root Catalog root.
#' @return Data frame of matching entries (same columns as [library_list()]).
#' @export
library_search <- function(
    query = "",
    compound = NULL,
    population = NULL,
    status = NULL,
    min_confidence = NULL,
    advan = NULL,
    model_type = NULL,
    assessment = NULL,
    root = library_catalog_root()) {
  df <- library_list(status = status, root = root)
  if (!nrow(df)) return(df)

  if (!is.null(compound) && nzchar(compound)) {
    df <- df[grepl(compound, df$compound, ignore.case = TRUE), , drop = FALSE]
  }
  if (!is.null(population) && nzchar(population)) {
    df <- df[grepl(population, df$population, ignore.case = TRUE), , drop = FALSE]
  }
  if (!is.null(min_confidence)) {
    df <- df[!is.na(df$confidence_overall) & df$confidence_overall >= min_confidence, , drop = FALSE]
  }
  if (!is.null(advan)) df <- df[!is.na(df$advan) & df$advan %in% as.integer(advan), , drop = FALSE]
  if (!is.null(model_type) && nzchar(model_type)) {
    df <- df[tolower(df$model_type) == tolower(model_type), , drop = FALSE]
  }
  if (!is.null(assessment) && nzchar(assessment)) {
    df <- df[tolower(df$assessment) == tolower(assessment), , drop = FALSE]
  }
  if (nzchar(query)) {
    q <- tolower(query)
    keep <- vapply(df$library_id, function(id) {
      m <- tryCatch(.library_read_manifest(id, root), error = function(e) NULL)
      if (is.null(m)) return(FALSE)
      hay <- tolower(paste(
        m$title, m$study$compound, m$study$population,
        paste(m$study$keywords %||% "", collapse = " "),
        m$study$abstract %||% "",
        collapse = " "
      ))
      grepl(q, hay, fixed = TRUE) || any(vapply(strsplit(q, "\\s+")[[1]], function(w) {
        nzchar(w) && grepl(w, hay, fixed = TRUE)
      }, logical(1)))
    }, logical(1))
    df <- df[keep, , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}
