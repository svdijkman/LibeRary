#' Score abstract relevance for pharmacometric models
#'
#' @param text Abstract or title+abstract text.
#' @return Numeric score and matched keywords.
#' @keywords internal
ingest_score_model_relevance <- function(text) {
  txt <- tolower(paste(text, collapse = " "))
  if (!nzchar(txt)) {
    return(list(score = 0L, keywords = character()))
  }
  hits <- MODEL_KEYWORDS[vapply(MODEL_KEYWORDS, function(k) {
    if (!nzchar(k)) return(FALSE)
    grepl(k, txt, fixed = TRUE)
  }, logical(1))]
  list(score = length(hits), keywords = hits)
}

#' Classify acquisition route for a literature record
#'
#' @param metadata Record from [ingest_entrez_fetch_metadata()].
#' @param cfg Ingest config.
#' @param triage Optional result from [ingest_triage_abstract()]. When supplied,
#'   its probability tier determines whether the record belongs to the first
#'   processing pass or the retained low-probability backlog.
#' @return Manifest row list.
#' @export
ingest_classify_entry <- function(metadata, cfg, triage = NULL) {
  meta <- ingest_coalesce_metadata(metadata)
  rel <- ingest_score_model_relevance(paste(meta$title, meta$abstract))
  if (is.null(triage)) {
    probability <- min(0.95, rel$score / 4)
    tier <- .library_triage_tier(probability, cfg)
    triage <- list(
      model_probability = probability,
      recoverability_probability = probability,
      tier = tier,
      recommended_action = if (tier %in% cfg$triage$first_pass_tiers) "first_pass" else "defer",
      method = "legacy_keyword_fallback",
      provider = "none",
      model = "keyword",
      evidence = rel$keywords,
      uncertainty = "No LLM abstract triage was supplied."
    )
  }
  tier <- tolower(as.character(triage$tier %||% "low"))
  first_pass <- tier %in% cfg$triage$first_pass_tiers
  unpaywall <- if (nzchar(meta$doi)) ingest_unpaywall_lookup(meta$doi, cfg) else NULL
  epmc <- ingest_europe_pmc_lookup(pmid = meta$pmid, doi = meta$doi, cfg = cfg)

  pmcid <- meta$pmcid
  if (!nzchar(pmcid) && nzchar(epmc$pmcid)) pmcid <- epmc$pmcid

  pmc_oa <- if (nzchar(pmcid)) ingest_pmc_oa_lookup(pmcid, cfg) else list(is_oa = FALSE, pdf_url = "")

  suggested_url <- ""
  strategy <- ""
  acquisition_class <- "skipped"
  publisher <- meta$journal %||% ""

  pdf_url <- ""
  if (isTRUE(pmc_oa$is_oa) && nzchar(pmc_oa$pdf_url)) {
    pdf_url <- pmc_oa$pdf_url
  } else if (!is.null(unpaywall) && nzchar(unpaywall$url_for_pdf)) {
    pdf_url <- unpaywall$url_for_pdf
  } else if (isTRUE(epmc$is_open_access) && nzchar(epmc$pdf_url)) {
    pdf_url <- epmc$pdf_url
  }

  is_oa <- isTRUE(pmc_oa$is_oa) ||
    (!is.null(unpaywall) && isTRUE(unpaywall$is_oa)) ||
    isTRUE(epmc$is_open_access)

  if (nzchar(pdf_url) && is_oa) {
    acquisition_class <- "oa_auto"
    suggested_url <- pdf_url
    strategy <- if (grepl("ncbi.nlm.nih.gov/pmc", pdf_url)) "pmc_pdf" else "oa_pdf"
  } else if (nzchar(meta$doi)) {
    acquisition_class <- "needs_institutional"
    suggested_url <- sprintf("https://doi.org/%s", ingest_normalize_doi(meta$doi))
    strategy <- "doi_follow"
  } else if (first_pass && nzchar(meta$abstract)) {
    acquisition_class <- "stub"
    suggested_url <- sprintf("https://pubmed.ncbi.nlm.nih.gov/%s/", meta$pmid)
    strategy <- "abstract_only"
  } else if (first_pass) {
    acquisition_class <- "stub"
    suggested_url <- sprintf("https://pubmed.ncbi.nlm.nih.gov/%s/", meta$pmid)
    strategy <- "abstract_only"
  }

  if (!first_pass) {
    # Preserve the actionable route, but keep the article out of the first
    # acquisition pass. It can later be processed with tiers = "low".
    acquisition_class <- "deferred_low"
  }

  list(
    pmid = meta$pmid,
    doi = meta$doi,
    pmcid = pmcid,
    title = meta$title,
    publisher = publisher,
    year = meta$year,
    suggested_url = suggested_url,
    strategy = strategy,
    acquisition_class = acquisition_class,
    status = if (first_pass) "pending" else "deferred",
    relevance_score = rel$score,
    relevance_keywords = paste(rel$keywords, collapse = "; "),
    triage_probability = as.numeric(triage[["relevant_probability"]] %||% triage[["model_probability"]] %||% 0),
    recoverability_probability = as.numeric(triage[["recoverable_probability"]] %||% triage[["recoverability_probability"]] %||% 0),
    triage_tier = tier,
    triage_action = as.character(triage$action %||% triage$recommended_action %||% if (first_pass) "first_pass" else "defer"),
    triage_method = as.character(triage$method %||% "unknown"),
    triage_provider = as.character(triage[["provider"]] %||% "none")[[1L]],
    triage_model = as.character(triage[["model"]] %||% "")[[1L]],
    triage_evidence = paste(as.character(triage$evidence %||% character()), collapse = "; "),
    triage_uncertainty = paste(as.character(triage$uncertainty %||% ""), collapse = " "),
    is_oa = is_oa,
    has_abstract = nzchar(meta$abstract)
  )
}

ingest_coalesce_metadata <- function(metadata) {
  scalar_chr <- function(x) {
    if (is.null(x) || length(x) == 0L) return("")
    paste(as.character(x), collapse = " ")
  }
  list(
    pmid = scalar_chr(metadata$pmid),
    title = scalar_chr(metadata$title),
    abstract = scalar_chr(metadata$abstract),
    journal = scalar_chr(metadata$journal),
    year = scalar_chr(metadata$year),
    doi = scalar_chr(metadata$doi),
    pmcid = sub("^PMC", "", scalar_chr(metadata$pmcid), ignore.case = TRUE)
  )
}
