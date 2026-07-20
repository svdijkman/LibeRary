TRIAGE_SYSTEM_PROMPT <- paste(
  "You screen scientific abstracts for pharmacometric models.",
  "Prioritize sensitivity: population PK, PD, PK/PD, exposure-response,",
  "disease-progression and model-based meta-analysis papers are relevant.",
  "Return only JSON. Base the answer exclusively on title and abstract,",
  "state uncertainty, and do not claim that model details are recoverable",
  "unless the abstract provides evidence for that conclusion."
)

.library_triage_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    relevant_probability = list(type = "number", minimum = 0, maximum = 1),
    recoverable_probability = list(type = "number", minimum = 0, maximum = 1),
    model_categories = list(type = "array", items = list(
      type = "string", enum = c("pk", "pd", "pkpd", "exposure_response",
                                "disease_progression", "mbma", "other"))),
    positive_signals = list(type = "array", items = list(type = "string")),
    negative_signals = list(type = "array", items = list(type = "string")),
    evidence = list(type = "array", items = list(type = "string")),
    uncertainty = list(type = "array", items = list(type = "string")),
    rationale = list(type = "string")
  ),
  required = c("relevant_probability", "recoverable_probability", "model_categories",
               "positive_signals", "negative_signals", "evidence", "uncertainty", "rationale")
)

.library_probability <- function(value, default = 0) {
  value <- suppressWarnings(as.numeric(value %||% default))
  if (!length(value) || !is.finite(value[[1L]])) value <- default
  max(0, min(1, value[[1L]]))
}

.library_triage_tier <- function(probability, cfg) {
  if (probability >= cfg$triage$high_threshold) return("high")
  if (probability >= cfg$triage$intermediate_threshold) return("intermediate")
  "low"
}

.library_triage_fallback <- function(metadata, cfg, reason = "triage_llm_unavailable") {
  meta <- ingest_coalesce_metadata(metadata)
  relevance <- ingest_score_model_relevance(paste(meta$title, meta$abstract))
  probability <- if (!nzchar(meta$abstract)) 0.05 else
    c(0.08, 0.48, 0.72, 0.88, 0.94)[[min(4L, relevance$score) + 1L]]
  recoverable <- max(0.03, probability - 0.25)
  list(
    relevant_probability = probability,
    recoverable_probability = recoverable,
    model_categories = if (probability >= cfg$triage$intermediate_threshold) "other" else character(),
    positive_signals = relevance$keywords,
    negative_signals = if (!nzchar(meta$abstract)) "Abstract unavailable" else character(),
    evidence = character(), uncertainty = reason,
    rationale = paste("Deterministic keyword fallback:", reason),
    method = "heuristic_fallback", audit = list(error = reason)
  )
}

.library_normalize_triage <- function(value, cfg, method, audit = NULL) {
  probability <- .library_probability(value$relevant_probability)
  recoverable <- .library_probability(value$recoverable_probability)
  tier <- .library_triage_tier(probability, cfg)
  list(
    relevant_probability = probability,
    recoverable_probability = recoverable,
    tier = tier,
    action = if (tier %in% cfg$triage$first_pass_tiers) "first_pass" else "defer_low",
    model_categories = unique(as.character(unlist(value$model_categories %||% character()))),
    positive_signals = unique(as.character(unlist(value$positive_signals %||% character()))),
    negative_signals = unique(as.character(unlist(value$negative_signals %||% character()))),
    evidence = unique(as.character(unlist(value$evidence %||% character()))),
    uncertainty = unique(as.character(unlist(value$uncertainty %||% character()))),
    rationale = as.character(value$rationale %||% "")[[1L]],
    method = method,
    provider = as.character(audit[["provider"]] %||% if (identical(method, "llm")) "unknown" else "none")[[1L]],
    model = as.character(audit[["model"]] %||% if (identical(method, "llm")) "unknown" else "heuristic")[[1L]],
    audit = audit,
    assessed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )
}

#' Triage one publication from its title and abstract
#'
#' The LLM score is converted to a deterministic `high`, `intermediate`, or
#' `low` tier using configuration thresholds. Low-tier records are deferred,
#' never discarded.
#' @param metadata PubMed metadata.
#' @param cfg LibeRary configuration.
#' @param fallback_on_error Use the conservative deterministic fallback when
#'   the configured triage model cannot be reached.
#' @return Structured triage decision and audit data.
#' @export
ingest_triage_abstract <- function(metadata, cfg = NULL, fallback_on_error = TRUE) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  meta <- ingest_coalesce_metadata(metadata)
  if (!nzchar(meta$title) && !nzchar(meta$abstract)) stop("Title and abstract are both missing.")
  endpoint <- .library_llm_role(cfg, "triage")
  if (!isTRUE(cfg$triage$enabled) || identical(endpoint$provider, "none")) {
    fallback <- .library_triage_fallback(meta, cfg, "triage_disabled")
    return(.library_normalize_triage(fallback, cfg, fallback$method, fallback$audit))
  }
  prompt <- paste0(
    "Assess whether this abstract reports or applies a pharmacometric model.\n",
    "Do not use information outside the supplied title and abstract.\n\n",
    "PMID: ", meta$pmid, "\nTITLE: ", meta$title, "\nABSTRACT:\n", meta$abstract
  )
  result <- tryCatch({
    structured <- .library_structured_chat(
      list(list(role = "system", content = .library_role_instruction(cfg, "triage")),
           list(role = "user", content = prompt)),
      cfg, role = "triage", schema = .library_triage_schema(), sensitive = FALSE
    )
    if (!isTRUE(structured$ok)) stop(structured$error, call. = FALSE)
    response <- structured$response
    audit <- c(unclass(response), list(
      prompt_md5 = .library_text_fingerprint(prompt), prompt_chars = nchar(prompt),
      response = response$content, retry_count = structured$retry_count,
      attempts = structured$attempts
    ))
    .library_normalize_triage(structured$value, cfg, "llm", audit)
  }, error = identity)
  if (!inherits(result, "error")) return(result)
  if (!isTRUE(fallback_on_error)) stop(result)
  fallback <- .library_triage_fallback(meta, cfg, conditionMessage(result))
  .library_normalize_triage(fallback, cfg, fallback$method, fallback$audit)
}

#' Triage a collection of PubMed metadata records
#' @param metadata Named list of metadata records.
#' @param cfg LibeRary configuration.
#' @param log Optional logger.
#' @param progress Optional progress callback.
#' @return A named list of triage decisions.
#' @export
ingest_triage_batch <- function(metadata, cfg = NULL, log = NULL, progress = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  if (!is.list(metadata)) stop("`metadata` must be a list.")
  log <- log %||% function(...) invisible(NULL)
  output <- vector("list", length(metadata)); names(output) <- names(metadata)
  for (index in seq_along(metadata)) {
    record <- metadata[[index]]
    pmid <- ingest_coalesce_metadata(record)$pmid
    if (!is.null(progress)) progress((index - 1) / max(1, length(metadata)),
                                     sprintf("Triaging PMID %s", pmid), index, length(metadata))
    output[[index]] <- ingest_triage_abstract(record, cfg)
    log(sprintf("PMID %s: %s (%.3f)", pmid, output[[index]]$tier,
                output[[index]]$relevant_probability), "INFO")
  }
  if (!is.null(progress)) progress(1, "Abstract triage complete", length(metadata), length(metadata))
  output
}
