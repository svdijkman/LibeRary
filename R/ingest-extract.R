EXTRACTION_SYSTEM_PROMPT <- paste(
  "You extract population PK/PD model evidence from scientific publications.",
  "Return only JSON matching the supplied schema. Never infer a numerical value",
  "that is not supported by the supplied text. Use null for missing information,",
  "include short evidence quotations, and distinguish reported facts from suggestions.",
  "All confidence values are proportions from 0 to 1, never percentages.",
  "For OMEGA and SIGMA, preserve the reported number and its metric, but put only",
  "the corresponding NONMEM variance in value. Never copy a CV percent or SD directly",
  "into a variance record. Distinguish IIV/BSV from IOV and residual variability.",
  "Record whether each ETA is normal/additive or log-normal/exponential and preserve",
  "the parameter equation. For P=TVP*exp(ETA), convert a distributional CV fraction c",
  "to OMEGA=log(1+c^2); use c^2 only when the publication explicitly defines the",
  "first-order convention CV=100*sqrt(OMEGA). For P=TVP+ETA, OMEGA is an absolute",
  "variance; convert SD to SD^2 and convert CV only when TVP is known.",
  "Do not confuse shrinkage, RSE, confidence intervals, or bootstrap precision with",
  "interindividual or residual variability. Preserve fixed parameters, covariance or",
  "correlation structure, covariate centering and exponents, units, and transformations.",
  "Extract study cohorts and structured demographics, including age, weight, sex,",
  "ethnicity, organ function and CYP genotype/phenotype. Preserve every reported",
  "summary form (mean/SD, median/quantiles, range, counts) rather than choosing one.",
  "Extract dose regimens and concentration-time points from tables or figures when",
  "recoverable, with source locator, units, scale, statistic, and uncertainty bounds.",
  "Define the canonical mathematical structure before mapping it to NONMEM. ADVAN and",
  "TRANS may be inferred only from route, compartments, input/elimination process and",
  "parameter equations; label the mapping inferred, give rationale and alternatives,",
  "and leave it unresolved when it is not unique.",
  "When the metric or parameterization is ambiguous, use null and require review."
)

ASSESSMENT_SYSTEM_PROMPT <- paste(
  "You are an independent pharmacometric reviewer. Check a proposed extraction",
  "against the supplied publication evidence. Return only JSON. Be conservative:",
  "unsupported values are discrepancies, not plausible assumptions. Verify that",
  "OMEGA and SIGMA values are NONMEM variances rather than reported CV percentages",
  "or standard deviations, and that normal versus log-normal ETA parameterizations",
  "and any covariance blocks agree with the published equations. Independently check",
  "the canonical compartment/input/elimination description and any inferred ADVAN/TRANS",
  "mapping. Verify cohort counts, demographic summary-statistic forms, CYP records, dose",
  "units/regimens, and every digitized concentration point against its cited table or figure."
)

.library_extraction_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    title = list(type = "string"), compound = list(type = c("string", "null")),
    population = list(type = c("string", "null")), route = list(type = c("string", "null")),
    n_subjects = list(type = c("number", "null")), software = list(type = c("string", "null")),
    estimation_method = list(type = c("string", "null")), model_type = list(type = c("string", "null")),
    population_details = .library_population_schema(),
    dosing = .library_dosing_schema(),
    structural_model = .library_structural_schema(),
    parameters = list(type = "object", additionalProperties = FALSE,
      properties = list(
        theta = list(type = "array", items = list(type = "object", additionalProperties = FALSE,
          properties = list(name = list(type = "string"), typical = list(type = c("number", "null")),
            se = list(type = c("number", "null")), unit = list(type = c("string", "null"))),
          required = c("name", "typical", "se", "unit"))),
        omega = list(type = "array", items = list(type = "object", additionalProperties = FALSE,
          properties = list(
            description = list(type = "string"),
            parameter = list(type = "string"),
            eta_index = list(type = c("integer", "null")),
            eta_distribution = list(type = "string",
              enum = c("log_normal", "normal", "other", "unknown")),
            eta_expression = list(type = c("string", "null")),
            variability_level = list(type = "string",
              enum = c("iiv", "iov", "other", "unknown")),
            reported_value = list(type = c("number", "null")),
            reported_metric = list(type = "string",
              enum = c("variance", "sd", "cv_percent", "cv_fraction",
                       "approximate_cv_percent", "unknown")),
            value = list(type = c("number", "null")),
            conversion = list(type = "string")
          ),
          required = c("description", "parameter", "eta_index", "eta_distribution",
                       "eta_expression", "variability_level", "reported_value",
                       "reported_metric", "value", "conversion"))),
        omega_covariance = list(type = "array", items = list(type = "object", additionalProperties = FALSE,
          properties = list(
            row_eta = list(type = "integer"), col_eta = list(type = "integer"),
            reported_value = list(type = c("number", "null")),
            reported_metric = list(type = "string", enum = c("covariance", "correlation", "unknown")),
            value = list(type = c("number", "null")), conversion = list(type = "string")
          ),
          required = c("row_eta", "col_eta", "reported_value", "reported_metric", "value", "conversion"))),
        sigma = list(type = "array", items = list(type = "object", additionalProperties = FALSE,
          properties = list(
            description = list(type = "string"),
            error_model = list(type = "string",
              enum = c("additive", "proportional", "combined", "log_additive", "other", "unknown")),
            reported_value = list(type = c("number", "null")),
            reported_metric = list(type = "string",
              enum = c("variance", "sd", "cv_percent", "cv_fraction", "unknown")),
            value = list(type = c("number", "null")),
            conversion = list(type = "string")
          ),
          required = c("description", "error_model", "reported_value", "reported_metric", "value", "conversion")))),
      required = c("theta", "omega", "omega_covariance", "sigma")),
    covariates = list(type = "array", maxItems = 50L, items = list(type = "string")),
    residual_error = list(type = c("string", "null")),
    reproduction_targets = .library_reproduction_target_schema(),
    confidence = list(type = "object", additionalProperties = FALSE,
      properties = list(overall = list(type = "number"), fields = list(type = "object",
        additionalProperties = FALSE, properties = list(structure = list(type = "number"),
          parameters = list(type = "number"), population = list(type = "number"), software = list(type = "number")),
        required = c("structure", "parameters", "population", "software"))),
      required = c("overall", "fields")),
    evidence_quotes = list(type = "array", maxItems = 30L,
                           items = list(type = "string")), notes = list(type = "string")
  ),
  required = c("title", "compound", "population", "population_details", "route", "n_subjects",
               "dosing", "software", "estimation_method", "model_type", "structural_model",
               "parameters", "covariates", "residual_error", "reproduction_targets",
               "confidence", "evidence_quotes", "notes")
)

.library_assessment_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    verdict = list(type = "string", enum = c("pass", "needs_review", "reject")),
    confidence = list(type = "number"), supported_fields = list(type = "array", items = list(type = "string")),
    discrepancies = list(type = "array", items = list(type = "object", additionalProperties = FALSE,
      properties = list(field = list(type = "string"), proposed = list(type = c("string", "null")),
        evidence = list(type = c("string", "null")), severity = list(type = "string", enum = c("minor", "major"))),
      required = c("field", "proposed", "evidence", "severity"))),
    missing_evidence = list(type = "array", items = list(type = "string")),
    recommended_status = list(type = "string", enum = c("stub", "draft", "review")),
    summary = list(type = "string")
  ), required = c("verdict", "confidence", "supported_fields", "discrepancies",
                   "missing_evidence", "recommended_status", "summary")
)

.library_text_fingerprint <- function(text) {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste(text, collapse = "\n"))), path)
  unname(tools::md5sum(path))
}

ingest_build_extraction_prompt <- function(metadata, full_text = "", supplement_text = "") {
  meta <- ingest_coalesce_metadata(metadata)
  paste0(
    "Extract the reported pharmacometric model. JSON schema:\n",
    jsonlite::toJSON(.library_extraction_schema(), auto_unbox = TRUE, pretty = TRUE), "\n\n",
    "=== METADATA ===\nPMID: ", meta$pmid, "\nTitle: ", meta$title, "\nDOI: ", meta$doi,
    "\nAbstract: ", meta$abstract, "\n\n",
    if (nzchar(supplement_text)) paste0("=== SUPPLEMENT / CONTROL STREAM ===\n", supplement_text, "\n\n") else "",
    if (nzchar(full_text)) paste0("=== ARTICLE TEXT ===\n", full_text) else
      "=== ARTICLE TEXT ===\nNot available. Use metadata only and keep confidence low.\n"
  )
}

.library_supplement_text <- function(directory, max_chars = 80000L) {
  files <- ingest_find_supplements(directory)
  files <- files[grepl("\\.(ctl|mod|txt)$", files, ignore.case = TRUE)]
  if (!length(files)) return("")
  pieces <- vapply(files, function(path) {
    text <- .library_read_text_utf8(path)
    paste0("--- ", basename(path), " ---\n", text)
  }, character(1))
  text <- paste(pieces, collapse = "\n\n")
  if (nchar(text) > max_chars) paste0(substr(text, 1L, max_chars), "\n[SUPPLEMENT TRUNCATED]") else text
}

#' Run model extraction for one publication
#' @param metadata PubMed metadata.
#' @param cfg Ingest configuration.
#' @param pdf_path Optional publication PDF.
#' @param assess Run a separate evidence assessment after extraction.
#' @param full_text Optional pre-extracted article text, used by remote workers.
#' @param supplement_text Optional pre-extracted supplement text.
#' @return Extraction, assessment and immutable audit metadata.
#' @export
ingest_extract_model <- function(metadata, cfg = NULL, pdf_path = NULL, assess = TRUE,
                                 full_text = NULL, supplement_text = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  meta <- ingest_coalesce_metadata(metadata)
  endpoint <- .library_llm_role(cfg, "indexing")
  max_chars <- endpoint$max_pdf_chars %||% cfg$ollama$max_pdf_chars %||% 120000L
  if (is.null(full_text)) {
    full_text <- if (!is.null(pdf_path) && file.exists(pdf_path)) ingest_pdf_text(pdf_path, max_chars) else ""
  } else {
    full_text <- as.character(full_text)[[1L]]
    if (nchar(full_text) > max_chars) full_text <- paste0(substr(full_text, 1L, max_chars), "\n[TRUNCATED]")
  }
  if (!nzchar(full_text) && !nzchar(meta$abstract)) stop("No PDF or abstract is available for extraction.")
  if (is.null(supplement_text)) supplement_text <- .library_supplement_text(file.path(cfg$inbox_dir, meta$pmid))
  status <- if (nzchar(full_text)) "draft" else "stub"
  if (endpoint$provider == "none" || !library_llm_available(cfg, "indexing")) {
    return(list(extraction = ingest_stub_extraction(meta, "indexing_llm_unavailable"), assessment = NULL,
                raw_llm = NULL, status = "stub", used_full_text = nzchar(full_text)))
  }
  prompt <- ingest_build_extraction_prompt(meta, full_text, supplement_text)
  structured <- .library_structured_chat(
    list(list(role = "system", content = .library_role_instruction(cfg, "indexing")),
         list(role = "user", content = prompt)),
    cfg, "indexing", schema = .library_extraction_schema(), sensitive = TRUE
  )
  response <- structured$response %||% list()
  extraction <- if (isTRUE(structured$ok)) tryCatch(
    library_model_enrich(structured$value),
    error = function(e) list(parse_error = conditionMessage(e), raw = response$content %||% NULL)
  ) else list(parse_error = structured$error, raw = response$content %||% NULL)
  assessment <- NULL
  assessment_audit <- NULL
  if (!is.null(extraction$parse_error)) {
    status <- "stub"
  } else if (isTRUE(assess)) {
    assessed <- ingest_assess_model(meta, extraction, cfg, full_text, supplement_text)
    assessment <- assessed$assessment
    assessment_audit <- assessed$audit
    if (!is.null(assessment$recommended_status)) status <- assessment$recommended_status
    if (identical(assessment$verdict, "reject")) status <- "stub"
  }
  audit <- list(
    schema_version = LIBRARY_SCHEMA_VERSION, prompt_version = LIBRARY_PROMPT_VERSION,
    indexing = c(unclass(response), list(prompt_md5 = .library_text_fingerprint(prompt),
                                        prompt_chars = nchar(prompt), response = response$content %||% NULL,
                                        retry_count = structured$retry_count,
                                        attempts = structured$attempts)),
    assessment = assessment_audit,
    source = list(full_text = nzchar(full_text), article_text_md5 = if (nzchar(full_text)) .library_text_fingerprint(full_text) else NULL,
                  supplement_md5 = if (nzchar(supplement_text)) .library_text_fingerprint(supplement_text) else NULL)
  )
  list(extraction = extraction, assessment = assessment, raw_llm = audit,
       status = status, used_full_text = nzchar(full_text))
}

#' Independently assess an extracted model against its evidence
#' @param metadata Publication metadata.
#' @param extraction Parsed extraction.
#' @param cfg Ingest configuration.
#' @param full_text Publication text.
#' @param supplement_text Supplement text.
#' @return Assessment and audit data.
#' @export
ingest_assess_model <- function(metadata, extraction, cfg = NULL,
                                full_text = "", supplement_text = "") {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  endpoint <- .library_llm_role(cfg, "assessment")
  evidence <- paste(ingest_coalesce_metadata(metadata)$abstract, full_text, supplement_text, sep = "\n\n")
  if (endpoint$provider == "none" || !library_llm_available(cfg, "assessment")) {
    quotes <- as.character(unlist(extraction$evidence_quotes %||% character()))
    supported <- if (length(quotes)) vapply(quotes, function(q) nzchar(q) && grepl(q, evidence, fixed = TRUE), logical(1)) else logical()
    assessment <- list(verdict = "needs_review", confidence = 0,
      supported_fields = character(), discrepancies = list(),
      missing_evidence = if (length(supported) && any(!supported)) quotes[!supported] else "Independent LLM assessment not available",
      recommended_status = "draft", summary = "Automated assessment unavailable; human review required.")
    return(list(assessment = assessment, audit = NULL))
  }
  prompt <- paste0(
    "Check this extraction against the evidence. JSON schema:\n",
    jsonlite::toJSON(.library_assessment_schema(), auto_unbox = TRUE, pretty = TRUE),
    "\n\n=== PROPOSED EXTRACTION ===\n", jsonlite::toJSON(extraction, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    "\n\n=== EVIDENCE ===\n", evidence
  )
  structured <- .library_structured_chat(
    list(list(role = "system", content = .library_role_instruction(cfg, "assessment")),
         list(role = "user", content = prompt)),
    cfg, "assessment", schema = .library_assessment_schema(), sensitive = TRUE
  )
  if (!isTRUE(structured$ok)) stop(structured$error, call. = FALSE)
  response <- structured$response
  assessment <- structured$value
  list(assessment = assessment,
       audit = c(unclass(response), list(prompt_md5 = .library_text_fingerprint(prompt),
                                        prompt_chars = nchar(prompt), response = response$content,
                                        retry_count = structured$retry_count,
                                        attempts = structured$attempts)))
}

#' Minimal metadata-only extraction
#' @param metadata PubMed metadata.
#' @param reason Reason for creating a stub.
#' @return Extraction-like list requiring review.
#' @export
ingest_stub_extraction <- function(metadata, reason = "abstract_only") {
  meta <- ingest_coalesce_metadata(metadata)
  rel <- ingest_score_model_relevance(paste(meta$title, meta$abstract))
  extraction <- list(title = meta$title, compound = NULL, population = NULL,
       population_details = library_population_normalize(NULL), route = NULL,
       n_subjects = NULL, dosing = list(),
       software = if (any(grepl("nonmem", rel$keywords))) "NONMEM" else NULL,
       estimation_method = NULL, model_type = NULL,
       structural_model = list(advan = NULL, trans = NULL, compartments = NULL, description = "",
                               canonical = .library_structural_canonical(list(), NULL, list()),
                               implementations = list()),
       parameters = list(theta = list(), omega = list(), omega_covariance = list(), sigma = list()),
       covariates = character(),
       residual_error = NULL,
       reproduction_targets = list(),
       confidence = list(overall = min(0.25, 0.05 * rel$score),
                         fields = list(structure = 0, parameters = 0, population = 0.1, software = 0.1)),
       evidence_quotes = character(),
       notes = paste("Metadata stub:", reason, "- full model evidence and human review are required."))
  library_model_enrich(extraction)
}

#' Parse JSON from an LLM response
#' @param text Raw response, optionally fenced or surrounded by short prose.
#' @param schema Optional JSON-schema-like list used to validate the decoded
#'   response before it reaches semantic model code.
#' @return Parsed list.
#' @export
ingest_parse_llm_json <- function(text, schema = NULL) {
  txt <- .library_llm_json_payload(text)
  value <- .library_normalize_llm_probabilities(
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  )
  if (!is.null(schema)) .library_validate_structured_value(value, schema)
  value
}

.library_llm_json_payload <- function(text) {
  text <- as.character(text %||% "")
  if (!length(text) || !nzchar(trimws(text[[1L]]))) {
    stop("The model returned an empty structured response.", call. = FALSE)
  }
  txt <- trimws(text[[1L]])
  txt <- sub("^```(?:json)?[[:space:]]*", "", txt, ignore.case = TRUE)
  txt <- sub("[[:space:]]*```[[:space:]]*$", "", txt)
  starts <- gregexpr("[\\[{]", txt, perl = TRUE)[[1L]]
  if (identical(starts[[1L]], -1L)) stop("No JSON object or array was found in the model response.", call. = FALSE)
  txt <- substr(txt, starts[[1L]], nchar(txt))

  chars <- strsplit(txt, "", fixed = TRUE)[[1L]]
  stack <- character()
  in_string <- FALSE
  escaped <- FALSE
  complete_at <- NA_integer_
  for (index in seq_along(chars)) {
    char <- chars[[index]]
    if (in_string) {
      if (escaped) escaped <- FALSE else if (identical(char, "\\")) escaped <- TRUE else if (identical(char, '"')) in_string <- FALSE
      next
    }
    if (identical(char, '"')) {
      in_string <- TRUE
    } else if (char %in% c("{", "[")) {
      stack <- c(stack, char)
    } else if (char %in% c("}", "]")) {
      if (!length(stack)) stop("Malformed JSON: unexpected closing delimiter.", call. = FALSE)
      expected <- if (identical(utils::tail(stack, 1L), "{")) "}" else "]"
      if (!identical(char, expected)) stop("Malformed JSON: mismatched closing delimiter.", call. = FALSE)
      stack <- utils::head(stack, -1L)
      if (!length(stack)) {
        complete_at <- index
        break
      }
    }
  }
  if (is.finite(complete_at)) {
    txt <- paste0(chars[seq_len(complete_at)], collapse = "")
  } else {
    tail_text <- trimws(txt)
    # Only repair missing structural delimiters. Never complete a quoted value,
    # field name, or dangling key/value separator because that would invent data.
    if (in_string || !length(stack) || grepl("[:,]$", tail_text)) {
      stop("Incomplete JSON response; a value was truncated.", call. = FALSE)
    }
    closers <- vapply(rev(stack), function(open) if (identical(open, "{")) "}" else "]", character(1))
    txt <- paste0(txt, paste0(closers, collapse = ""))
  }
  # Trailing commas are a common local-model formatting error and can be fixed
  # without changing any value.
  repeat {
    repaired <- gsub(",([[:space:]]*[}\\]])", "\\1", txt, perl = TRUE)
    if (identical(repaired, txt)) break
    txt <- repaired
  }
  txt
}

.library_schema_path <- function(path, key) {
  if (grepl("^[0-9]+$", as.character(key))) paste0(path, "[", key, "]") else paste(path, key, sep = ".")
}

.library_schema_matches_type <- function(value, type) {
  switch(type,
    null = is.null(value),
    object = is.list(value) && (!is.null(names(value)) || !length(value)),
    array = is.list(value) && (is.null(names(value)) || !length(value)),
    string = is.character(value) && length(value) == 1L && !is.na(value),
    number = is.numeric(value) && length(value) == 1L && is.finite(value),
    integer = is.numeric(value) && length(value) == 1L && is.finite(value) && value == round(value),
    boolean = is.logical(value) && length(value) == 1L && !is.na(value),
    FALSE
  )
}

.library_validate_structured_value <- function(value, schema, path = "response") {
  allowed <- as.character(unlist(schema$type %||% character()))
  if (length(allowed) && !any(vapply(allowed, function(type) .library_schema_matches_type(value, type), logical(1)))) {
    stop("Invalid structured response at ", path, ": expected ", paste(allowed, collapse = " or "), ".", call. = FALSE)
  }
  if (is.null(value)) return(invisible(TRUE))
  if (!is.null(schema$enum) && !any(vapply(schema$enum, identical, logical(1), value))) {
    stop("Invalid structured response at ", path, ": value is outside the allowed enumeration.", call. = FALSE)
  }
  if (is.numeric(value) && length(value) == 1L) {
    if (!is.null(schema$minimum) && value < schema$minimum) stop("Invalid structured response at ", path, ": below minimum.", call. = FALSE)
    if (!is.null(schema$maximum) && value > schema$maximum) stop("Invalid structured response at ", path, ": above maximum.", call. = FALSE)
  }
  is_object <- "object" %in% allowed && .library_schema_matches_type(value, "object")
  is_array <- "array" %in% allowed && .library_schema_matches_type(value, "array")
  if (is_object) {
    fields <- names(value) %||% character()
    missing <- setdiff(as.character(unlist(schema$required %||% character())), fields)
    if (length(missing)) stop("Invalid structured response at ", path, ": missing required field `", missing[[1L]], "`.", call. = FALSE)
    properties <- schema$properties %||% list()
    if (identical(schema$additionalProperties, FALSE)) {
      extra <- setdiff(fields, names(properties))
      if (length(extra)) stop("Invalid structured response at ", path, ": unexpected field `", extra[[1L]], "`.", call. = FALSE)
    }
    for (field in intersect(fields, names(properties))) {
      .library_validate_structured_value(value[[field]], properties[[field]], .library_schema_path(path, field))
    }
  } else if (is_array) {
    if (!is.null(schema$maxItems) && length(value) > schema$maxItems) stop("Invalid structured response at ", path, ": too many items.", call. = FALSE)
    if (!is.null(schema$items)) for (index in seq_along(value)) {
      .library_validate_structured_value(value[[index]], schema$items, .library_schema_path(path, index))
    }
  }
  invisible(TRUE)
}

.library_structured_length_limited <- function(response) {
  identical(.library_semantic_scalar(
    response$done_reason %||% response$usage$done_reason
  ), "length")
}

.library_structured_request_limits <- function(cfg, role) {
  endpoint <- .library_llm_role(cfg, role)
  if (!identical(endpoint$provider, "ollama")) {
    return(list(num_ctx = NULL, num_predict = NULL))
  }
  list(
    num_ctx = suppressWarnings(as.integer(
      endpoint$num_ctx %||% cfg$ollama$num_ctx %||% 16384L
    )),
    num_predict = suppressWarnings(as.integer(
      endpoint$num_predict %||% cfg$ollama$num_predict %||% 8192L
    ))
  )
}

.library_structured_expand_context <- function(cfg, role) {
  endpoint <- .library_llm_role(cfg, role)
  if (!identical(endpoint$provider, "ollama")) return(cfg)
  current <- suppressWarnings(as.integer(
    endpoint$num_ctx %||% cfg$ollama$num_ctx %||% 16384L
  ))
  if (!length(current) || !is.finite(current[[1L]])) current <- 16384L
  current <- current[[1L]]
  expanded <- min(32768L, max(current + 4096L,
                              as.integer(ceiling(current * 1.5 / 1024) * 1024)))
  cfg$llm[[role]]$num_ctx <- expanded
  cfg
}

.library_structured_length_recovery_message <- function(error) {
  paste(
    "The preceding attempt reached the output/context limit:", error,
    "Regenerate the complete JSON response from the beginning; do not continue the truncated text.",
    "Complete every required schema field. Preserve all reported numerical model parameters and equations.",
    "Be compact: never duplicate a cohort, descriptor, statistic, parameter, or evidence item;",
    "use null or [] for unreported information; keep narrative fields and evidence excerpts short.",
    "Prioritize schema completeness and pharmacometric facts over prose. Return JSON only."
  )
}

.library_structured_chat <- function(messages, cfg, role, schema, sensitive = TRUE,
                                     max_retries = NULL, chat = library_llm_chat) {
  retries <- suppressWarnings(as.integer(max_retries %||% cfg$llm$structured_retries %||% 1L))
  if (!length(retries) || !is.finite(retries[[1L]])) retries <- 1L
  retries <- max(0L, min(3L, retries[[1L]]))
  attempts <- list()
  working_messages <- messages
  working_cfg <- cfg
  last_response <- NULL
  last_error <- "Structured response failed."
  for (attempt in seq_len(retries + 1L)) {
    request_limits <- .library_structured_request_limits(working_cfg, role)
    response <- tryCatch(
      chat(working_messages, working_cfg, role = role, format = schema, sensitive = sensitive),
      error = identity
    )
    if (inherits(response, "error")) {
      last_error <- conditionMessage(response)
      attempts[[attempt]] <- list(attempt = attempt, response = NULL, error = last_error)
    } else {
      last_response <- response
      content <- as.character(response$content %||% "")
      content <- if (length(content)) content[[1L]] else ""
      parsed <- tryCatch(ingest_parse_llm_json(content, schema), error = identity)
      error <- if (inherits(parsed, "error")) conditionMessage(parsed) else NULL
      length_limited <- .library_structured_length_limited(response)
      if (!is.null(error) && length_limited) {
        error <- paste0(error, " The provider reported that its output-length limit was reached.")
      }
      attempts[[attempt]] <- list(
        attempt = attempt,
        provider = response$provider %||% NULL,
        model = response$model %||% NULL,
        done_reason = response$done_reason %||% response$usage$done_reason %||% NULL,
        request_num_ctx = request_limits$num_ctx,
        request_num_predict = request_limits$num_predict,
        usage = response$usage %||% NULL,
        response = content,
        error = error
      )
      if (!inherits(parsed, "error")) {
        return(list(ok = TRUE, value = parsed, response = response, attempts = attempts,
                    retry_count = attempt - 1L, error = NULL))
      }
      last_error <- error
      if (length_limited) {
        # Never feed a large truncated answer back into the next request. That
        # consumes the very context required to regenerate a complete object.
        working_cfg <- .library_structured_expand_context(working_cfg, role)
        working_messages <- c(messages, list(list(
          role = "user",
          content = .library_structured_length_recovery_message(error)
        )))
      } else {
        working_messages <- c(
          working_messages,
          list(list(role = "assistant", content = content)),
          list(list(role = "user", content = paste(
            "The preceding response was invalid:", error,
            "Regenerate the complete response from the beginning as JSON only.",
            "Match the supplied schema exactly, keep narrative fields concise, and use null or [] only where the schema permits."
          )))
        )
      }
    }
  }
  list(ok = FALSE, value = NULL, response = last_response, attempts = attempts,
       retry_count = max(0L, length(attempts) - 1L), error = last_error)
}

.library_probability_value <- function(value, path) {
  if (is.null(value) || length(value) != 1L) return(value)
  original <- value
  if (is.character(value)) {
    value <- trimws(value)
    percent <- grepl("%$", value)
    value <- suppressWarnings(as.numeric(sub("%$", "", value)))
    if (!is.finite(value)) return(original)
    if (percent) value <- value / 100
  } else if (is.numeric(value)) {
    value <- as.numeric(value)
  } else {
    return(original)
  }
  # Some local models ignore JSON-schema bounds and return percentages. Values
  # in (1, 100] are unambiguous percentages; preserve 1 as a valid probability.
  if (value > 1 && value <= 100) value <- value / 100
  if (!is.finite(value) || value < 0 || value > 1) {
    stop("Invalid probability at ", path, ": expected 0-1 or 0-100%.", call. = FALSE)
  }
  value
}

.library_normalize_llm_probabilities <- function(value, path = "response",
                                                  probability_context = FALSE) {
  if (!is.list(value)) {
    return(if (probability_context) .library_probability_value(value, path) else value)
  }
  labels <- names(value)
  for (index in seq_along(value)) {
    label <- if (!is.null(labels) && nzchar(labels[[index]])) labels[[index]] else as.character(index)
    child_path <- paste(path, label, sep = ".")
    child_context <- probability_context ||
      label %in% c("confidence", "recoverability", "coverage") ||
      grepl("(?:^|_)(?:confidence|probability)$", label)
    normalized <- .library_normalize_llm_probabilities(
      value[[index]], child_path, child_context
    )
    # Single-bracket assignment preserves explicit JSON null list elements.
    value[index] <- list(normalized)
  }
  value
}
