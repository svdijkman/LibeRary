DUAL_EXTRACTION_SYSTEM_PROMPT <- paste(
  "You extract recoverable population PK/PD models from scientific evidence.",
  "Return only JSON matching the schema. Never invent numerical values.",
  "Preserve the publication's parameterization and attach a source locator to",
  "every material claim. A paper may discuss modelling without reporting a",
  "recoverable model; represent that explicitly with model_present=false.",
  "All probability, recoverability, evidence, and extraction confidence values",
  "are proportions from 0 to 1, never percentages.",
  "Return compact JSON and complete every required schema field before adding",
  "narrative detail. Never repeat the same cohort, descriptor, statistic, parameter,",
  "or evidence item. Use null or [] for information that was not reported, and keep",
  "evidence, rationale, notes, and limitations concise while retaining source locators.",
  "For every OMEGA or SIGMA value, preserve the reported number and metric while",
  "placing only the converted NONMEM variance in value. A CV percent or SD must never",
  "be copied directly into a variance record. For P=TVP*exp(ETA), a distributional",
  "CV fraction c implies OMEGA=log(1+c^2); use c^2 only when the paper explicitly",
  "defines CV=100*sqrt(OMEGA). For P=TVP+ETA, OMEGA is an absolute variance.",
  "Record the ETA distribution and parameter equation. Distinguish IIV/BSV, IOV,",
  "residual variability, shrinkage, and parameter precision. Preserve covariance",
  "blocks, correlations, units, fixed parameters, transformations, covariate centering",
  "and exponents. Extract cohorts, age, weight, sex, organ function and CYP",
  "genotype/phenotype with the exact reported summary-statistic form. Extract dosing",
  "and digitizable concentration data from tables and figures with units and locators.",
  "First encode canonical compartments, input, elimination and parameterization; then",
  "map to ADVAN/TRANS. Mark the mapping reported or inferred, explain the inference,",
  "retain alternatives, and leave ambiguous mappings unresolved. Leave ambiguous",
  "values null and explain the limitation."
)

ADJUDICATION_SYSTEM_PROMPT <- paste(
  "You adjudicate two independently produced pharmacometric model extractions.",
  "Resolve each conflict only from the supplied publication evidence. Prefer",
  "a directly supported value over consensus or plausibility. Return unresolved",
  "fields explicitly; never manufacture a compromise value. Explicitly verify",
  "OMEGA/SIGMA scale conversions, normal versus log-normal ETA equations, covariance",
  "structure, and the distinction between variability, shrinkage, and uncertainty.",
  "Resolve canonical model structure before any ADVAN/TRANS mapping. Check cohort-level",
  "demographics, CYP records, dose units/regimens, and figure/table concentration targets",
  "against their locators. Leave any implementation or digitized point unresolved when",
  "the source evidence does not distinguish the alternatives."
)

.library_recoverability_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    overall = list(type = "number", minimum = 0, maximum = 1),
    structure = list(type = "number", minimum = 0, maximum = 1),
    parameters = list(type = "number", minimum = 0, maximum = 1),
    variability = list(type = "number", minimum = 0, maximum = 1),
    covariates = list(type = "number", minimum = 0, maximum = 1),
    data = list(type = "number", minimum = 0, maximum = 1)
  ), required = c("overall", "structure", "parameters", "variability", "covariates", "data")
)

.library_evidence_item_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    field = list(type = "string"),
    value = list(type = c("string", "null")),
    source_locator = list(type = "string"),
    evidence = list(type = "string"),
    confidence = list(type = "number", minimum = 0, maximum = 1)
  ), required = c("field", "value", "source_locator", "evidence", "confidence")
)

.library_lane_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    model_present = list(type = "boolean"),
    model_probability = list(type = "number", minimum = 0, maximum = 1),
    recoverability = .library_recoverability_schema(),
    extraction = .library_extraction_schema(),
    field_evidence = list(type = "array", maxItems = 20L,
                          items = .library_evidence_item_schema()),
    limitations = list(type = "array", maxItems = 20L, items = list(type = "string"))
  ), required = c("model_present", "model_probability", "recoverability", "extraction",
                  "field_evidence", "limitations")
)

.library_adjudication_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    model_present = list(type = "boolean"),
    resolved_extraction = .library_extraction_schema(),
    decisions = list(type = "array", items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        field = list(type = "string"),
        selected = list(type = "string", enum = c("text", "vision", "neither", "equivalent")),
        resolved_value = list(type = c("string", "null")),
        source_locator = list(type = "string"),
        evidence = list(type = "string"),
        rationale = list(type = "string")
      ), required = c("field", "selected", "resolved_value", "source_locator", "evidence", "rationale")
    )),
    unresolved_fields = list(type = "array", items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(field = list(type = "string"),
                        impact = list(type = "string", enum = c("minor", "major")),
                        reason = list(type = "string")),
      required = c("field", "impact", "reason")
    )),
    confidence = list(type = "number", minimum = 0, maximum = 1),
    summary = list(type = "string")
  ), required = c("model_present", "resolved_extraction", "decisions", "unresolved_fields",
                  "confidence", "summary")
)

.library_bundle_text <- function(bundle, max_chars = 120000L) {
  path <- as.character(bundle$parser$markdown_path %||% "")[[1L]]
  if (!nzchar(path) || !file.exists(path)) return("")
  text <- .library_read_text_utf8(path)
  if (nchar(text) > max_chars) paste0(substr(text, 1L, max_chars), "\n[DOCUMENT TRUNCATED]") else text
}

.library_lane_audit <- function(structured, prompt, lane, bundle) {
  response <- structured$response %||% list()
  c(unclass(response), list(
    lane = lane,
    prompt_version = LIBRARY_PROMPT_VERSION,
    prompt_md5 = .library_text_fingerprint(prompt),
    prompt_chars = nchar(prompt),
    response = response$content %||% NULL,
    structured_valid = isTRUE(structured$ok),
    structured_error = structured$error %||% NULL,
    retry_count = structured$retry_count %||% 0L,
    attempts = structured$attempts %||% list(),
    source_sha256 = bundle$source$sha256 %||% "",
    source_parser = if (identical(lane, "text")) bundle$parser$name %||% "" else "original_pdf_pages"
  ))
}

.library_enrich_lane_result <- function(value) {
  if (is.list(value) && is.list(value$extraction)) {
    value$extraction <- library_model_enrich(value$extraction)
  }
  value
}

#' Extract a model from the standard-parser text lane
#' @param metadata Publication metadata.
#' @param bundle Document bundle.
#' @param cfg LibeRary configuration.
#' @param progress Optional callback accepting `value`, `message`, and `stage`.
#' @return Lane result with extraction and immutable audit data.
#' @export
ingest_extract_text_lane <- function(metadata, bundle, cfg = NULL, progress = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  if (isTRUE(cfg$deliberative$enabled)) {
    result <- tryCatch(
      ingest_deliberative_extract(metadata, bundle, cfg, progress = progress),
      error = identity
    )
    if (!inherits(result, "error")) return(result)
    return(list(
      available = FALSE, lane = "text", error = conditionMessage(result),
      audit = list(
        lane = "deliberative_text", pipeline = "evidence_led_deliberative",
        pipeline_version = LIBRARY_PROMPT_VERSION,
        error = conditionMessage(result)
      )
    ))
  }
  endpoint <- .library_llm_role(cfg, "indexing")
  max_chars <- endpoint$max_pdf_chars %||% cfg$ollama$max_pdf_chars %||% 120000L
  text <- .library_bundle_text(bundle, max_chars)
  meta <- ingest_coalesce_metadata(metadata)
  if (!nzchar(text)) return(list(available = FALSE, lane = "text", error = "Parsed document text is unavailable."))
  prompt <- paste0(
    "Extract the model using only this standard-parser document representation. ",
    "Page or section markers are source locators.\n\nPMID: ", meta$pmid,
    "\nTITLE: ", meta$title, "\nDOI: ", meta$doi,
    "\n\nDOCUMENT:\n", text
  )
  structured <- .library_structured_chat(
      list(list(role = "system", content = .library_role_instruction(cfg, "indexing")),
           list(role = "user", content = prompt)),
      cfg, role = "indexing", schema = .library_lane_schema(), sensitive = TRUE
  )
  result <- tryCatch({
    if (!isTRUE(structured$ok)) stop(structured$error, call. = FALSE)
    list(available = TRUE, lane = "text",
         result = .library_enrich_lane_result(structured$value),
         audit = .library_lane_audit(structured, prompt, "text", bundle))
  }, error = identity)
  if (inherits(result, "error")) list(
    available = FALSE, lane = "text", error = conditionMessage(result),
    audit = .library_lane_audit(structured, prompt, "text", bundle)
  ) else result
}

#' Extract a model directly from original PDF page images
#' @param metadata Publication metadata.
#' @param bundle Document bundle.
#' @param cfg LibeRary configuration.
#' @return Lane result with extraction and immutable audit data.
#' @export
ingest_extract_vision_lane <- function(metadata, bundle, cfg = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  paths <- as.character(unlist(bundle$vision$image_paths %||% character()))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(list(available = FALSE, lane = "vision", error = "Rendered PDF pages are unavailable."))
  meta <- ingest_coalesce_metadata(metadata)
  page_numbers <- as.integer(unlist(bundle$vision$page_numbers %||% seq_along(paths)))
  prompt <- paste0(
    "Extract the reported model directly from the supplied original PDF pages. ",
    "Read equations, tables, figures, captions, and footnotes. Do not use a parsed-text representation. ",
    "Use PDF page numbers as source locators.\n\nPMID: ", meta$pmid,
    "\nTITLE: ", meta$title, "\nDOI: ", meta$doi
  )
  result <- tryCatch({
    user_message <- library_image_message(prompt, paths, paste("PDF page", page_numbers), detail = "high")
    structured <- .library_structured_chat(
      list(list(role = "system", content = .library_role_instruction(cfg, "vision")), user_message),
      cfg, role = "vision", schema = .library_lane_schema(), sensitive = TRUE
    )
    if (!isTRUE(structured$ok)) stop(structured$error, call. = FALSE)
    list(available = TRUE, lane = "vision",
         result = .library_enrich_lane_result(structured$value),
         audit = .library_lane_audit(structured, prompt, "vision", bundle))
  }, error = identity)
  if (inherits(result, "error")) list(
    available = FALSE, lane = "vision", error = conditionMessage(result),
    audit = if (exists("structured", inherits = FALSE)) .library_lane_audit(structured, prompt, "vision", bundle) else NULL
  ) else result
}

.library_claim_value <- function(value) {
  if (is.null(value) || !length(value)) return(NULL)
  if (is.atomic(value) && length(value) == 1L && (is.na(value) || !nzchar(trimws(as.character(value))))) return(NULL)
  value
}

.library_parameter_claims <- function(items, prefix, name_field = "name") {
  output <- list()
  for (index in seq_along(items %||% list())) {
    item <- items[[index]]
    label <- toupper(trimws(as.character(item[[name_field]] %||% index)))
    if (!nzchar(label)) label <- as.character(index)
    value <- item$typical %||% item$value %||% NULL
    output[[paste(prefix, label, sep = ".")]] <- .library_claim_value(value)
  }
  output
}

.library_claim_json <- function(value) {
  if (is.null(value) || !length(value)) return(NULL)
  jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA)
}

.library_extraction_claims <- function(lane_result) {
  lane <- lane_result$result %||% lane_result
  extraction <- lane$extraction %||% ingest_stub_extraction(list(title = ""), "missing_lane")
  structural <- extraction$structural_model %||% list()
  implementations <- structural$implementations %||% list()
  implementation <- if (length(implementations)) implementations[[1L]] else list()
  population_details <- extraction$population_details %||% list()
  claims <- list(
    model_present = lane$model_present %||% FALSE,
    compound = extraction$compound,
    population = extraction$population,
    route = extraction$route,
    n_subjects = extraction$n_subjects,
    software = extraction$software,
    estimation_method = extraction$estimation_method,
    model_type = extraction$model_type,
    `structural.advan` = structural$advan,
    `structural.trans` = structural$trans,
    `structural.compartments` = structural$compartments,
    `structural.description` = structural$description,
    `structural.canonical.compartments` = structural$canonical$compartments$count,
    `structural.canonical.input` = structural$canonical$input$process,
    `structural.canonical.elimination` = structural$canonical$elimination$type,
    `structural.canonical.parameterization` = structural$canonical$parameterization$type,
    `structural.implementation_status` = implementation$status %||% NULL,
    `population_details.n_total` = population_details$n_total,
    `population_details.cohorts` = .library_claim_json(population_details$cohorts),
    dosing = .library_claim_json(extraction$dosing),
    reproduction_targets = .library_claim_json(extraction$reproduction_targets),
    covariates = sort(tolower(as.character(unlist(extraction$covariates %||% character())))),
    residual_error = extraction$residual_error
  )
  c(claims,
    .library_parameter_claims(extraction$parameters$theta, "theta", "name"),
    .library_parameter_claims(extraction$parameters$omega, "omega", "description"),
    .library_parameter_claims(extraction$parameters$sigma, "sigma", "description"))
}

.library_claim_equal <- function(a, b, tolerance = 0.02) {
  a <- .library_claim_value(a); b <- .library_claim_value(b)
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)
  if (is.numeric(a) && is.numeric(b) && length(a) == 1L && length(b) == 1L) {
    return(abs(a - b) <= max(1e-8, tolerance * max(1, abs(a), abs(b))))
  }
  normalize <- function(x) sort(tolower(gsub("[[:space:]]+", " ", trimws(as.character(x)))))
  identical(normalize(a), normalize(b))
}

.library_claim_impact <- function(field) {
  if (grepl("^(model_present|structural|theta|omega|sigma|covariates|residual_error|population_details|dosing|reproduction_targets)", field)) "major" else "minor"
}

#' Compare text- and vision-lane model claims field by field
#' @param text Text-lane result.
#' @param vision Vision-lane result.
#' @param tolerance Relative tolerance for numeric claims.
#' @return Reconciliation report.
#' @export
ingest_compare_extractions <- function(text, vision, tolerance = 0.02) {
  if (!isTRUE(text$available) || !isTRUE(vision$available)) {
    return(list(comparable = FALSE, consistent = FALSE, agreement = 0,
                differences = list(), missing_lanes = c(if (!isTRUE(text$available)) "text", if (!isTRUE(vision$available)) "vision")))
  }
  a <- .library_extraction_claims(text); b <- .library_extraction_claims(vision)
  fields <- sort(unique(c(names(a), names(b))))
  equal <- vapply(fields, function(field) .library_claim_equal(a[[field]], b[[field]], tolerance), logical(1))
  differences <- lapply(fields[!equal], function(field) list(
    field = field, text = a[[field]], vision = b[[field]], impact = .library_claim_impact(field)
  ))
  present <- vapply(fields, function(field) !is.null(.library_claim_value(a[[field]])) ||
                      !is.null(.library_claim_value(b[[field]])), logical(1))
  denominator <- max(1L, sum(present))
  agreement <- sum(equal & present) / denominator
  major <- sum(vapply(differences, function(x) identical(x$impact, "major"), logical(1)))
  list(comparable = TRUE, consistent = !length(differences), agreement = agreement,
       compared_fields = sum(present), difference_count = length(differences),
       major_difference_count = major, differences = differences)
}

.library_lane_confidence <- function(lane) {
  value <- suppressWarnings(as.numeric(unlist(lane$result$recoverability$overall %||%
                                                lane$result$extraction$confidence$overall %||% 0)))
  value <- value[is.finite(value)]
  if (length(value)) value[[1L]] else 0
}

.library_best_lane <- function(text, vision) {
  available <- Filter(function(x) isTRUE(x$available), list(text = text, vision = vision))
  if (!length(available)) return(NULL)
  confidence <- vapply(available, .library_lane_confidence, numeric(1))
  best <- which(confidence == max(confidence, na.rm = TRUE))
  available[[if (length(best)) best[[1L]] else 1L]]
}

#' Adjudicate discrepancies using a third configured LLM role
#' @param metadata Publication metadata.
#' @param text Text-lane result.
#' @param vision Vision-lane result.
#' @param comparison Field comparison.
#' @param bundle Document bundle.
#' @param cfg LibeRary configuration.
#' @return Adjudication result, or an unavailable result with an error.
#' @export
ingest_adjudicate_extractions <- function(metadata, text, vision, comparison, bundle, cfg = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  if (!isTRUE(comparison$comparable) || !length(comparison$differences)) {
    return(list(available = FALSE, error = "No comparable discrepancies require adjudication."))
  }
  endpoint <- .library_llm_role(cfg, "adjudication")
  max_chars <- endpoint$max_pdf_chars %||% cfg$ollama$max_pdf_chars %||% 120000L
  evidence <- .library_bundle_text(bundle, max_chars)
  prompt <- paste0(
    "Resolve the listed conflicts against the publication.\n\nTEXT LANE:\n",
    jsonlite::toJSON(text$result, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    "\n\nVISION LANE:\n",
    jsonlite::toJSON(vision$result, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    "\n\nFIELD COMPARISON:\n",
    jsonlite::toJSON(comparison, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    "\n\nSTANDARD-PARSER EVIDENCE:\n", evidence
  )
  paths <- as.character(unlist(bundle$vision$image_paths %||% character()))
  paths <- paths[file.exists(paths)]
  result <- tryCatch({
    user <- if (length(paths)) {
      pages <- as.integer(unlist(bundle$vision$page_numbers %||% seq_along(paths)))
      library_image_message(prompt, paths, paste("Original PDF page", pages), detail = "high")
    } else list(role = "user", content = prompt)
    structured <- .library_structured_chat(
      list(list(role = "system", content = .library_role_instruction(cfg, "adjudication")), user), cfg,
      role = "adjudication", schema = .library_adjudication_schema(), sensitive = TRUE
    )
    if (!isTRUE(structured$ok)) stop(structured$error, call. = FALSE)
    parsed <- structured$value
    if (is.list(parsed$resolved_extraction)) {
      parsed$resolved_extraction <- library_model_enrich(parsed$resolved_extraction)
    }
    list(available = TRUE, result = parsed,
         audit = .library_lane_audit(structured, prompt, "adjudication", bundle))
  }, error = identity)
  if (inherits(result, "error")) list(
    available = FALSE, error = conditionMessage(result),
    audit = if (exists("structured", inherits = FALSE)) .library_lane_audit(structured, prompt, "adjudication", bundle) else NULL
  ) else result
}

#' Run independent text and vision extraction with automated reconciliation
#'
#' Consistent records are labelled `machine_consistent`. Conflicts are sent to
#' the adjudication role and labelled `machine_adjudicated` unless a major field
#' remains unresolved. Machine agreement is deliberately not called human
#' validation.
#'
#' @param metadata Publication metadata.
#' @param bundle Document bundle or path to one.
#' @param cfg LibeRary configuration.
#' @param adjudicate Automatically adjudicate discrepancies.
#' @param progress Optional callback for progress within this article, accepting
#'   `value`, `message`, and `stage`.
#' @return Reconciled extraction, status, lane outputs, and audit data.
#' @export
ingest_dual_extract <- function(metadata, bundle, cfg = NULL, adjudicate = TRUE,
                                progress = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  if (is.character(bundle) && length(bundle) == 1L) bundle <- ingest_read_document_bundle(bundle)
  stage <- function(value, message, name) {
    if (!is.null(progress)) progress(value, message, name)
  }
  endpoint_label <- function(role) {
    endpoint <- .library_llm_role(cfg, role)
    paste(endpoint$provider, endpoint$model %||% "automatic model", sep = "/")
  }
  runtime_label <- function(lane) {
    processor <- lane$audit$runtime$processor %||% ""
    if (nzchar(processor)) paste0(" \u2014 ", processor) else ""
  }
  text_label <- if (isTRUE(cfg$deliberative$enabled)) "Evidence-led investigation" else "Parsed-text extraction"
  stage(0.03, paste0(text_label, ": ", endpoint_label("indexing"), " (LLM inference)"), "text_extraction")
  text <- ingest_extract_text_lane(
    metadata, bundle, cfg,
    progress = function(value, message, name) {
      stage(0.03 + 0.53 * value, message, paste0("text_", name))
    }
  )
  stage(0.57, paste0(text_label, " complete", runtime_label(text)), "text_extraction")
  stage(0.59, paste0("PDF vision extraction: ", endpoint_label("vision"), " (LLM inference)"), "vision_extraction")
  vision <- ingest_extract_vision_lane(metadata, bundle, cfg)
  stage(0.76, paste0("PDF vision extraction complete", runtime_label(vision)), "vision_extraction")
  stage(0.78, "Comparing text and vision claims (CPU)", "reconciliation")
  comparison <- ingest_compare_extractions(text, vision)
  text_endpoint <- .library_llm_role(cfg, "indexing")
  vision_endpoint <- .library_llm_role(cfg, "vision")
  independent_models <- !identical(
    paste(text_endpoint$provider, text_endpoint$model, sep = "|"),
    paste(vision_endpoint$provider, vision_endpoint$model, sep = "|")
  )
  if (isTRUE(cfg$llm$require_independent_extraction_models) && !independent_models) {
    stop("Text and vision extraction roles must use different provider/model combinations.", call. = FALSE)
  }
  warning <- if (independent_models) "" else
    "Text and vision lanes use the same provider/model; input modalities are independent but model errors may be correlated."

  best <- .library_best_lane(text, vision)
  if (is.null(best)) {
    stage(1, "No valid extraction lane; article requires review", "complete")
    return(list(status = "needs_review", model_present = NA, extraction = NULL,
                text = text, vision = vision, comparison = comparison,
                adjudication = NULL, warning = warning,
                audit = list(
                  schema_version = LIBRARY_SCHEMA_VERSION,
                  prompt_version = LIBRARY_PROMPT_VERSION,
                  source_sha256 = bundle$source$sha256 %||% "",
                  text = text$audit %||% list(error = text$error %||% ""),
                  vision = vision$audit %||% list(error = vision$error %||% ""),
                  comparison = comparison,
                  adjudication = list(error = "No valid extraction lane was available."),
                  independent_models = independent_models,
                  warning = warning
                )))
  }
  extraction <- best$result$extraction
  text_present <- if (isTRUE(text$available)) isTRUE(text$result$model_present) else NA
  vision_present <- if (isTRUE(vision$available)) isTRUE(vision$result$model_present) else NA
  model_present <- if (!is.na(text_present) && !is.na(vision_present) &&
                       identical(text_present, vision_present)) text_present else
    if (isTRUE(best$result$model_present)) TRUE else NA
  status <- "needs_review"
  adjudication <- NULL
  if (isTRUE(comparison$comparable) && !text_present && !vision_present) {
    status <- "machine_consistent"
    model_present <- FALSE
  } else if (isTRUE(comparison$comparable) && isTRUE(comparison$consistent)) {
    status <- "machine_consistent"
    model_present <- isTRUE(text$result$model_present) && isTRUE(vision$result$model_present)
  } else if (isTRUE(adjudicate) && isTRUE(comparison$comparable)) {
    stage(0.81, paste0("Discrepancy adjudication: ", endpoint_label("adjudication"),
                       " (LLM inference)"), "adjudication")
    adjudication <- ingest_adjudicate_extractions(metadata, text, vision, comparison, bundle, cfg)
    if (isTRUE(adjudication$available)) {
      extraction <- adjudication$result$resolved_extraction
      model_present <- isTRUE(adjudication$result$model_present)
      unresolved <- adjudication$result$unresolved_fields %||% list()
      major_unresolved <- any(vapply(unresolved, function(x) identical(x$impact, "major"), logical(1)))
      status <- if (major_unresolved) "needs_review" else "machine_adjudicated"
    }
    stage(0.95, paste0("Adjudication complete", runtime_label(adjudication)), "adjudication")
  }
  checks <- text$audit$deterministic_checks %||% text$checks %||% NULL
  if (isTRUE(cfg$deliberative$enabled) && isTRUE(model_present) &&
      is.list(checks) && !isTRUE(checks$ready)) {
    status <- "needs_review"
    warning <- paste(Filter(nzchar, c(
      warning,
      "The evidence-led investigation did not pass all deterministic completeness and consistency gates."
    )), collapse = " ")
  }
  stage(1, "Extraction and reconciliation complete", "complete")
  list(
    status = status,
    model_present = model_present,
    extraction = extraction,
    text = text,
    vision = vision,
    comparison = comparison,
    adjudication = adjudication,
    independent_models = independent_models,
    warning = warning,
    audit = list(
      schema_version = LIBRARY_SCHEMA_VERSION,
      prompt_version = LIBRARY_PROMPT_VERSION,
      source_sha256 = bundle$source$sha256 %||% "",
      text = text$audit %||% list(error = text$error %||% ""),
      vision = vision$audit %||% list(error = vision$error %||% ""),
      comparison = comparison,
      adjudication = adjudication$audit %||% list(error = adjudication$error %||% ""),
      independent_models = independent_models,
      warning = warning
    )
  )
}
