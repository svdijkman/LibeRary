DELIBERATIVE_RECONNAISSANCE_PROMPT <- paste(
  "You are the reconnaissance investigator for a pharmacometric evidence review.",
  "Do not attempt to write the final model. Map the models discussed in the article,",
  "distinguish base, intermediate, final, validation, and externally inherited models,",
  "record only source-supported anchor facts, and formulate targeted questions for",
  "later investigators. Treat early structural interpretations as hypotheses rather",
  "than facts. Every reported claim needs an exact chunk, page, table, figure, section,",
  "or equation locator and a short evidence excerpt. Return compact JSON only."
)

DELIBERATIVE_INVESTIGATION_PROMPT <- paste(
  "You are one specialist in a staged pharmacometric fact-finding investigation.",
  "Investigate only the assigned topic. Use the supplied evidence excerpts and current",
  "ledger as leads, not as authority. Distinguish the final model from base, candidate,",
  "validation, and externally inherited models. Never fill a gap with a plausible value.",
  "Record conflicting observations separately. Use canonical dotted field names, exact",
  "Assign every claim one domain: structure, theta, omega, sigma, covariates, population,",
  "dosing, reproduction, or other. Claim ids need only be unique within this response.",
  "source locators, short evidence excerpts, reported units and metrics. A reported claim",
  "must be directly visible in the excerpts; derived and inferred claims must name their",
  "dependencies. Return compact JSON only."
)

DELIBERATIVE_REVIEW_PROMPT <- paste(
  "You are the skeptical reviewer in a pharmacometric evidence investigation.",
  "Try to falsify important ledger claims rather than endorsing a coherent-looking model.",
  "Look especially for base-versus-final model confusion, values inherited from another",
  "publication, fixed versus estimated parameters, uncertainty mistaken for variability,",
  "OMEGA/SIGMA scale errors, ETA distribution errors, and conflicting cohort definitions.",
  "Mark an existing question id resolved only when the evidence ledger or supplied source",
  "actually answers it. If evidence is insufficient, return a precise follow-up search",
  "question. Return JSON only."
)

DELIBERATIVE_SYNTHESIS_PROMPT <- paste(
  "You synthesize a final pharmacometric record from an audited evidence ledger.",
  "Use only claims present in the ledger. Prefer directly reported final-model evidence,",
  "then deterministic derivations, and only then explicitly labelled inferences. Never",
  "copy a base-model, validation, bootstrap, or external-model value into the final model.",
  "When a major conflict remains unresolved, use null or [] and describe the limitation.",
  "OMEGA and SIGMA value fields must contain NONMEM variances while reported_value and",
  "reported_metric retain the publication scale. Complete every schema field compactly.",
  "Never turn narrative findings into demographic descriptors or duplicate the same fact.",
  "Use at most 20 short field_evidence records for the most material final-model fields,",
  "at most 12 cohorts, and at most 30 distinct descriptors across the entire population."
)

.library_deliberative_instruction <- function(cfg, role, stage_prompt, synthesis = FALSE) {
  override <- trimws(as.character(cfg$llm[[role]]$instruction %||% "")[[1L]])
  # The historical role defaults request the final one-shot schema and conflict
  # with reconnaissance or falsification. Use them only for final synthesis;
  # explicit user instructions remain active for every stage assigned to a role.
  base <- if (nzchar(override)) override else if (isTRUE(synthesis)) {
    .library_role_instruction(cfg, role)
  } else ""
  paste(Filter(nzchar, c(base, stage_prompt)), collapse = "\n\n")
}

.library_evidence_claim_schema <- function(max_items = NULL) {
  item <- list(
    type = "object", additionalProperties = FALSE,
    properties = list(
      id = list(type = "string"),
      domain = list(type = "string", enum = c(
        "structure", "theta", "omega", "sigma", "covariates",
        "population", "dosing", "reproduction", "other"
      )),
      field = list(type = "string"),
      value = list(type = c("string", "null")),
      unit = list(type = c("string", "null")),
      status = list(type = "string", enum = c("reported", "derived", "inferred", "unresolved")),
      model_stage = list(type = "string", enum = c(
        "base", "intermediate", "final", "validation", "bootstrap", "external", "unknown"
      )),
      source_locator = list(type = "string"),
      evidence = list(type = "string"),
      confidence = list(type = "number", minimum = 0, maximum = 1),
      dependencies = list(type = "array", items = list(type = "string")),
      alternatives = list(type = "array", items = list(type = "string"))
    ),
    required = c("id", "domain", "field", "value", "unit", "status", "model_stage",
                 "source_locator", "evidence", "confidence", "dependencies", "alternatives")
  )
  out <- list(type = "array", items = item)
  if (!is.null(max_items)) out$maxItems <- as.integer(max_items)
  out
}

.library_investigation_question_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    id = list(type = "string"), field = list(type = "string"),
    question = list(type = "string"),
    priority = list(type = "string", enum = c("low", "medium", "high", "critical")),
    search_terms = list(type = "array", items = list(type = "string"))
  ), required = c("id", "field", "question", "priority", "search_terms")
)

.library_reconnaissance_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    model_present = list(type = "boolean"),
    model_probability = list(type = "number", minimum = 0, maximum = 1),
    model_inventory = list(type = "array", maxItems = 20L, items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        id = list(type = "string"), label = list(type = "string"),
        role = list(type = "string", enum = c(
          "base", "intermediate", "final", "validation", "external", "unknown"
        )),
        compound = list(type = c("string", "null")),
        endpoint = list(type = c("string", "null")),
        source_locator = list(type = "string"), evidence = list(type = "string"),
        confidence = list(type = "number", minimum = 0, maximum = 1)
      ), required = c("id", "label", "role", "compound", "endpoint",
                      "source_locator", "evidence", "confidence")
    )),
    anchor_claims = .library_evidence_claim_schema(40L),
    hypotheses = list(type = "array", maxItems = 20L, items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        id = list(type = "string"), description = list(type = "string"),
        status = list(type = "string", enum = c("candidate", "supported", "rejected", "unresolved")),
        confidence = list(type = "number", minimum = 0, maximum = 1),
        supporting_claim_ids = list(type = "array", items = list(type = "string")),
        contradicting_claim_ids = list(type = "array", items = list(type = "string"))
      ), required = c("id", "description", "status", "confidence",
                      "supporting_claim_ids", "contradicting_claim_ids")
    )),
    investigation_questions = list(type = "array", maxItems = 30L,
                                   items = .library_investigation_question_schema()),
    relevant_sections = list(type = "array", maxItems = 30L, items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(topic = list(type = "string"), source_locator = list(type = "string"),
                        reason = list(type = "string")),
      required = c("topic", "source_locator", "reason")
    )),
    referenced_sources = list(type = "array", maxItems = 20L, items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        kind = list(type = "string"), citation = list(type = "string"),
        reason = list(type = "string"), required_for_reconstruction = list(type = "boolean")
      ), required = c("kind", "citation", "reason", "required_for_reconstruction")
    ))
  ),
  required = c("model_present", "model_probability", "model_inventory", "anchor_claims",
               "hypotheses", "investigation_questions", "relevant_sections", "referenced_sources")
)

.library_investigation_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    topic = list(type = "string"),
    claims = .library_evidence_claim_schema(60L),
    resolved_question_ids = list(type = "array", items = list(type = "string")),
    new_questions = list(type = "array", maxItems = 20L,
                         items = .library_investigation_question_schema()),
    contradictions = list(type = "array", maxItems = 20L, items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        claim_id = list(type = c("string", "null")), description = list(type = "string"),
        source_locator = list(type = "string"), evidence = list(type = "string"),
        severity = list(type = "string", enum = c("minor", "major"))
      ), required = c("claim_id", "description", "source_locator", "evidence", "severity")
    )),
    coverage = list(type = "number", minimum = 0, maximum = 1),
    summary = list(type = "string")
  ), required = c("topic", "claims", "resolved_question_ids", "new_questions",
                  "contradictions", "coverage", "summary")
)

.library_falsification_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    verdicts = list(type = "array", maxItems = 100L, items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        claim_id = list(type = "string"),
        verdict = list(type = "string", enum = c("supported", "contradicted", "uncertain")),
        corrected_value = list(type = c("string", "null")),
        source_locator = list(type = "string"), evidence = list(type = "string"),
        reason = list(type = "string")
      ), required = c("claim_id", "verdict", "corrected_value", "source_locator", "evidence", "reason")
    )),
    missing_critical = list(type = "array", maxItems = 30L,
                            items = .library_investigation_question_schema()),
    follow_up_queries = list(type = "array", maxItems = 30L,
                             items = .library_investigation_question_schema()),
    resolved_question_ids = list(type = "array", maxItems = 100L,
                                 items = list(type = "string")),
    overall_ready = list(type = "boolean"),
    summary = list(type = "string")
  ), required = c("verdicts", "missing_critical", "follow_up_queries",
                  "resolved_question_ids", "overall_ready", "summary")
)

.library_deliberative_text <- function(bundle, cfg) {
  path <- as.character(bundle$parser$markdown_path %||% "")[[1L]]
  if (!nzchar(path) || !file.exists(path)) return("")
  text <- .library_read_text_utf8(path)
  # Deliberative retrieval sends only selected chunks to each LLM stage, so it
  # can search substantially more of the source than the legacy one-shot lane.
  limit <- suppressWarnings(as.integer(cfg$deliberative$max_document_chars %||% 500000L))
  if (is.finite(limit) && limit > 0L && nchar(text) > limit) {
    text <- substr(text, 1L, limit)
  }
  text
}

.library_document_chunks <- function(text, chunk_chars = 4200L, overlap = 350L) {
  text <- .library_normalize_utf8(as.character(text %||% ""))[[1L]]
  text <- gsub("\r\n?", "\n", text)
  if (!nzchar(trimws(text))) return(list())
  chunk_chars <- max(500L, as.integer(chunk_chars))
  overlap <- max(0L, min(as.integer(overlap), chunk_chars %/% 3L))
  starts <- seq.int(1L, nchar(text), by = max(1L, chunk_chars - overlap))
  chunks <- vector("list", length(starts))
  heading_pattern <- "(?m)^#{1,6}[[:space:]]+.*$"
  page_pattern <- "(?im)^(?:#{1,6}[[:space:]]*)?[[]?page[[:space:]]+[0-9]+[]]?[[:space:]]*$"
  for (index in seq_along(starts)) {
    start <- starts[[index]]
    end <- min(nchar(text), start + chunk_chars - 1L)
    excerpt <- .library_normalize_utf8(substr(text, start, end))[[1L]]
    prefix <- .library_normalize_utf8(
      substr(text, max(1L, start - 12000L), start)
    )[[1L]]
    headings <- regmatches(prefix, gregexpr(heading_pattern, prefix, perl = TRUE))[[1L]]
    pages <- regmatches(prefix, gregexpr(page_pattern, prefix, perl = TRUE))[[1L]]
    heading <- if (length(headings) && !identical(headings[[1L]], "")) utils::tail(headings, 1L) else ""
    page <- if (length(pages) && !identical(pages[[1L]], "")) utils::tail(pages, 1L) else ""
    locator <- paste(Filter(nzchar, c(page, sub("^#+[[:space:]]*", "", heading),
                                      sprintf("characters %d-%d", start, end))), collapse = "; ")
    chunks[[index]] <- list(
      id = sprintf("C%03d", index), locator = locator,
      start = start, end = end, text = excerpt
    )
    if (end >= nchar(text)) {
      chunks <- chunks[seq_len(index)]
      break
    }
  }
  chunks
}

.library_keyword_count <- function(text, keyword) {
  # Boundaries prevent short pharmacometric terms such as ETA from matching
  # ordinary words such as "detail" or "metadata".
  pattern <- paste0(
    "(?<![[:alnum:]_])\\Q", tolower(as.character(keyword)), "\\E(?![[:alnum:]_])"
  )
  text <- .library_normalize_utf8(text)[[1L]]
  # PCRE performs Unicode-aware case folding directly. Avoid base::tolower()
  # on a complete article chunk: the Windows implementation can reject valid
  # UTF-8 containing characters outside the active system locale.
  hits <- gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE)[[1L]]
  if (length(hits) == 1L && hits[[1L]] < 0L) 0L else length(hits)
}

.library_retrieve_chunks <- function(chunks, keywords, limit = 8L) {
  if (!length(chunks)) return(list())
  keywords <- unique(tolower(trimws(as.character(unlist(keywords %||% character())))))
  keywords <- keywords[nzchar(keywords)]
  scores <- vapply(chunks, function(chunk) {
    sum(vapply(keywords, function(keyword) .library_keyword_count(chunk$text, keyword), integer(1)))
  }, numeric(1))
  # The beginning normally contains study identity and population anchors.
  scores[[1L]] <- scores[[1L]] + 0.25
  order_index <- order(scores, decreasing = TRUE, seq_along(scores))
  take <- utils::head(order_index, min(as.integer(limit), length(order_index)))
  chunks[sort(take)]
}

.library_format_chunks <- function(chunks) {
  paste(vapply(chunks, function(chunk) paste0(
    "=== ", chunk$id, " | ", chunk$locator, " ===\n", chunk$text
  ), character(1)), collapse = "\n\n")
}

.library_deliberative_topics <- function() list(
  structure = list(
    keywords = c("structural model", "compartment", "clearance", "volume", "absorption",
                 "elimination", "equation", "advan", "trans", "final model", "base model"),
    instruction = paste(
      "Establish model inventory and final-model structure: route, compartments, input,",
      "elimination, parameterization, equations, delays and nonlinearities. Record ADVAN/TRANS",
      "as reported only; otherwise create an explicitly inferred hypothesis with alternatives."
    )
  ),
  theta_covariates = list(
    keywords = c("parameter estimate", "typical value", "theta", "fixed", "covariate",
                 "allometric", "centred", "centered", "bootstrap", "relative standard error"),
    instruction = paste(
      "Find final-model THETAs and covariate equations. Preserve names, values, units, fixed",
      "status, uncertainty, centering and exponents. Separate base, final, validation and",
      "bootstrap columns. Use fields such as parameters.theta.CL.typical and covariates.WT.CL."
    )
  ),
  omega_iov = list(
    keywords = c("omega", "interindividual", "between-subject", "variability", "eta",
                 "interoccasion", "iov", "covariance", "correlation", "shrinkage"),
    instruction = paste(
      "Find ETA-to-parameter assignments, normal versus exponential equations, IIV/IOV,",
      "OMEGA variances, SDs, CVs and covariance blocks. Keep reported metric separate from",
      "converted variance. Do not treat shrinkage, RSE or bootstrap precision as OMEGA."
    )
  ),
  sigma_observation = list(
    keywords = c("sigma", "residual error", "proportional error", "additive error",
                 "combined error", "observation model", "blq", "likelihood", "residual variability"),
    instruction = paste(
      "Find the observation and residual-error model for every endpoint, including scale,",
      "additive/proportional components, reported metric, BLQ handling and transformations.",
      "Use an explicit .none claim if the article states that a component was absent."
    )
  ),
  population_dosing = list(
    keywords = c("patients", "subjects", "demographic", "age", "weight", "sex", "cyp",
                 "genotype", "phenotype", "dose", "dosing", "infusion", "administration"),
    instruction = paste(
      "Find each cohort, its role and sample count, demographics with reported summary form,",
      "organ function and CYP records, plus dose regimens, routes, intervals and infusion",
      "durations. Do not merge development and validation populations."
    )
  ),
  reproduction_data = list(
    keywords = c("figure", "table", "concentration", "time", "observed", "prediction",
                 "visual predictive", "goodness-of-fit", "simulation", "external validation"),
    instruction = paste(
      "Inventory tables and figures that can test the reconstructed model. Record digitizable",
      "concentration-time or distribution data, axes, units, statistic, uncertainty and cohort.",
      "Do not invent points that cannot be read reliably."
    )
  )
)

.library_messages_text <- function(messages) {
  pieces <- character()
  for (message in messages) {
    content <- message$content %||% ""
    if (is.list(content)) {
      content <- vapply(content, function(block) {
        if (identical(block$type %||% "", "text")) as.character(block$text %||% "") else "[image]"
      }, character(1))
    }
    pieces <- c(pieces, paste(content, collapse = "\n"))
  }
  paste(pieces, collapse = "\n\n")
}

.library_investigation_cache_path <- function(bundle, stage) {
  directory <- file.path(bundle$bundle_path, "investigation", "stages")
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  file.path(directory, paste0(gsub("[^A-Za-z0-9_.-]", "_", stage), ".json"))
}

.library_investigation_stage <- function(stage, messages, schema, cfg, role, bundle,
                                         cache = TRUE, force = FALSE,
                                         chat = library_llm_chat) {
  endpoint <- .library_llm_role(cfg, role)
  fingerprint <- digest::digest(list(
    source_sha256 = bundle$source$sha256 %||% "",
    prompt_version = LIBRARY_PROMPT_VERSION,
    stage = stage, provider = endpoint$provider, model = endpoint$model,
    instruction_override = cfg$llm[[role]]$instruction %||% "",
    messages = messages, schema = schema
  ), algo = "sha256", serialize = TRUE)
  path <- .library_investigation_cache_path(bundle, stage)
  if (isTRUE(cache) && !isTRUE(force) && file.exists(path)) {
    saved <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
    valid <- !is.null(saved) && identical(saved$fingerprint %||% "", fingerprint) &&
      is.list(saved$result) && isTRUE(tryCatch({
        .library_validate_structured_value(saved$result, schema); TRUE
      }, error = function(e) FALSE))
    if (valid) {
      audit <- saved$audit %||% list()
      audit$cached <- TRUE
      audit$cache_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      return(list(value = saved$result, audit = audit, cached = TRUE, path = path))
    }
  }
  stage_started <- proc.time()[["elapsed"]]
  structured <- .library_structured_chat(
    messages, cfg, role = role, schema = schema, sensitive = TRUE, chat = chat
  )
  stage_elapsed <- unname(proc.time()[["elapsed"]] - stage_started)
  failure_path <- sub("[.]json$", ".failure.json", path)
  if (!isTRUE(structured$ok)) {
    .library_atomic_write(list(
      schema_version = LIBRARY_SCHEMA_VERSION,
      prompt_version = LIBRARY_PROMPT_VERSION,
      fingerprint = fingerprint, stage = stage,
      failed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      elapsed_seconds = stage_elapsed,
      error = structured$error, attempts = structured$attempts %||% list()
    ), failure_path)
    stop("Investigation stage `", stage, "` failed: ", structured$error,
         " Failure audit: ", normalizePath(failure_path, winslash = "/", mustWork = TRUE),
         call. = FALSE)
  }
  response <- structured$response %||% list()
  audit <- unclass(response)
  audit$response_elapsed_seconds <- audit$elapsed_seconds %||% NULL
  audit$elapsed_seconds <- stage_elapsed
  audit$stage <- stage
  audit$role <- role
  audit$provider <- response$provider %||% endpoint$provider
  audit$model <- response$model %||% endpoint$model
  audit$prompt_version <- LIBRARY_PROMPT_VERSION
  audit$prompt_md5 <- .library_text_fingerprint(.library_messages_text(messages))
  audit$retry_count <- structured$retry_count %||% 0L
  audit$attempts <- structured$attempts %||% list()
  audit$source_sha256 <- bundle$source$sha256 %||% ""
  audit$cached <- FALSE
  envelope <- list(
    schema_version = LIBRARY_SCHEMA_VERSION,
    prompt_version = LIBRARY_PROMPT_VERSION,
    fingerprint = fingerprint, stage = stage,
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    result = structured$value, audit = audit
  )
  .library_atomic_write(envelope, path)
  if (file.exists(failure_path)) unlink(failure_path, force = TRUE)
  audit$cache_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  list(value = structured$value, audit = audit, cached = FALSE, path = path)
}

.library_new_evidence_ledger <- function(metadata, bundle, reconnaissance) {
  list(
    schema_version = LIBRARY_SCHEMA_VERSION,
    prompt_version = LIBRARY_PROMPT_VERSION,
    source = list(
      pmid = metadata$pmid %||% "", doi = metadata$doi %||% "",
      title = metadata$title %||% "", sha256 = bundle$source$sha256 %||% ""
    ),
    model_present = reconnaissance$model_present %||% NA,
    model_probability = reconnaissance$model_probability %||% 0,
    model_inventory = reconnaissance$model_inventory %||% list(),
    claims = list(), hypotheses = reconnaissance$hypotheses %||% list(),
    questions = list(),
    contradictions = list(), referenced_sources = reconnaissance$referenced_sources %||% list(),
    stage_summaries = list(), reviews = list()
  )
}

.library_claim_id <- function(claim, stage) {
  supplied <- trimws(as.character(claim$id %||% "")[[1L]])
  if (nzchar(supplied)) {
    prefix <- paste0(stage, "::")
    return(if (startsWith(supplied, prefix)) supplied else paste0(prefix, supplied))
  }
  paste0(stage, "_", substr(digest::digest(list(
    claim$field, claim$value, claim$model_stage, claim$source_locator
  ), algo = "sha256"), 1L, 12L))
}

.library_ledger_add_claims <- function(ledger, claims, stage) {
  claims <- claims %||% list()
  supplied_ids <- vapply(claims, function(claim) {
    trimws(as.character(claim$id %||% "")[[1L]])
  }, character(1))
  namespaced_ids <- vapply(claims, .library_claim_id, character(1), stage = stage)
  id_map <- stats::setNames(namespaced_ids[nzchar(supplied_ids)], supplied_ids[nzchar(supplied_ids)])
  for (claim in claims %||% list()) {
    if (!is.list(claim)) next
    claim$id <- .library_claim_id(claim, stage)
    dependencies <- as.character(unlist(claim$dependencies %||% character()))
    claim$dependencies <- lapply(dependencies, function(id) {
      if (id %in% names(id_map)) unname(id_map[[id]]) else id
    })
    claim$investigation_stage <- stage
    duplicate <- vapply(ledger$claims, function(existing) {
      identical(existing$id %||% "", claim$id) ||
        identical(digest::digest(existing[c("field", "value", "model_stage", "source_locator")]),
                  digest::digest(claim[c("field", "value", "model_stage", "source_locator")]))
    }, logical(1))
    if (!any(duplicate)) ledger$claims[[length(ledger$claims) + 1L]] <- claim
  }
  ledger
}

.library_ledger_add_questions <- function(ledger, questions, stage) {
  for (question in questions %||% list()) {
    if (!is.list(question)) next
    if (!nzchar(trimws(as.character(question$id %||% "")[[1L]]))) {
      question$id <- paste0("Q_", substr(digest::digest(list(question$field, question$question)), 1L, 12L))
    }
    prefix <- paste0(stage, "::")
    if (!startsWith(question$id, prefix) && !grepl("::", question$id, fixed = TRUE)) {
      question$id <- paste0(prefix, question$id)
    }
    question$investigation_stage <- stage
    key <- tolower(paste(question$field %||% "", question$question %||% "", sep = "|"))
    existing <- vapply(ledger$questions, function(item) {
      identical(tolower(paste(item$field %||% "", item$question %||% "", sep = "|")), key)
    }, logical(1))
    if (!any(existing)) ledger$questions[[length(ledger$questions) + 1L]] <- question
  }
  ledger
}

.library_ledger_merge_investigation <- function(ledger, result, stage) {
  ledger <- .library_ledger_add_claims(ledger, result$claims %||% list(), stage)
  ledger <- .library_ledger_add_questions(ledger, result$new_questions %||% list(), stage)
  ledger$contradictions <- c(ledger$contradictions, lapply(result$contradictions %||% list(), function(x) {
    x$investigation_stage <- stage; x
  }))
  resolved <- as.character(unlist(result$resolved_question_ids %||% character()))
  if (length(resolved)) {
    ledger$questions <- Filter(function(question) !(question$id %||% "") %in% resolved, ledger$questions)
  }
  ledger$stage_summaries[[stage]] <- list(
    coverage = result$coverage %||% 0, summary = result$summary %||% "",
    claim_count = length(result$claims %||% list()),
    contradiction_count = length(result$contradictions %||% list())
  )
  ledger
}

.library_compact_ledger <- function(ledger, max_chars = 24000L) {
  compact_claim <- function(claim) list(
    id = claim$id, domain = claim$domain %||% "other",
    field = claim$field, value = claim$value, unit = claim$unit,
    status = claim$status, model_stage = claim$model_stage,
    superseded_by = claim$superseded_by %||% NULL,
    source_locator = substr(claim$source_locator %||% "", 1L, 180L),
    evidence = substr(claim$evidence %||% "", 1L, 240L),
    confidence = claim$confidence, dependencies = utils::head(claim$dependencies %||% list(), 6L),
    alternatives = utils::head(claim$alternatives %||% list(), 4L)
  )
  claims <- lapply(ledger$claims %||% list(), compact_claim)
  confidence <- vapply(claims, function(x) suppressWarnings(as.numeric(x$confidence %||% 0)), numeric(1))
  confidence[!is.finite(confidence)] <- 0
  if (length(claims)) claims <- claims[order(confidence, decreasing = TRUE)]
  make <- function(current) list(
    source = ledger$source, model_present = ledger$model_present,
    model_probability = ledger$model_probability, model_inventory = ledger$model_inventory,
    claims = current, hypotheses = ledger$hypotheses,
    questions = ledger$questions, contradictions = ledger$contradictions,
    referenced_sources = ledger$referenced_sources,
    omitted_claim_count = length(claims) - length(current)
  )
  current <- claims
  repeat {
    payload <- make(current)
    encoded <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA)
    if (nchar(encoded) <= max_chars || length(current) <= 12L) return(encoded)
    current <- utils::head(current, max(12L, floor(length(current) * 0.85)))
  }
}

.library_review_search_terms <- function(review) {
  questions <- c(review$missing_critical %||% list(), review$follow_up_queries %||% list())
  unique(as.character(unlist(lapply(questions, function(item) c(
    item$field %||% "", item$question %||% "", item$search_terms %||% character()
  )))))
}

.library_open_question_search_terms <- function(ledger) {
  questions <- Filter(function(item) {
    (item$priority %||% "") %in% c("critical", "high")
  }, ledger$questions %||% list())
  unique(as.character(unlist(lapply(questions, function(item) c(
    item$field %||% "", item$question %||% "", item$search_terms %||% character()
  )))))
}

.library_ledger_apply_review <- function(ledger, review, stage) {
  ledger$reviews[[stage]] <- review
  for (verdict in review$verdicts %||% list()) {
    id <- as.character(verdict$claim_id %||% "")[[1L]]
    match_index <- which(vapply(ledger$claims, function(x) identical(x$id %||% "", id), logical(1)))
    if (!length(match_index)) next
    original <- ledger$claims[[match_index[[1L]]]]
    if (identical(verdict$verdict, "contradicted")) {
      corrected <- verdict$corrected_value %||% NULL
      corrected_available <- !is.null(corrected) &&
        nzchar(trimws(as.character(corrected)[[1L]]))
      ledger$contradictions[[length(ledger$contradictions) + 1L]] <- list(
        claim_id = id, description = verdict$reason %||% "Contradicted during review",
        source_locator = verdict$source_locator %||% "", evidence = verdict$evidence %||% "",
        severity = "major", resolved = corrected_available, investigation_stage = stage
      )
      if (corrected_available) {
        replacement <- original
        replacement$id <- paste0(id, "_correction_", substr(digest::digest(corrected), 1L, 8L))
        replacement$value <- as.character(corrected)[[1L]]
        replacement$status <- "reported"
        replacement$source_locator <- verdict$source_locator %||% ""
        replacement$evidence <- verdict$evidence %||% ""
        replacement$confidence <- max(0.7, suppressWarnings(as.numeric(original$confidence %||% 0)))
        replacement$dependencies <- list(id)
        replacement$investigation_stage <- stage
        ledger$claims[[match_index[[1L]]]]$superseded_by <- replacement$id
        ledger <- .library_ledger_add_claims(ledger, list(replacement), stage)
      }
    }
  }
  resolved <- as.character(unlist(review$resolved_question_ids %||% character()))
  if (length(resolved)) {
    ledger$questions <- Filter(function(question) {
      !(question$id %||% "") %in% resolved
    }, ledger$questions)
  }
  ledger <- .library_ledger_add_questions(
    ledger, c(review$missing_critical %||% list(), review$follow_up_queries %||% list()), stage
  )
  ledger
}

.library_ledger_consistency_checks <- function(ledger) {
  claims <- Filter(function(claim) !nzchar(as.character(claim$superseded_by %||% "")[[1L]]),
                   ledger$claims %||% list())
  fields <- tolower(vapply(claims, function(x) as.character(x$field %||% "")[[1L]], character(1)))
  claim_domains <- tolower(vapply(claims, function(x) as.character(x$domain %||% "")[[1L]], character(1)))
  domains <- list(
    structure = "structural", theta = "parameters.theta", omega = "parameters.omega",
    sigma = "parameters.sigma", covariates = "covariate", population = "population",
    dosing = "dosing", reproduction = "reproduction"
  )
  coverage <- vapply(names(domains), function(domain) {
    any(claim_domains == domain) || any(startsWith(fields, domains[[domain]]))
  }, logical(1))
  reported_missing_evidence <- vapply(claims, function(claim) {
    identical(claim$status %||% "", "reported") &&
      (!nzchar(trimws(as.character(claim$source_locator %||% "")[[1L]])) ||
       !nzchar(trimws(as.character(claim$evidence %||% "")[[1L]])))
  }, logical(1))
  group_keys <- vapply(claims, function(claim) paste(
    tolower(as.character(claim$field %||% "")[[1L]]),
    tolower(as.character(claim$model_stage %||% "unknown")[[1L]]), sep = "|"
  ), character(1))
  conflicts <- list()
  scalar_field <- function(claim) {
    domain <- tolower(as.character(claim$domain %||% "")[[1L]])
    field <- tolower(as.character(claim$field %||% "")[[1L]])
    pattern <- switch(domain,
      structure = "compartment|input|route|absorption|elimination|parameterization|advan|trans|structural",
      theta = "theta|typical|clearance|volume|absorption|bioavailability|lag|half[- ]?life|parameter estimate|(^|[^a-z])cl([^a-z]|$)|(^|[^a-z])ka([^a-z]|$)",
      omega = "omega|eta|iiv|iov|interindividual|interoccasion|variab|covariance|correlation",
      sigma = "sigma|residual|error|additive|proportional",
      covariates = "covariate|exponent|coefficient|centering|effect",
      ""
    )
    nzchar(pattern) && grepl(pattern, field, perl = TRUE)
  }
  for (key in unique(group_keys[nzchar(group_keys)])) {
    subset <- claims[group_keys == key]
    subset <- Filter(scalar_field, subset)
    # Population narratives and other broad findings are not scalar fields;
    # several complementary observations may legitimately share a heading.
    if (length(subset) < 2L) next
    values <- unique(tolower(trimws(vapply(subset, function(x) as.character(x$value %||% "")[[1L]], character(1)))))
    values <- values[nzchar(values)]
    if (length(values) > 1L) conflicts[[length(conflicts) + 1L]] <- list(
      field_stage = key, values = as.list(values), claim_ids = lapply(subset, `[[`, "id")
    )
  }
  final_identified <- any(vapply(ledger$model_inventory %||% list(), function(model) {
    identical(model$role %||% "", "final")
  }, logical(1))) || any(vapply(claims, function(claim) {
    identical(claim$model_stage %||% "", "final")
  }, logical(1)))
  major_contradictions <- Filter(function(item) {
    identical(item$severity %||% "major", "major") && !isTRUE(item$resolved)
  }, ledger$contradictions %||% list())
  critical_questions <- Filter(function(item) {
    (item$priority %||% "") %in% c("critical", "high")
  }, ledger$questions %||% list())
  reviews <- ledger$reviews %||% list()
  latest_review <- if (length(reviews)) reviews[[length(reviews)]] else NULL
  review_ready <- is.null(latest_review) || isTRUE(latest_review$overall_ready)
  errors <- character()
  if (length(conflicts)) errors <- c(errors, "Conflicting values remain for the same field and model stage.")
  if (any(reported_missing_evidence)) errors <- c(errors, "Reported claims without source evidence remain.")
  if (length(major_contradictions)) errors <- c(errors, "Major evidence contradictions remain unresolved.")
  if (length(critical_questions)) errors <- c(errors, "High-priority evidence questions remain unresolved.")
  if (!review_ready) errors <- c(errors, "The latest skeptical review did not find the record ready for synthesis.")
  warnings <- character()
  if (!final_identified) warnings <- c(warnings, "The final model has not been distinguished conclusively.")
  if (mean(coverage) < 0.75) warnings <- c(warnings, "Evidence coverage is incomplete across required domains.")
  list(
    ready = !length(errors) && mean(coverage) >= 0.75 && final_identified,
    coverage = as.list(coverage), coverage_fraction = unname(mean(coverage)),
    final_model_identified = final_identified,
    skeptical_review_ready = review_ready,
    unresolved_high_priority_questions = critical_questions,
    unresolved_major_contradictions = major_contradictions,
    reported_claims_without_evidence = as.list(which(reported_missing_evidence)),
    conflicting_claims = conflicts, errors = errors, warnings = warnings
  )
}

.library_stage_audit_summary <- function(audit) list(
  stage = audit$stage %||% "", role = audit$role %||% "",
  provider = audit$provider %||% "", model = audit$model %||% "",
  elapsed_seconds = audit$elapsed_seconds %||% NULL,
  runtime = audit$runtime %||% NULL, retry_count = audit$retry_count %||% 0L,
  cached = isTRUE(audit$cached), cache_path = audit$cache_path %||% ""
)

#' Run a staged evidence-led model investigation
#'
#' The article is mapped into searchable chunks, investigated by pharmacometric
#' topic, challenged by a skeptical reviewer, optionally re-searched for gaps,
#' checked deterministically, and only then synthesized into the standard
#' LibeRary extraction schema. Every stage is content-addressed and resumable.
#'
#' @param metadata Publication metadata.
#' @param bundle Document bundle or bundle path.
#' @param cfg LibeRary configuration.
#' @param progress Optional callback accepting `value`, `message`, and `stage`.
#' @param force Ignore reusable stage caches.
#' @param chat Chat function, primarily for deterministic testing.
#' @return A text-lane-compatible result with an evidence-led audit.
#' @export
ingest_deliberative_extract <- function(metadata, bundle, cfg = NULL, progress = NULL,
                                        force = FALSE, chat = library_llm_chat) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  if (is.character(bundle) && length(bundle) == 1L) bundle <- ingest_read_document_bundle(bundle)
  meta <- ingest_coalesce_metadata(metadata)
  report <- function(value, message, stage) {
    if (!is.null(progress)) progress(value, message, stage)
  }
  text <- .library_deliberative_text(bundle, cfg)
  if (!nzchar(text)) return(list(
    available = FALSE, lane = "text", error = "Parsed document text is unavailable."
  ))
  report(0.02, "Building searchable document evidence map (CPU)", "document_map")
  chunks <- .library_document_chunks(
    text, cfg$deliberative$chunk_chars, cfg$deliberative$chunk_overlap
  )
  topics <- .library_deliberative_topics()
  cache <- isTRUE(cfg$deliberative$cache_stages)
  stage_audits <- list()
  run_stage <- function(name, messages, schema, role, value, message, stage_cfg = NULL) {
    report(value, message, name)
    output <- .library_investigation_stage(
      name, messages, schema, stage_cfg %||% cfg, role, bundle, cache = cache,
      force = force, chat = chat
    )
    stage_audits[[name]] <<- output$audit
    output
  }

  recon_terms <- unique(unlist(lapply(topics, `[[`, "keywords")))
  recon_chunks <- .library_retrieve_chunks(
    chunks, recon_terms, max(cfg$deliberative$max_chunks_per_stage, 10L)
  )
  recon_user <- paste0(
    "PMID: ", meta$pmid, "\nTITLE: ", meta$title, "\nDOI: ", meta$doi,
    "\n\nCreate the document map and investigation agenda from these retrieved excerpts. ",
    "A missing fact is a question, not permission to infer it.\n\n",
    .library_format_chunks(recon_chunks)
  )
  recon <- run_stage(
    "reconnaissance",
    list(list(role = "system", content = .library_deliberative_instruction(
      cfg, "indexing", DELIBERATIVE_RECONNAISSANCE_PROMPT
    )), list(role = "user", content = recon_user)),
    .library_reconnaissance_schema(), "indexing", 0.06,
    "Reconnaissance: model inventory and investigation agenda (LLM)"
  )
  ledger <- .library_new_evidence_ledger(meta, bundle, recon$value)
  ledger <- .library_ledger_add_claims(ledger, recon$value$anchor_claims, "reconnaissance")
  ledger <- .library_ledger_add_questions(
    ledger, recon$value$investigation_questions, "reconnaissance"
  )
  ledger$stage_summaries$reconnaissance <- list(
    model_count = length(recon$value$model_inventory %||% list()),
    anchor_claim_count = length(recon$value$anchor_claims %||% list())
  )

  topic_names <- names(topics)
  topic_progress <- seq(0.16, 0.60, length.out = length(topic_names))
  for (index in seq_along(topic_names)) {
    topic <- topic_names[[index]]
    specification <- topics[[topic]]
    open_terms <- unlist(lapply(ledger$questions %||% list(), function(question) {
      if (grepl(topic, question$field %||% "", fixed = TRUE) ||
          any(vapply(specification$keywords, function(term) grepl(term, question$question %||% "",
                                                               ignore.case = TRUE), logical(1)))) {
        c(question$field, question$question, question$search_terms)
      } else character()
    }))
    evidence <- .library_retrieve_chunks(
      chunks, c(specification$keywords, open_terms), cfg$deliberative$max_chunks_per_stage
    )
    user <- paste0(
      "INVESTIGATION TOPIC: ", topic, "\n", specification$instruction,
      "\n\nCURRENT EVIDENCE LEDGER (may contain hypotheses or conflicts):\n",
      .library_compact_ledger(ledger, cfg$deliberative$ledger_context_chars),
      "\n\nRETRIEVED SOURCE EXCERPTS:\n", .library_format_chunks(evidence)
    )
    stage <- run_stage(
      paste0("investigate_", topic),
      list(list(role = "system", content = .library_deliberative_instruction(
        cfg, "indexing", DELIBERATIVE_INVESTIGATION_PROMPT
      )), list(role = "user", content = user)),
      .library_investigation_schema(), "indexing", topic_progress[[index]],
      paste("Fact-finding:", gsub("_", " / ", topic), "(LLM)")
    )
    ledger <- .library_ledger_merge_investigation(ledger, stage$value, topic)
  }

  review_terms <- c("final model", "base model", "fixed", "bootstrap", "validation",
                    "omega", "sigma", "eta", "table", "supplement")
  review_evidence <- .library_retrieve_chunks(
    chunks, c(review_terms, unlist(lapply(ledger$questions, `[[`, "search_terms"))),
    max(cfg$deliberative$max_chunks_per_stage, 10L)
  )
  review_messages <- function(extra = "") list(
    list(role = "system", content = .library_deliberative_instruction(
      cfg, "assessment", DELIBERATIVE_REVIEW_PROMPT
    )),
    list(role = "user", content = paste0(
      "Challenge this evidence ledger against the retrieved source. Do not synthesize the model.",
      if (nzchar(extra)) paste0("\n", extra) else "",
      "\n\nLEDGER:\n", .library_compact_ledger(ledger, cfg$deliberative$ledger_context_chars),
      "\n\nSOURCE EXCERPTS:\n", .library_format_chunks(review_evidence)
    ))
  )
  review <- run_stage(
    "falsification_1", review_messages(), .library_falsification_schema(),
    "assessment", 0.68, "Skeptical falsification review (LLM)"
  )
  ledger <- .library_ledger_apply_review(ledger, review$value, "falsification_1")

  latest_review <- review$value
  rounds <- cfg$deliberative$max_gap_rounds
  if (rounds > 0L) for (round in seq_len(rounds)) {
    review_terms <- .library_review_search_terms(latest_review)
    open_terms <- .library_open_question_search_terms(ledger)
    terms <- unique(c(review_terms, open_terms))
    if ((isTRUE(latest_review$overall_ready) && !length(open_terms)) || !length(terms)) break
    gap_chunks <- .library_retrieve_chunks(
      chunks, c(terms, "supplement", "appendix", "table", "equation"),
      max(cfg$deliberative$max_chunks_per_stage, 10L)
    )
    gap_name <- paste0("gap_search_", round)
    gap_user <- paste0(
      "Resolve the high-priority open ledger questions and any skeptical-review gaps. Search terms: ",
      paste(terms, collapse = "; "),
      "\n\nCURRENT LEDGER:\n",
      .library_compact_ledger(ledger, cfg$deliberative$ledger_context_chars),
      "\n\nTARGETED SOURCE EXCERPTS:\n", .library_format_chunks(gap_chunks)
    )
    gap <- run_stage(
      gap_name,
      list(list(role = "system", content = .library_deliberative_instruction(
        cfg, "indexing", DELIBERATIVE_INVESTIGATION_PROMPT
      )), list(role = "user", content = gap_user)),
      .library_investigation_schema(), "indexing", 0.72 + 0.05 * round,
      paste("Targeted gap search round", round, "(LLM)")
    )
    ledger <- .library_ledger_merge_investigation(ledger, gap$value, gap_name)
    review_name <- paste0("falsification_", round + 1L)
    follow <- run_stage(
      review_name, review_messages("Reassess previously missing and contradicted fields after the gap search."),
      .library_falsification_schema(), "assessment", 0.75 + 0.05 * round,
      paste("Falsification review round", round + 1L, "(LLM)")
    )
    latest_review <- follow$value
    ledger <- .library_ledger_apply_review(ledger, latest_review, review_name)
  }

  paths <- as.character(unlist(bundle$vision$image_paths %||% character()))
  paths <- paths[file.exists(paths)]
  if (isTRUE(cfg$deliberative$visual_verification) && length(paths)) {
    pages <- as.integer(unlist(bundle$vision$page_numbers %||% seq_along(paths)))
    visual_prompt <- paste0(
      "Independently verify the ledger's material structure, parameter, variability, dosing,",
      "and population claims against the original PDF pages. Focus on tables, equations,",
      "captions and footnotes. Return claim verdicts and precise missing-evidence questions.",
      "\n\nLEDGER:\n", .library_compact_ledger(ledger, cfg$deliberative$visual_context_chars)
    )
    visual_cfg <- cfg
    visual_endpoint <- .library_llm_role(visual_cfg, "vision")
    if (identical(visual_endpoint$provider, "ollama")) {
      visual_cfg$llm$vision$num_ctx <- max(
        as.integer(visual_endpoint$num_ctx %||% visual_cfg$ollama$num_ctx),
        cfg$deliberative$visual_num_ctx
      )
      visual_cfg$llm$vision$num_predict <- max(
        as.integer(visual_endpoint$num_predict %||% visual_cfg$ollama$num_predict),
        cfg$deliberative$visual_num_predict
      )
    }
    visual <- run_stage(
      "visual_verification",
      list(list(role = "system", content = .library_deliberative_instruction(
        cfg, "vision", DELIBERATIVE_REVIEW_PROMPT
      )), library_image_message(visual_prompt, paths, paste("Original PDF page", pages), detail = "high")),
      .library_falsification_schema(), "vision", 0.88,
      "Independent table/equation verification from PDF pages (vision LLM)",
      stage_cfg = visual_cfg
    )
    ledger <- .library_ledger_apply_review(ledger, visual$value, "visual_verification")
  }

  report(0.92, "Running deterministic cross-field consistency checks (CPU)", "consistency_checks")
  checks <- .library_ledger_consistency_checks(ledger)
  ledger$deterministic_checks <- checks
  ledger_dir <- file.path(bundle$bundle_path, "investigation")
  if (!dir.exists(ledger_dir)) dir.create(ledger_dir, recursive = TRUE, showWarnings = FALSE)
  ledger_path <- file.path(ledger_dir, "evidence-ledger.json")
  .library_atomic_write(ledger, ledger_path)

  synthesis_input <- paste0(
    "Create the final lane response for PMID ", meta$pmid, ". The evidence ledger, skeptical",
    "reviews and deterministic checks are the only permitted factual source. Populate the",
    "final model, not base/candidate/validation models. field_evidence should cite ledger",
    "claim ids. If coverage or consistency is insufficient, retain nulls and limitations.",
    "\n\nEVIDENCE LEDGER:\n",
    .library_compact_ledger(ledger, cfg$deliberative$synthesis_context_chars),
    "\n\nDETERMINISTIC CHECKS:\n",
    jsonlite::toJSON(checks, auto_unbox = TRUE, null = "null", digits = NA)
  )
  synthesis_cfg <- cfg
  synthesis_endpoint <- .library_llm_role(synthesis_cfg, "indexing")
  if (identical(synthesis_endpoint$provider, "ollama")) {
    synthesis_cfg$llm$indexing$num_ctx <- max(
      as.integer(synthesis_endpoint$num_ctx %||% synthesis_cfg$ollama$num_ctx),
      cfg$deliberative$synthesis_num_ctx
    )
    synthesis_cfg$llm$indexing$num_predict <- max(
      as.integer(synthesis_endpoint$num_predict %||% synthesis_cfg$ollama$num_predict),
      cfg$deliberative$synthesis_num_predict
    )
  }
  synthesis <- run_stage(
    "synthesis",
    list(list(role = "system", content = .library_deliberative_instruction(
      cfg, "indexing", DELIBERATIVE_SYNTHESIS_PROMPT, synthesis = TRUE
    )), list(role = "user", content = synthesis_input)),
    .library_lane_schema(), "indexing", 0.95,
    "Evidence-constrained final model synthesis (LLM)", stage_cfg = synthesis_cfg
  )
  value <- .library_enrich_lane_result(synthesis$value)
  retries <- sum(vapply(stage_audits, function(audit) {
    suppressWarnings(as.integer(audit$retry_count %||% 0L))
  }, integer(1)), na.rm = TRUE)
  audit <- synthesis$audit
  audit$lane <- "deliberative_text"
  audit$retry_count <- retries
  audit$pipeline <- "evidence_led_deliberative"
  audit$pipeline_version <- LIBRARY_PROMPT_VERSION
  audit$evidence_ledger_path <- normalizePath(ledger_path, winslash = "/", mustWork = TRUE)
  audit$evidence_claim_count <- length(ledger$claims)
  audit$open_question_count <- length(ledger$questions)
  audit$deterministic_checks <- checks
  audit$stages <- lapply(stage_audits, .library_stage_audit_summary)
  report(1, "Deliberative evidence investigation complete", "deliberative_complete")
  list(available = TRUE, lane = "text", result = value, audit = audit,
       evidence_ledger = ledger, checks = checks)
}
