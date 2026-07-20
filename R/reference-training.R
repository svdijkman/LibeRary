.library_reference_copy_tree <- function(source, target) {
  if (dir.exists(target) && length(list.files(target, all.files = TRUE, no.. = TRUE))) {
    stop("Target corpus version already exists and is not empty: ", target, call. = FALSE)
  }
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  entries <- list.files(source, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(entries) && !all(file.copy(entries, target, recursive = TRUE, copy.mode = TRUE))) {
    stop("Unable to copy the source corpus to ", target, call. = FALSE)
  }
  invisible(target)
}

#' Locate the optional local-adapter training tools
#' @return Named paths to the Python trainer and its requirements file.
#' @export
library_reference_training_files <- function() {
  root <- system.file("tools", package = "LibeRary")
  if (!nzchar(root) || !dir.exists(root)) {
    candidates <- c(file.path(getwd(), "inst", "tools"),
                    file.path(getwd(), "LibeRary", "inst", "tools"))
    root <- candidates[dir.exists(candidates)][1L]
  }
  if (is.na(root) || !dir.exists(root)) stop("Reference training tools are not installed.", call. = FALSE)
  list(trainer = normalizePath(file.path(root, "train_reference_lora.py"), winslash = "/", mustWork = TRUE),
       requirements = normalizePath(file.path(root, "reference-training-requirements.txt"),
                                    winslash = "/", mustWork = TRUE))
}

#' Create a reviewed successor version of a reference corpus
#'
#' The source version is never edited. Curation decisions are applied to a new
#' directory and recorded in each model's audit trail. Test records can be
#' promoted for strict scoring but can never become training eligible.
#'
#' @param root Existing reference corpus root.
#' @param output_dir Empty directory for the successor version.
#' @param decisions Data frame or CSV path with `reference_id`, `tier`, and
#'   optional `training_eligible`, `notes`, and `review_status` columns.
#' @param version New semantic corpus version.
#' @param curator Name or identifier of the reviewer.
#' @param corrections Optional data frame or CSV path containing field-level
#'   corrections with `reference_id`, JSON `pointer`, decision `source`, and
#'   `value_json`. Corrections are applied only to the successor's normalized
#'   `reference$extraction_target`; the historical `raw` transcription remains
#'   untouched.
#' @return New manifest invisibly.
#' @export
library_reference_revise <- function(root, output_dir, decisions, version, curator,
                                     corrections = NULL) {
  validation <- library_reference_validate(root)
  if (!validation$valid) stop("Source corpus is invalid: ", paste(validation$errors, collapse = "; "), call. = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (is.character(decisions) && length(decisions) == 1L) {
    decisions <- utils::read.csv(decisions, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!is.data.frame(decisions) || !all(c("reference_id", "tier") %in% names(decisions))) {
    stop("`decisions` must provide reference_id and tier columns.", call. = FALSE)
  }
  if (!nzchar(.library_reference_text(version)) || !nzchar(.library_reference_text(curator))) {
    stop("A new version and curator are required.", call. = FALSE)
  }
  corrections <- .library_reference_corrections(corrections)
  if (nrow(corrections)) {
    unknown <- setdiff(unique(as.character(corrections$reference_id)),
                       as.character(decisions$reference_id))
    if (length(unknown)) {
      stop("Field corrections require a model-level decision for: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
  }
  output_dir <- path.expand(output_dir)
  .library_reference_copy_tree(root, output_dir)
  for (index in seq_len(nrow(decisions))) {
    id <- .library_valid_id(decisions$reference_id[[index]])
    path <- file.path(output_dir, "models", paste0(id, ".json"))
    if (!file.exists(path)) stop("Curation decision references an unknown model: ", id, call. = FALSE)
    model <- jsonlite::read_json(path, simplifyVector = FALSE)
    previous_tier <- model$quality$tier %||% "unknown"
    tier <- toupper(trimws(as.character(decisions$tier[[index]])))
    if (!tier %in% c("A", "B", "C", "D")) stop("Invalid reference tier for ", id, call. = FALSE)
    requested_training <- if ("training_eligible" %in% names(decisions)) {
      isTRUE(as.logical(decisions$training_eligible[[index]]))
    } else isTRUE(model$quality$training_eligible)
    if (identical(model$partition, "test") && requested_training) {
      stop("Locked test model cannot be made training eligible: ", id, call. = FALSE)
    }
    review_status <- if ("review_status" %in% names(decisions) &&
                         nzchar(as.character(decisions$review_status[[index]]))) {
      as.character(decisions$review_status[[index]])
    } else "reviewed"
    notes <- if ("notes" %in% names(decisions)) .library_reference_text(decisions$notes[[index]]) else ""
    model$quality$tier <- tier
    model$quality$review_status <- review_status
    model$quality$strict_score_eligible <- tier %in% c("A", "B") && identical(review_status, "reviewed")
    model$quality$training_eligible <- requested_training && !identical(model$partition, "test") && tier %in% c("A", "B")
    model$provenance$field_tiers <- lapply(model$provenance$field_tiers, function(value) tier)
    model_corrections <- if (nrow(corrections)) {
      corrections[as.character(corrections$reference_id) == id, , drop = FALSE]
    } else data.frame()
    correction_audit <- list()
    if (nrow(model_corrections)) {
      for (correction_index in seq_len(nrow(model_corrections))) {
        pointer <- as.character(model_corrections$pointer[[correction_index]])
        source <- as.character(model_corrections$source[[correction_index]])
        if (!source %in% c("liberary", "custom")) {
          stop("Correction source must be 'liberary' or 'custom' for ", id, call. = FALSE)
        }
        replacement <- .library_reference_parse_json_value(
          model_corrections$value_json[[correction_index]]
        )
        previous <- .library_reference_pointer_get(
          model$reference$extraction_target, pointer
        )
        if (inherits(previous, "library_missing")) previous <- NULL
        model$reference$extraction_target <- .library_reference_pointer_set(
          model$reference$extraction_target, pointer, replacement
        )
        correction_audit[[length(correction_audit) + 1L]] <- list(
          pointer = pointer,
          source = source,
          previous = previous,
          value = replacement
        )
      }
    }
    event <- list(timestamp = .library_reference_now(), curator = curator,
                  parent_tier = previous_tier, tier = tier,
                  review_status = review_status, training_eligible = model$quality$training_eligible,
                  notes = notes, field_corrections = correction_audit)
    model$provenance$curation <- c(model$provenance$curation %||% list(), list(event))
    model$updated_at <- .library_reference_now()
    .library_atomic_write(model, path)
  }
  manifest_path <- file.path(output_dir, "manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  parent <- list(version = manifest$version, manifest_sha256 = .library_reference_hash(file.path(root, "manifest.json")))
  manifest$version <- as.character(version)
  manifest$status <- if (all(vapply(.library_reference_read_records(output_dir, "models"),
                                    function(x) x$quality$tier %in% c("A", "B"), logical(1)))) "curated_reference" else "mixed_reference"
  manifest$parent <- parent
  manifest$created_at <- .library_reference_now()
  manifest$curation <- list(curator = curator, decisions = nrow(decisions),
                            field_corrections = nrow(corrections))
  all_models <- .library_reference_read_records(output_dir, "models")
  manifest$counts$tiers <- as.list(table(vapply(all_models, function(x) x$quality$tier, character(1))))
  manifest$counts$training_eligible <- sum(vapply(all_models, function(x) isTRUE(x$quality$training_eligible), logical(1)))
  .library_atomic_write(manifest, manifest_path)
  checked <- library_reference_validate(output_dir)
  if (!checked$valid) stop("Revised corpus failed validation: ", paste(checked$errors, collapse = "; "), call. = FALSE)
  invisible(manifest)
}

.library_reference_jsonl <- function(records, path) {
  lines <- vapply(records, function(value) {
    as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA))
  }, character(1))
  .library_atomic_write_lines(lines, path)
}

.library_reference_training_message <- function(system, user, assistant, id, task, partition, tier) {
  list(
    id = id, task = task, partition = partition,
    leakage_guard = list(test_data = FALSE, target_absent_from_user_message = TRUE),
    quality_tier = tier,
    messages = list(
      list(role = "system", content = system),
      list(role = "user", content = user),
      list(role = "assistant", content = assistant)
    )
  )
}

.library_reference_source_text <- function(model, source_dir, max_source_chars) {
  relative <- model$source$publication_pdf$path %||% ""
  if (!nzchar(relative)) return(list(text = "", reason = "source_pdf_missing"))
  path <- file.path(source_dir, relative)
  if (!file.exists(path)) return(list(text = "", reason = "source_pdf_not_found"))
  if (!requireNamespace("pdftools", quietly = TRUE)) return(list(text = "", reason = "pdftools_not_installed"))
  text <- tryCatch(ingest_pdf_text(path, max_chars = max_source_chars), error = identity)
  if (inherits(text, "error")) return(list(text = "", reason = paste0("pdf_text_error: ", conditionMessage(text))))
  if (!nzchar(text)) return(list(text = "", reason = "pdf_text_empty"))
  list(text = text, reason = "")
}

.library_reference_triage_target <- function(record) {
  positive <- isTRUE(record$relevant_model)
  list(
    relevant_probability = if (positive) 1 else 0,
    recoverable_probability = if (isTRUE(record$recoverable_model)) 1 else 0,
    model_categories = if (positive) "other" else character(),
    positive_signals = character(), negative_signals = character(), evidence = character(),
    uncertainty = character(),
    rationale = if (positive) "Reference review included a recoverable pharmacometric model." else
      paste("Reference exclusion:", record$exclusion_reason %||% "not relevant")
  )
}

#' Export leakage-checked adapter-training examples
#'
#' Extraction examples pair source-paper text with reviewed JSON targets. The
#' appendix text itself is never included in the user message. By default only
#' reviewed A/B records explicitly marked training-eligible are exported.
#' Locked test data is rejected unconditionally.
#'
#' @param root Reference corpus root.
#' @param source_dir Original AED PK/PD review directory containing PDFs.
#' @param output_dir Empty or existing output directory.
#' @param tasks Any of `"extraction"` and `"triage"`.
#' @param partitions Training/development partitions; `test` is forbidden.
#' @param tiers Eligible quality tiers.
#' @param allow_silver Permit C/D extraction targets for experiments. These are
#'   labelled silver and should not be used for a production adapter.
#' @param require_training_eligible Require the model-level review flag.
#' @param max_source_chars Maximum source-paper characters per extraction example.
#' @return Export manifest invisibly.
#' @export
library_reference_training_export <- function(root, source_dir, output_dir,
                                              tasks = c("extraction", "triage"),
                                              partitions = c("train", "validation"),
                                              tiers = c("A", "B"), allow_silver = FALSE,
                                              require_training_eligible = TRUE,
                                              max_source_chars = 60000L) {
  validation <- library_reference_validate(root)
  if (!validation$valid) stop("Reference corpus is invalid: ", paste(validation$errors, collapse = "; "), call. = FALSE)
  tasks <- unique(match.arg(tasks, c("extraction", "triage"), several.ok = TRUE))
  partitions <- unique(as.character(partitions))
  if ("test" %in% partitions) stop("Locked test records can never be exported for training.", call. = FALSE)
  if (!all(partitions %in% c("train", "validation"))) stop("Unknown training partition.", call. = FALSE)
  tiers <- unique(toupper(as.character(tiers)))
  if (any(tiers %in% c("C", "D")) && !isTRUE(allow_silver)) {
    stop("Set `allow_silver = TRUE` to export C/D records explicitly.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list(); candidates <- list(); skipped <- list()
  if ("extraction" %in% tasks) {
    models <- .library_reference_read_records(root, "models")
    models <- Filter(function(x) x$partition %in% partitions, models)
    for (partition in partitions) {
      examples <- list()
      for (model in Filter(function(x) identical(x$partition, partition), models)) {
        reason <- ""
        if (!model$quality$tier %in% tiers) reason <- "tier_not_selected"
        if (!nzchar(reason) && isTRUE(require_training_eligible) && !isTRUE(model$quality$training_eligible)) {
          reason <- "not_training_eligible"
        }
        source <- if (!nzchar(reason)) .library_reference_source_text(model, source_dir, max_source_chars) else
          list(text = "", reason = reason)
        reason <- source$reason %||% reason
        candidates[[length(candidates) + 1L]] <- list(reference_id = model$reference_id,
          task = "extraction", partition = partition, tier = model$quality$tier,
          eligible = !nzchar(reason), reason = reason)
        if (nzchar(reason)) { skipped[[length(skipped) + 1L]] <- candidates[[length(candidates)]]; next }
        article <- jsonlite::read_json(file.path(root, "articles", paste0(model$article_id, ".json")), simplifyVector = FALSE)
        metadata <- list(pmid = article$pmid, title = article$title %||% "", abstract = article$abstract %||% "",
                         doi = "", journal = "", year = article$year %||% "", pmcid = "")
        user <- ingest_build_extraction_prompt(metadata, full_text = source$text, supplement_text = "")
        assistant <- as.character(jsonlite::toJSON(model$reference$extraction_target,
                                                   auto_unbox = TRUE, null = "null", digits = NA))
        examples[[length(examples) + 1L]] <- .library_reference_training_message(
          EXTRACTION_SYSTEM_PROMPT, user, assistant, model$reference_id,
          "extraction", partition, model$quality$tier
        )
      }
      path <- file.path(output_dir, paste0("extraction_", partition, ".jsonl"))
      .library_reference_jsonl(examples, path)
      files[[basename(path)]] <- length(examples)
    }
  }
  if ("triage" %in% tasks) {
    screening <- .library_reference_read_records(root, "screening")
    screening <- Filter(function(x) x$partition %in% partitions, screening)
    for (partition in partitions) {
      examples <- list()
      for (record in Filter(function(x) identical(x$partition, partition), screening)) {
        safe_label <- isTRUE(record$relevant_model) ||
          (record$exclusion_reason %||% "") %in% c("non_human", "no_pkpd", "no_model")
        article_path <- file.path(root, "articles", paste0("pmid_", record$pmid, ".json"))
        article <- if (file.exists(article_path)) jsonlite::read_json(article_path, simplifyVector = FALSE) else list()
        reason <- if (!safe_label) "ambiguous_or_query_specific_label" else if (!nzchar(article$abstract %||% "")) "abstract_missing" else ""
        candidates[[length(candidates) + 1L]] <- list(reference_id = record$screening_id,
          task = "triage", partition = partition, tier = record$label_quality,
          eligible = !nzchar(reason), reason = reason)
        if (nzchar(reason)) { skipped[[length(skipped) + 1L]] <- candidates[[length(candidates)]]; next }
        user <- paste0("Assess whether this abstract reports or applies a pharmacometric model.\n",
                       "Do not use information outside the supplied title and abstract.\n\n",
                       "PMID: ", record$pmid, "\nTITLE: ", article$title %||% "",
                       "\nABSTRACT:\n", article$abstract)
        assistant <- as.character(jsonlite::toJSON(.library_reference_triage_target(record),
                                                   auto_unbox = TRUE, null = "null", digits = NA))
        examples[[length(examples) + 1L]] <- .library_reference_training_message(
          TRIAGE_SYSTEM_PROMPT, user, assistant, record$screening_id,
          "triage", partition, record$label_quality
        )
      }
      path <- file.path(output_dir, paste0("triage_", partition, ".jsonl"))
      .library_reference_jsonl(examples, path)
      files[[basename(path)]] <- length(examples)
    }
  }
  manifest <- list(
    schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION,
    corpus_id = "aed-pkpd-reference",
    corpus_version = jsonlite::read_json(file.path(root, "manifest.json"), simplifyVector = FALSE)$version,
    created_at = .library_reference_now(), tasks = tasks, partitions = partitions,
    tiers = tiers, allow_silver = isTRUE(allow_silver),
    safeguards = list(test_exported = FALSE, appendix_text_in_inputs = FALSE,
                      training_eligibility_required = isTRUE(require_training_eligible)),
    files = files, candidate_count = length(candidates), skipped_count = length(skipped),
    note = "Adjudication training requires stored independent text/vision candidates and is intentionally not synthesized from the reference answer."
  )
  .library_atomic_write(manifest, file.path(output_dir, "training_manifest.json"))
  .library_atomic_write(candidates, file.path(output_dir, "candidates.json"), auto_unbox = TRUE)
  invisible(manifest)
}
