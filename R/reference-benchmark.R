.library_reference_read_records <- function(root, kind, partition = NULL) {
  paths <- list.files(file.path(root, kind), pattern = "[.]json$", full.names = TRUE)
  records <- lapply(paths, jsonlite::read_json, simplifyVector = FALSE)
  if (!is.null(partition)) records <- Filter(function(x) identical(x$partition, partition), records)
  records
}

.library_reference_prediction_files <- function(path) {
  if (!dir.exists(path)) stop("Prediction directory not found: ", path, call. = FALSE)
  paths <- list.files(path, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  paths[!basename(paths) %in% c("run_manifest.json", "summary.json", "training_manifest.json")]
}

.library_reference_prediction_index <- function(path, id_field, variant = "prediction") {
  records <- lapply(.library_reference_prediction_files(path), function(file) {
    value <- tryCatch(jsonlite::read_json(file, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(value)) return(NULL)
    id <- value[[id_field]] %||% tools::file_path_sans_ext(basename(file))
    prediction <- if (!identical(variant, "prediction")) {
      value$variants[[variant]] %||% NULL
    } else if ("prediction" %in% names(value)) {
      value[["prediction"]]
    } else value$extraction %||% value$triage %||% value
    list(id = as.character(id)[[1L]], prediction = prediction, envelope = value, path = file)
  })
  records <- Filter(Negate(is.null), records)
  output <- stats::setNames(lapply(records, `[[`, "prediction"), vapply(records, `[[`, character(1), "id"))
  output[!duplicated(names(output))]
}

.library_reference_value <- function(value, path) {
  for (name in strsplit(path, "[.]", fixed = FALSE)[[1L]]) {
    if (is.null(value) || is.null(value[[name]])) return(NULL)
    value <- value[[name]]
  }
  value
}

.library_reference_present <- function(value) {
  if (is.null(value) || !length(value)) return(FALSE)
  if (is.character(value)) return(any(nzchar(trimws(value))))
  if (is.numeric(value)) return(any(is.finite(value)))
  TRUE
}

.library_reference_normalize_string <- function(value) {
  value <- tolower(.library_reference_text(value))
  trimws(gsub("[^a-z0-9]+", " ", value))
}

.library_reference_compound <- function(value) {
  value <- .library_reference_normalize_string(value)
  aliases <- list(
    cbz = c("cbz", "carbamazepine"), clnz = c("clnz", "clonazepam"),
    gbp = c("gbp", "gabapentin"), lmt = c("lmt", "lamotrigine"),
    lvt = c("lvt", "levetiracetam"), oxc = c("oxc", "oxcarbazepine"),
    phb = c("phb", "phenobarbital", "phenobarbitone"), pht = c("pht", "phenytoin"),
    tpm = c("tpm", "topiramate"), vpa = c("vpa", "valproate", "valproic acid"),
    zns = c("zns", "zonisamide")
  )
  for (name in names(aliases)) if (value %in% aliases[[name]]) return(name)
  value
}

.library_reference_token_score <- function(target, prediction) {
  left <- unique(strsplit(.library_reference_normalize_string(target), " +")[[1L]])
  right <- unique(strsplit(.library_reference_normalize_string(prediction), " +")[[1L]])
  left <- left[nzchar(left)]; right <- right[nzchar(right)]
  if (!length(left) || !length(right)) return(0)
  precision <- length(intersect(left, right)) / length(right)
  recall <- length(intersect(left, right)) / length(left)
  if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
}

.library_reference_scalar_score <- function(path, target, prediction) {
  if (identical(path, "compound")) return(as.numeric(identical(.library_reference_compound(target),
                                                               .library_reference_compound(prediction))))
  if (is.numeric(target)) {
    prediction <- suppressWarnings(as.numeric(prediction))
    if (!length(prediction) || !is.finite(prediction[[1L]])) return(0)
    return(as.numeric(isTRUE(all.equal(as.numeric(target)[[1L]], prediction[[1L]], tolerance = 0.05))))
  }
  short <- path %in% c("route", "software", "estimation_method", "model_type")
  if (short) as.numeric(identical(.library_reference_normalize_string(target),
                                 .library_reference_normalize_string(prediction))) else
    .library_reference_token_score(target, prediction)
}

.library_reference_numeric_close <- function(target, prediction, relative_tolerance, absolute_tolerance) {
  target <- suppressWarnings(as.numeric(target)); prediction <- suppressWarnings(as.numeric(prediction))
  if (!length(target) || !length(prediction) || !is.finite(target[[1L]]) || !is.finite(prediction[[1L]])) return(FALSE)
  abs(target[[1L]] - prediction[[1L]]) <= absolute_tolerance + relative_tolerance * max(abs(target[[1L]]), absolute_tolerance)
}

.library_reference_parameter_score <- function(target, prediction, relative_tolerance, absolute_tolerance) {
  target <- target %||% list(); prediction <- prediction %||% list()
  types <- c("theta", "omega", "omega_covariance", "sigma")
  expected <- covered <- correct <- extras <- 0L
  details <- list()
  key <- function(item, type, index) {
    if (type == "omega") return(as.character(item$eta_index %||% index))
    if (type == "omega_covariance") return(paste(item$row_eta %||% index, item$col_eta %||% index, sep = ","))
    as.character(index)
  }
  numeric_value <- function(item, type) {
    if (type == "theta") item$typical else item$value %||% item$reported_value
  }
  for (type in types) {
    target_items <- target[[type]] %||% list(); prediction_items <- prediction[[type]] %||% list()
    target_keys <- vapply(seq_along(target_items), function(i) key(target_items[[i]], type, i), character(1))
    prediction_keys <- vapply(seq_along(prediction_items), function(i) key(prediction_items[[i]], type, i), character(1))
    expected <- expected + length(target_items)
    extras <- extras + sum(!prediction_keys %in% target_keys)
    for (index in seq_along(target_items)) {
      hit <- match(target_keys[[index]], prediction_keys)
      item_correct <- FALSE
      if (!is.na(hit)) {
        covered <- covered + 1L
        item_correct <- .library_reference_numeric_close(
          numeric_value(target_items[[index]], type), numeric_value(prediction_items[[hit]], type),
          relative_tolerance, absolute_tolerance
        )
        correct <- correct + as.integer(item_correct)
      }
      details[[paste(type, target_keys[[index]], sep = ":")]] <- list(covered = !is.na(hit), correct = item_correct)
    }
  }
  list(expected = expected, covered = covered, correct = correct, unverified_extras = extras,
       coverage = if (expected) covered / expected else NA_real_,
       accuracy = if (covered) correct / covered else NA_real_, details = details)
}

.library_reference_semantic_claims <- function(extraction) {
  extraction <- tryCatch(library_model_enrich(extraction), error = function(e) extraction)
  output <- list()
  add <- function(path, value) {
    if (.library_reference_present(value)) output[[path]] <<- value
  }
  structural <- extraction$structural_model %||% list()
  canonical <- structural$canonical %||% list()
  implementation <- (structural$implementations %||% list())
  implementation <- if (length(implementation)) implementation[[1L]] else list()
  add("implementation.advan", implementation$advan %||% structural$advan)
  add("implementation.trans", implementation$trans %||% structural$trans)
  add("canonical.compartments", canonical$compartments$count)
  add("canonical.input", canonical$input$process)
  add("canonical.elimination", canonical$elimination$type)
  add("canonical.parameterization", canonical$parameterization$type)

  population <- extraction$population_details %||% list()
  add("population.n_total", population$n_total)
  cohorts <- population$cohorts %||% list()
  if (length(cohorts)) add("population.cohort_count", length(cohorts))
  for (cohort_index in seq_along(cohorts)) {
    cohort <- cohorts[[cohort_index]]
    prefix <- paste0("population.cohort", cohort_index)
    add(paste0(prefix, ".n"), cohort$n$analysed)
    for (descriptor in cohort$descriptors %||% list()) {
      descriptor_name <- .library_reference_normalize_string(descriptor$name)
      if (!nzchar(descriptor_name)) next
      statistics <- descriptor$statistics %||% list()
      for (statistic_index in seq_along(statistics)) {
        statistic <- statistics[[statistic_index]]
        statistic_prefix <- paste(prefix, descriptor_name, statistic$type %||% statistic_index, sep = ".")
        for (field in c("mean", "sd", "se", "median", "minimum", "maximum",
                        "value", "cv_percent", "count", "proportion")) {
          add(paste0(statistic_prefix, ".", field), statistic[[field]])
        }
      }
    }
  }

  for (index in seq_along(extraction$dosing %||% list())) {
    regimen <- extraction$dosing[[index]]
    prefix <- paste0("dosing.", index)
    for (field in c("route", "administration", "amount", "amount_unit", "interval",
                    "interval_unit", "duration", "duration_unit", "repetitions", "steady_state")) {
      add(paste0(prefix, ".", field), regimen[[field]])
    }
  }
  for (target_index in seq_along(extraction$reproduction_targets %||% list())) {
    target <- extraction$reproduction_targets[[target_index]]
    prefix <- paste0("target.", target_index)
    for (field in c("kind", "statistic", "x_unit", "y_unit", "scale")) {
      add(paste0(prefix, ".", field), target[[field]])
    }
    for (point_index in seq_along(target$points %||% list())) {
      point <- target$points[[point_index]]
      for (field in c("time", "value", "lower", "upper")) {
        add(paste0(prefix, ".point.", point_index, ".", field), point[[field]])
      }
    }
  }
  output
}

.library_reference_semantic_score <- function(target, prediction, relative_tolerance,
                                               absolute_tolerance) {
  truth <- .library_reference_semantic_claims(target)
  proposed <- .library_reference_semantic_claims(prediction)
  details <- list()
  correct <- covered <- 0L
  for (path in names(truth)) {
    is_covered <- .library_reference_present(proposed[[path]])
    is_correct <- FALSE
    if (is_covered) {
      covered <- covered + 1L
      if (is.numeric(truth[[path]])) {
        is_correct <- .library_reference_numeric_close(
          truth[[path]], proposed[[path]], relative_tolerance, absolute_tolerance
        )
      } else if (is.logical(truth[[path]])) {
        is_correct <- identical(isTRUE(truth[[path]]), isTRUE(proposed[[path]]))
      } else {
        is_correct <- identical(
          .library_reference_normalize_string(truth[[path]]),
          .library_reference_normalize_string(proposed[[path]])
        )
      }
      correct <- correct + as.integer(is_correct)
    }
    details[[path]] <- list(covered = is_covered, correct = is_correct,
                            target = truth[[path]], prediction = proposed[[path]])
  }
  expected <- length(truth)
  extras <- sum(!names(proposed) %in% names(truth))
  list(
    expected = expected, covered = covered, correct = correct,
    coverage = if (expected) covered / expected else NA_real_,
    accuracy = if (covered) correct / covered else NA_real_,
    unverified_extras = extras, details = details
  )
}

.library_reference_extraction_score <- function(record, prediction, relative_tolerance, absolute_tolerance) {
  target <- record$reference$extraction_target
  scalar_paths <- c("title", "compound", "population", "route", "n_subjects", "software",
                    "estimation_method", "model_type", "structural_model.compartments",
                    "structural_model.description", "covariates", "residual_error")
  expected <- covered <- 0L; score_sum <- 0
  field_details <- list()
  for (path in scalar_paths) {
    truth <- .library_reference_value(target, path)
    if (!.library_reference_present(truth)) next
    expected <- expected + 1L
    proposed <- .library_reference_value(prediction, path)
    is_covered <- .library_reference_present(proposed)
    score <- if (is_covered) .library_reference_scalar_score(path, truth, proposed) else 0
    covered <- covered + as.integer(is_covered); score_sum <- score_sum + score
    field_details[[path]] <- list(covered = is_covered, score = score)
  }
  parameters <- .library_reference_parameter_score(target$parameters, prediction$parameters,
                                                   relative_tolerance, absolute_tolerance)
  semantics <- .library_reference_semantic_score(
    target, prediction, relative_tolerance, absolute_tolerance
  )
  list(
    reference_id = record$reference_id, article_id = record$article_id, pmid = record$pmid,
    compound = record$compound_group, partition = record$partition, tier = record$quality$tier,
    prediction_available = TRUE,
    scalar_expected = expected, scalar_covered = covered,
    scalar_coverage = if (expected) covered / expected else NA_real_,
    scalar_score = if (expected) score_sum / expected else NA_real_,
    numeric_expected = parameters$expected, numeric_covered = parameters$covered,
    numeric_correct = parameters$correct, numeric_coverage = parameters$coverage,
    numeric_accuracy = parameters$accuracy, unverified_numeric_extras = parameters$unverified_extras,
    semantic_expected = semantics$expected, semantic_covered = semantics$covered,
    semantic_correct = semantics$correct, semantic_coverage = semantics$coverage,
    semantic_accuracy = semantics$accuracy,
    unverified_semantic_extras = semantics$unverified_extras,
    details = list(fields = field_details, parameters = parameters$details,
                   semantics = semantics$details)
  )
}

.library_reference_mean <- function(value) {
  value <- as.numeric(value)
  if (!length(value) || all(!is.finite(value))) NA_real_ else mean(value[is.finite(value)])
}

.library_reference_auc <- function(labels, probabilities) {
  positives <- sum(labels == 1); negatives <- sum(labels == 0)
  if (!positives || !negatives) return(NA_real_)
  ranks <- rank(probabilities, ties.method = "average")
  (sum(ranks[labels == 1]) - positives * (positives + 1) / 2) / (positives * negatives)
}

.library_reference_average_precision <- function(labels, probabilities) {
  if (!sum(labels == 1)) return(NA_real_)
  order <- order(probabilities, decreasing = TRUE)
  labels <- labels[order]
  precision <- cumsum(labels == 1) / seq_along(labels)
  mean(precision[labels == 1])
}

.library_reference_write_csv <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("liberary-reference-", tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) stop("Unable to publish ", path, call. = FALSE)
  invisible(path)
}

.library_reference_prediction_reusable <- function(path, task, mode, endpoint_configuration) {
  if (!file.exists(path)) return(FALSE)
  value <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
  !is.null(value) && identical(value$task %||% "", task) &&
    identical(value$pipeline$mode %||% "", mode) &&
    identical(value$endpoint_configuration %||% "", endpoint_configuration) &&
    identical(value$status %||% "", "complete")
}

#' Run the current LibeRary pipeline on a protected reference partition
#'
#' Only article metadata and source documents are supplied to the model. The
#' appendix-derived target is never placed in a prompt or copied into the run
#' directory. Results are resumable and can be scored separately with
#' [library_reference_benchmark()].
#'
#' @param root Reference corpus root.
#' @param source_dir Original AED PK/PD review directory containing the PDFs.
#' @param output_dir Directory for prediction envelopes.
#' @param cfg LibeRary configuration.
#' @param task `"extraction"` or `"triage"`.
#' @param partition Protected partition to run.
#' @param extraction_mode For extraction, use the complete independent
#'   text/vision/adjudication pipeline or the legacy text-only lane.
#' @param limit Optional maximum number of unique articles.
#' @param resume Reuse completed prediction files.
#' @param assess Run LibeRary's independent assessment after extraction.
#' @param progress Optional callback accepting fraction, message, index, total.
#' @return Run manifest invisibly.
#' @export
library_reference_run <- function(root, source_dir, output_dir, cfg = NULL,
                                  task = c("extraction", "triage"), partition = "test",
                                  extraction_mode = c("dual", "text"),
                                  limit = Inf, resume = TRUE, assess = FALSE,
                                  progress = NULL) {
  task <- match.arg(task)
  extraction_mode <- match.arg(extraction_mode)
  if (!partition %in% c("train", "validation", "test")) stop("Unknown partition.", call. = FALSE)
  validation <- library_reference_validate(root)
  if (!validation$valid) stop("Reference corpus is invalid: ", paste(validation$errors, collapse = "; "), call. = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  dir.create(file.path(output_dir, "predictions"), recursive = TRUE, showWarnings = FALSE)
  if (task == "extraction") {
    records <- .library_reference_read_records(root, "models", partition)
    sources <- lapply(records, function(record) list(
      id = record$reference_id, article_id = record$article_id, pmid = record$pmid,
      pdf = record$source$publication_pdf$path %||% "",
      pdf_sha256 = record$source$publication_pdf$sha256 %||% ""
    ))
  } else {
    records <- .library_reference_read_records(root, "screening", partition)
    sources <- lapply(records, function(record) list(
      id = record$screening_id, article_id = paste0("pmid_", record$pmid), pmid = record$pmid,
      pdf = record$source$path %||% "", pdf_sha256 = record$source$sha256 %||% ""
    ))
  }
  if (!length(sources)) stop("No records found in partition ", partition, call. = FALSE)
  article_ids <- unique(vapply(sources, `[[`, character(1), "article_id"))
  if (is.finite(limit)) article_ids <- utils::head(article_ids, max(0L, as.integer(limit)))
  sources <- Filter(function(x) x$article_id %in% article_ids, sources)
  by_article <- split(sources, vapply(sources, `[[`, character(1), "article_id"))
  endpoint <- .library_llm_role(cfg, if (task == "extraction") "indexing" else "triage")
  endpoint_label <- if (task == "extraction" && extraction_mode == "dual") {
    vision_endpoint <- .library_llm_role(cfg, "vision")
    adjudication_endpoint <- .library_llm_role(cfg, "adjudication")
    paste0("text=", endpoint$provider, "/", endpoint$model,
           "; vision=", vision_endpoint$provider, "/", vision_endpoint$model,
           "; adjudication=", adjudication_endpoint$provider, "/", adjudication_endpoint$model)
  } else paste0(endpoint$provider, "/", endpoint$model)
  run_started <- Sys.time()
  completed <- failed <- reused <- executed <- 0L
  for (index in seq_along(by_article)) {
    group <- by_article[[index]]
    article_path <- file.path(root, "articles", paste0(group[[1L]]$article_id, ".json"))
    article <- jsonlite::read_json(article_path, simplifyVector = FALSE)
    metadata <- list(pmid = article$pmid, title = article$title %||% "", abstract = article$abstract %||% "",
                     doi = "", journal = "", year = article$year %||% "", pmcid = "", authors = article$first_author %||% "")
    destinations <- file.path(output_dir, "predictions", paste0(vapply(group, `[[`, character(1), "id"), ".json"))
    mode <- if (task == "extraction") extraction_mode else "abstract"
    if (isTRUE(resume) && all(vapply(destinations, .library_reference_prediction_reusable,
                                     logical(1), task = task, mode = mode,
                                     endpoint_configuration = endpoint_label)) &&
        all(vapply(destinations, function(path) {
          value <- jsonlite::read_json(path, simplifyVector = FALSE)
          identical(value$source_pdf_sha256 %||% "", group[[1L]]$pdf_sha256 %||% "")
        }, logical(1)))) {
      completed <- completed + length(destinations)
      reused <- reused + length(destinations)
      next
    }
    if (!is.null(progress)) progress((index - 1L) / length(by_article),
                                     paste("Running", task, article$pmid), index, length(by_article))
    article_started <- Sys.time()
    result <- tryCatch({
      if (task == "extraction") {
        pdf_relative <- group[[1L]]$pdf
        pdf_path <- if (nzchar(pdf_relative)) file.path(source_dir, pdf_relative) else ""
        if (!nzchar(pdf_path) || !file.exists(pdf_path)) stop("Source PDF is unavailable.")
        if (extraction_mode == "dual") {
          bundle <- ingest_document_bundle(metadata, pdf_path, cfg)
          dual <- ingest_dual_extract(metadata, bundle, cfg, adjudicate = TRUE)
          list(
            prediction = dual$extraction,
            variants = list(
              text = dual$text$result$extraction %||% NULL,
              vision = dual$vision$result$extraction %||% NULL,
              reconciled = dual$extraction
            ),
            pipeline = list(mode = "dual", status = dual$status,
                            model_present = dual$model_present,
                            comparison = dual$comparison,
                            independent_models = dual$independent_models %||% FALSE,
                            warning = dual$warning %||% "", audit = dual$audit %||% list())
          )
        } else {
          extraction_result <- ingest_extract_model(metadata, cfg, pdf_path = pdf_path, assess = assess)
          extraction <- extraction_result$extraction
          list(prediction = extraction, variants = list(text = extraction),
               pipeline = list(mode = "text", assessed = isTRUE(assess),
                               audit = extraction_result$raw_llm %||% list()))
        }
      } else {
        triage <- ingest_triage_abstract(metadata, cfg)
        list(prediction = triage, variants = list(), pipeline = list(mode = "abstract"))
      }
    }, error = identity)
    article_elapsed <- as.numeric(difftime(Sys.time(), article_started, units = "secs"))
    executed <- executed + length(group)
    for (position in seq_along(group)) {
      envelope <- list(
        schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION, task = task,
        reference_partition = partition,
        target_absent_from_prompt = TRUE,
        reference_id = if (task == "extraction") group[[position]]$id else NULL,
        screening_id = if (task == "triage") group[[position]]$id else NULL,
        article_id = group[[position]]$article_id, pmid = group[[position]]$pmid,
        source_pdf_sha256 = group[[position]]$pdf_sha256,
        provider = endpoint$provider %||% "", model = endpoint$model %||% "",
        endpoint_configuration = endpoint_label,
        created_at = .library_reference_now(),
        runtime_seconds = article_elapsed,
        status = if (inherits(result, "error")) "error" else "complete",
        error = if (inherits(result, "error")) conditionMessage(result) else NULL,
        prediction = if (inherits(result, "error")) NULL else result$prediction,
        variants = if (inherits(result, "error")) list() else result$variants,
        pipeline = if (inherits(result, "error")) list(mode = extraction_mode) else result$pipeline
      )
      .library_atomic_write(envelope, destinations[[position]])
      if (inherits(result, "error")) failed <- failed + 1L else completed <- completed + 1L
    }
  }
  manifest <- list(
    schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION, task = task, partition = partition,
    corpus_version = jsonlite::read_json(file.path(root, "manifest.json"), simplifyVector = FALSE)$version,
    provider = endpoint$provider %||% "", model = endpoint$model %||% "",
    endpoint_configuration = endpoint_label,
    extraction_mode = if (task == "extraction") extraction_mode else NULL,
    started_from_target_free_source_manifest = TRUE,
    completed = completed, failed = failed, executed = executed, reused = reused,
    elapsed_seconds = as.numeric(difftime(Sys.time(), run_started, units = "secs")),
    updated_at = .library_reference_now()
  )
  .library_atomic_write(manifest, file.path(output_dir, "run_manifest.json"))
  if (!is.null(progress)) progress(1, paste(task, "run complete"), length(by_article), length(by_article))
  invisible(manifest)
}

#' Score LibeRary predictions against a reference corpus
#'
#' Strict and silver-tier results are reported separately. Additional values in
#' a silver reference are labelled unverified extras, not hallucinations,
#' because the appendix itself may be incomplete.
#'
#' @param root Reference corpus root.
#' @param predictions Directory produced by [library_reference_run()] or an equivalent runner.
#' @param task `"extraction"` or `"triage"`.
#' @param partition Partition to score.
#' @param output_dir Optional report directory.
#' @param relative_tolerance Relative numeric tolerance.
#' @param absolute_tolerance Absolute numeric tolerance.
#' @param probability_threshold First-pass threshold for triage sensitivity/specificity.
#' @param prediction_variant Score the primary prediction or a stored
#'   `text`, `vision`, or `reconciled` dual-pipeline variant.
#' @return Benchmark summary and per-record results.
#' @export
library_reference_benchmark <- function(root, predictions, task = c("extraction", "triage"),
                                        partition = "test", output_dir = NULL,
                                        relative_tolerance = 0.05, absolute_tolerance = 1e-8,
                                        probability_threshold = 0.30,
                                        prediction_variant = "prediction") {
  task <- match.arg(task)
  validation <- library_reference_validate(root)
  if (!validation$valid) stop("Reference corpus is invalid: ", paste(validation$errors, collapse = "; "), call. = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  run_manifest_path <- file.path(dirname(normalizePath(predictions, winslash = "/", mustWork = TRUE)),
                                 "run_manifest.json")
  run_info <- if (file.exists(run_manifest_path)) {
    jsonlite::read_json(run_manifest_path, simplifyVector = FALSE)
  } else list(status = "run_manifest_unavailable")
  if (task == "extraction") {
    records <- .library_reference_read_records(root, "models", partition)
    index <- .library_reference_prediction_index(predictions, "reference_id", prediction_variant)
    scores <- lapply(records, function(record) {
      prediction <- index[[record$reference_id]]
      if (is.null(prediction)) return(c(list(reference_id = record$reference_id, article_id = record$article_id,
                                                  pmid = record$pmid, compound = record$compound_group,
                                                  partition = record$partition, tier = record$quality$tier),
                                             list(prediction_available = FALSE,
                                                  scalar_expected = NA, scalar_covered = 0L, scalar_coverage = 0,
                                                  scalar_score = 0, numeric_expected = NA, numeric_covered = 0L,
                                                  numeric_correct = 0L, numeric_coverage = 0, numeric_accuracy = NA,
                                                  unverified_numeric_extras = 0L,
                                                  semantic_expected = NA, semantic_covered = 0L,
                                                  semantic_correct = 0L, semantic_coverage = 0,
                                                  semantic_accuracy = NA,
                                                  unverified_semantic_extras = 0L,
                                                  details = list(missing_prediction = TRUE))))
      .library_reference_extraction_score(record, prediction, relative_tolerance, absolute_tolerance)
    })
    table <- data.frame(
      reference_id = vapply(scores, `[[`, character(1), "reference_id"),
      article_id = vapply(scores, `[[`, character(1), "article_id"),
      pmid = vapply(scores, `[[`, character(1), "pmid"),
      compound = vapply(scores, `[[`, character(1), "compound"),
      tier = vapply(scores, `[[`, character(1), "tier"),
      prediction_available = vapply(scores, function(x) isTRUE(x$prediction_available), logical(1)),
      scalar_coverage = vapply(scores, function(x) as.numeric(x$scalar_coverage %||% NA_real_), numeric(1)),
      scalar_score = vapply(scores, function(x) as.numeric(x$scalar_score %||% NA_real_), numeric(1)),
      numeric_coverage = vapply(scores, function(x) as.numeric(x$numeric_coverage %||% NA_real_), numeric(1)),
      numeric_accuracy = vapply(scores, function(x) as.numeric(x$numeric_accuracy %||% NA_real_), numeric(1)),
      semantic_coverage = vapply(scores, function(x) as.numeric(x$semantic_coverage %||% NA_real_), numeric(1)),
      semantic_accuracy = vapply(scores, function(x) as.numeric(x$semantic_accuracy %||% NA_real_), numeric(1)),
      unverified_numeric_extras = vapply(scores, function(x) as.integer(x$unverified_numeric_extras %||% 0L), integer(1)),
      unverified_semantic_extras = vapply(scores, function(x) as.integer(x$unverified_semantic_extras %||% 0L), integer(1)),
      stringsAsFactors = FALSE
    )
    summarize <- function(tiers) {
      targets <- table$tier %in% tiers
      selected <- targets & table$prediction_available
      list(n = sum(targets), scored = sum(selected),
           scalar_coverage = .library_reference_mean(table$scalar_coverage[selected]),
           scalar_score = .library_reference_mean(table$scalar_score[selected]),
           numeric_coverage = .library_reference_mean(table$numeric_coverage[selected]),
           numeric_accuracy = .library_reference_mean(table$numeric_accuracy[selected]),
           semantic_coverage = .library_reference_mean(table$semantic_coverage[selected]),
           semantic_accuracy = .library_reference_mean(table$semantic_accuracy[selected]),
           unverified_numeric_extras = sum(table$unverified_numeric_extras[selected], na.rm = TRUE),
           unverified_semantic_extras = sum(table$unverified_semantic_extras[selected], na.rm = TRUE))
    }
    summary <- list(
      schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION, task = task, partition = partition,
      prediction_variant = prediction_variant,
      evaluated_at = .library_reference_now(), predictions_found = sum(table$prediction_available),
      predictions_expected = nrow(table), numeric_tolerance = list(relative = relative_tolerance, absolute = absolute_tolerance),
      run = run_info,
      strict = summarize(c("A", "B")), silver = summarize(c("C", "D")), all = summarize(c("A", "B", "C", "D")),
      interpretation = "Tier C/D metrics are development signals, not gold-standard claims. Extras are unverified, not automatically hallucinated."
    )
  } else {
    records <- .library_reference_read_records(root, "screening", partition)
    # Wrong-drug and unspecified exclusions are compound-review decisions, not
    # reliable negatives for LibeRary's general pharmacometric-model triage.
    records <- Filter(function(x) isTRUE(x$relevant_model) ||
                        (x$exclusion_reason %||% "") %in% c("non_human", "no_pkpd", "no_model"), records)
    index <- .library_reference_prediction_index(predictions, "screening_id", prediction_variant)
    rows <- lapply(records, function(record) {
      prediction <- index[[record$screening_id]]
      probability <- if (is.null(prediction)) NA_real_ else suppressWarnings(as.numeric(
        prediction$relevant_probability %||% prediction$model_probability %||% NA_real_))
      data.frame(screening_id = record$screening_id, pmid = record$pmid,
                 compound = record$compound_group, label = as.integer(isTRUE(record$relevant_model)),
                 probability = if (length(probability)) probability[[1L]] else NA_real_,
                 stringsAsFactors = FALSE)
    })
    table <- if (length(rows)) do.call(rbind, rows) else data.frame()
    scored <- is.finite(table$probability)
    labels <- table$label[scored]; probabilities <- pmin(1, pmax(0, table$probability[scored]))
    decisions <- probabilities >= probability_threshold
    summary <- list(
      schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION, task = task, partition = partition,
      prediction_variant = prediction_variant,
      evaluated_at = .library_reference_now(), predictions_found = sum(scored), predictions_expected = nrow(table),
      run = run_info,
      scope = "general pharmacometric-model triage; wrong-drug and unspecified exclusions omitted",
      brier = if (length(labels)) mean((probabilities - labels)^2) else NA_real_,
      log_loss = if (length(labels)) -mean(labels * log(pmax(probabilities, 1e-12)) +
                                             (1 - labels) * log(pmax(1 - probabilities, 1e-12))) else NA_real_,
      auroc = .library_reference_auc(labels, probabilities),
      average_precision = .library_reference_average_precision(labels, probabilities),
      threshold = probability_threshold,
      sensitivity = if (sum(labels == 1)) mean(decisions[labels == 1]) else NA_real_,
      specificity = if (sum(labels == 0)) mean(!decisions[labels == 0]) else NA_real_
    )
  }
  result <- list(summary = summary, per_record = table)
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    .library_atomic_write(summary, file.path(output_dir, "summary.json"))
    .library_reference_write_csv(table, file.path(output_dir, "per_record.csv"))
    if (task == "extraction") .library_atomic_write(scores, file.path(output_dir, "details.json"), auto_unbox = TRUE)
  }
  result
}
