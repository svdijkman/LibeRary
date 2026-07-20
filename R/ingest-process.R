#' Process a triaged literature batch into reconciled catalogue entries
#'
#' By default only High and Intermediate probability records are processed.
#' Low-probability records remain in the manifest and can be processed later by
#' setting `tiers = "low"`. Each PDF is converted to a resumable document bundle,
#' independently extracted through text and vision lanes, compared field by
#' field, and automatically adjudicated when necessary.
#'
#' @param manifest Discovery manifest path or data frame.
#' @param cfg LibeRary configuration.
#' @param tiers Triage tiers to process.
#' @param limit Maximum records to process.
#' @param resume Reuse terminal decisions and existing catalogue entries.
#' @param adjudicate Run automatic third-model adjudication.
#' @param overwrite Update an existing catalogue entry.
#' @param continue_on_error Continue remaining articles after an article error.
#' @param log Optional logger.
#' @param progress Optional progress callback.
#' @param article_progress Optional callback for progress within the current
#'   PMID, accepting `value`, `message`, `step`, `total`, `pmid`, `stage`, and
#'   `status`.
#' @return Batch results and summary.
#' @export
ingest_process_batch <- function(manifest, cfg = NULL, tiers = NULL, limit = Inf,
                                 resume = TRUE, adjudicate = TRUE, overwrite = TRUE,
                                 continue_on_error = TRUE, log = NULL, progress = NULL,
                                 article_progress = NULL) {
  log <- log %||% function(...) invisible(NULL)
  prog <- function(value, message, step = NA_integer_, total = NA_integer_) {
    if (!is.null(progress)) progress(value, message, step, total)
  }
  article_prog <- function(value, message, step = NA_integer_, total = 8L,
                           pmid = "", stage = "", status = "running") {
    if (!is.null(article_progress)) {
      article_progress(value, message, step, total, pmid, stage, status)
    }
  }
  cfg <- if (is.character(cfg) && length(cfg) == 1L) ingest_load_config(cfg) else
    if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  ingest_ensure_dirs(cfg); .library_initialize_catalog(cfg$catalog_dir)
  df <- if (is.character(manifest) && length(manifest) == 1L && file.exists(manifest)) {
    ingest_read_manifest(manifest)
  } else if (is.data.frame(manifest)) manifest else stop("`manifest` must be a CSV path or data frame.")
  tiers <- tolower(as.character(tiers %||% cfg$triage$first_pass_tiers))
  if (any(!tiers %in% LIBRARY_TRIAGE_TIERS)) stop("Unknown triage tier.", call. = FALSE)
  if ("triage_tier" %in% names(df)) df <- df[tolower(df$triage_tier) %in% tiers, , drop = FALSE]
  df <- df[df$acquisition_class != "skipped", , drop = FALSE]
  if (is.finite(limit) && nrow(df) > as.integer(limit)) df <- df[seq_len(as.integer(limit)), , drop = FALSE]
  results <- vector("list", nrow(df))

  for (index in seq_len(nrow(df))) {
    row <- df[index, , drop = FALSE]
    pmid <- as.character(row$pmid[[1L]])
    entry_id <- paste0("pmid_", pmid)
    prog((index - 1) / max(1, nrow(df)),
         sprintf("Processing PMID %s (%d/%d)", pmid, index, nrow(df)), index, nrow(df))
    process_one <- function() {
      article_prog(0.02, sprintf("PMID %s \u2014 retrieving metadata (network/cache)", pmid),
                   1L, pmid = pmid, stage = "metadata")
      metadata <- ingest_entrez_fetch_metadata(pmid, cfg = cfg, use_cache = TRUE)[[pmid]]
      if (is.null(metadata)) metadata <- list(
        pmid = pmid, title = as.character(row$title[[1L]] %||% ""), abstract = "",
        doi = as.character(row$doi[[1L]] %||% ""), journal = as.character(row$publisher[[1L]] %||% ""),
        year = as.character(row$year[[1L]] %||% ""), pmcid = as.character(row$pmcid[[1L]] %||% ""),
        authors = character()
      )
      candidate <- as.character(row$local_path[[1L]] %||% "")
      pdf <- if (nzchar(candidate) && file.exists(candidate)) candidate else
        file.path(cfg$inbox_dir, pmid, "article.pdf")
      if (!file.exists(pdf)) {
        article_prog(1, sprintf("PMID %s \u2014 awaiting PDF", pmid), 8L, pmid = pmid,
                     stage = "awaiting_pdf", status = "done")
        return(list(pmid = pmid, status = "awaiting_pdf", skipped = TRUE,
                    message = "Acquire the PDF before full-text processing."))
      }
      article_prog(0.08, sprintf("PMID %s \u2014 Docling parsing and PDF rendering (CPU)", pmid),
                   2L, pmid = pmid, stage = "document_preparation")
      bundle <- ingest_document_bundle(metadata, pdf, cfg)
      article_prog(0.23, sprintf("PMID %s \u2014 document bundle ready (CPU/disk)", pmid),
                   3L, pmid = pmid, stage = "document_preparation")
      decision_path <- file.path(bundle$bundle_path, "decision.json")
      if (isTRUE(resume) && file.exists(decision_path)) {
        old <- jsonlite::read_json(decision_path, simplifyVector = FALSE)
        expected_pipeline <- if (isTRUE(cfg$deliberative$enabled)) {
          "evidence_led_deliberative"
        } else "dual_lane_one_shot"
        if (identical(old$source_sha256 %||% "", bundle$source$sha256 %||% "") &&
            identical(old$pipeline %||% "", expected_pipeline) &&
            identical(old$pipeline_version %||% "", LIBRARY_PROMPT_VERSION) &&
            old$status %in% c(
              "excluded_no_model", "machine_consistent", "machine_adjudicated", "needs_review"
            )) {
          article_prog(1, sprintf("PMID %s \u2014 reused completed article decision", pmid),
                       8L, pmid = pmid, stage = "reused", status = "done")
          return(c(old, list(pmid = pmid, skipped = TRUE, reused = TRUE)))
        }
      }
      dual <- ingest_dual_extract(
        metadata, bundle, cfg, adjudicate = adjudicate,
        progress = function(value, message, stage) {
          article_prog(0.23 + 0.61 * value, sprintf("PMID %s \u2014 %s", pmid, message),
                       4L + as.integer(value >= 0.34) + as.integer(value >= 0.66) +
                         as.integer(value >= 0.71),
                       pmid = pmid, stage = stage)
        }
      )
      article_prog(0.86, sprintf("PMID %s \u2014 saving extraction audit and decision (disk)", pmid),
                   7L, pmid = pmid, stage = "audit")
      audit_path <- file.path(bundle$bundle_path, "extraction-audit.json")
      .library_atomic_write(dual$audit, audit_path)
      lane_issues <- list()
      for (lane in c("text", "vision")) {
        lane_result <- dual[[lane]]
        retries <- suppressWarnings(as.integer(lane_result$audit$retry_count %||% 0L))
        if (isTRUE(lane_result$available) && length(retries) && is.finite(retries[[1L]]) && retries[[1L]] > 0L) {
          log(sprintf("PMID %s %s lane recovered after %d structured-output retry/retries",
                      pmid, lane, retries[[1L]]), "INFO")
        }
        if (!isTRUE(lane_result$available)) {
          issue <- as.character(lane_result$error %||% paste(lane, "lane unavailable"))[[1L]]
          lane_issues[[lane]] <- issue
          log(sprintf("PMID %s %s lane unavailable: %s", pmid, lane, issue), "WARN")
        }
      }
      confirmed_absent <- identical(dual$model_present, FALSE) &&
        dual$status %in% c("machine_consistent", "machine_adjudicated")
      status <- if (confirmed_absent) "excluded_no_model" else dual$status
      decision <- list(
        schema_version = LIBRARY_SCHEMA_VERSION,
        pipeline = if (isTRUE(cfg$deliberative$enabled)) {
          "evidence_led_deliberative"
        } else "dual_lane_one_shot",
        pipeline_version = LIBRARY_PROMPT_VERSION,
        source_sha256 = bundle$source$sha256,
        pmid = pmid,
        triage_tier = if ("triage_tier" %in% names(row)) as.character(row$triage_tier[[1L]]) else "",
        status = status,
        model_present = dual$model_present,
        comparison = dual$comparison,
        adjudication = dual$adjudication$result %||% NULL,
        warning = dual$warning,
        lane_issues = lane_issues,
        audit_artifact = "extraction-audit.json",
        completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
      )
      .library_atomic_write(decision, decision_path)
      if (confirmed_absent) {
        log(sprintf("PMID %s excluded: no recoverable model", pmid), "INFO")
        article_prog(1, sprintf("PMID %s \u2014 no recoverable model", pmid), 8L,
                     pmid = pmid, stage = "complete", status = "done")
        return(c(decision, list(decision_path = decision_path, skipped = FALSE)))
      }
      if (!isTRUE(dual$model_present)) {
        log(sprintf("PMID %s needs review before model-presence decision", pmid), "WARN")
        article_prog(1, sprintf("PMID %s \u2014 review required", pmid), 8L,
                     pmid = pmid, stage = "complete", status = "done")
        return(c(decision, list(decision_path = decision_path, skipped = FALSE)))
      }
      catalog_status <- if (identical(dual$status, "needs_review")) "review" else dual$status
      assessment <- list(
        method = if (isTRUE(cfg$deliberative$enabled)) {
          "deliberative_dual_lane_reconciliation"
        } else "dual_lane_reconciliation",
        comparison = dual$comparison,
        adjudication = dual$adjudication$result %||% list(status = "not_required"),
        machine_status = dual$status,
        human_validated = FALSE
      )
      article_prog(0.93, sprintf("PMID %s \u2014 catalogue and reproduction planning (CPU/disk)", pmid),
                   8L, pmid = pmid, stage = "publication")
      publication <- ingest_publish_catalog_entry(
        metadata, dual$extraction, cfg, status = catalog_status,
        raw_llm = dual$audit, library_id = entry_id, assessment = assessment,
        overwrite = overwrite, document_bundle = bundle
      )
      log(sprintf("Published %s (%s)", entry_id, catalog_status), "INFO")
      article_prog(1, sprintf("PMID %s \u2014 published", pmid), 8L,
                   pmid = pmid, stage = "complete", status = "done")
      c(publication, decision, list(decision_path = decision_path, skipped = FALSE))
    }
    article_result <- tryCatch(process_one(), error = identity)
    if (inherits(article_result, "error")) {
      log(sprintf("PMID %s failed: %s", pmid, conditionMessage(article_result)), "ERROR")
      article_prog(1, sprintf("PMID %s failed: %s", pmid, conditionMessage(article_result)),
                   8L, pmid = pmid, stage = "failed", status = "error")
      results[[index]] <- list(pmid = pmid, status = "failed", error = conditionMessage(article_result), skipped = FALSE)
      if (!isTRUE(continue_on_error)) stop(article_result)
    } else results[[index]] <- article_result
    prog(index / max(1, nrow(df)), sprintf("Completed %d/%d articles", index, nrow(df)),
         index, nrow(df))
  }
  prog(1, "Literature processing complete", nrow(df), nrow(df))
  statuses <- vapply(results, function(x) as.character(x$status %||% "unknown")[[1L]], character(1))
  list(
    results = results,
    summary = list(
      selected = nrow(df),
      tiers = tiers,
      status_counts = as.list(table(statuses)),
      catalog_dir = cfg$catalog_dir,
      providers = lapply(.library_llm_roles(), function(role) {
        endpoint <- .library_llm_role(cfg, role)
        endpoint[c("provider", "model")]
      }) |> stats::setNames(.library_llm_roles())
    )
  )
}
