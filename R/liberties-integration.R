.library_job_config <- function(cfg) {
  cfg <- ingest_validate_config(cfg)
  cfg$entrez$api_key <- ""
  cfg$data_dir <- ""; cfg$inbox_dir <- ""; cfg$cache_dir <- ""; cfg$catalog_dir <- ""
  # Only environment-variable names travel with a job; secret values never do.
  cfg
}

#' Create a typed LibeRties literature job
#'
#' The resulting job contains data, provider/model choices and evidence only;
#' it cannot contain executable R code. API keys are resolved from environment
#' variables on the worker. Full-text transfer requires explicit confirmation.
#' @param task One of `triage`, `parse`, `index`, `dual_extract`, `assess`, or
#'   `adjudicate`.
#' @param metadata Publication metadata.
#' @param extraction Proposed extraction for an assessment job.
#' @param full_text Optional article text.
#' @param supplement_text Optional supplement/control-stream text.
#' @param pdf_path Optional PDF for parse, dual extraction, or adjudication.
#' @param text_lane Text-lane result for an adjudication job.
#' @param vision_lane Vision-lane result for an adjudication job.
#' @param comparison Field comparison for an adjudication job.
#' @param cfg LibeRary configuration.
#' @param run_assessment Also assess the extraction produced by an index job.
#' @param confirm_transfer Confirm that supplied text may be sent to the queue worker.
#' @param label Job label.
#' @return A `liber_job` accepted by local and remote LibeRties queues.
#' @export
library_job <- function(task = c("triage", "parse", "index", "dual_extract", "assess", "adjudicate"), metadata,
                        extraction = NULL, full_text = "", supplement_text = "",
                        pdf_path = NULL, text_lane = NULL, vision_lane = NULL,
                        comparison = NULL,
                        cfg = NULL, run_assessment = TRUE,
                        confirm_transfer = FALSE, label = NULL) {
  task <- match.arg(task)
  if (!requireNamespace("LibeRties", quietly = TRUE)) stop("Install LibeRties >= 0.6.0 to create queued literature jobs.")
  full_text <- as.character(full_text %||% "")[[1L]]
  supplement_text <- as.character(supplement_text %||% "")[[1L]]
  has_pdf <- !is.null(pdf_path) && length(pdf_path) == 1L && file.exists(pdf_path)
  if ((nzchar(full_text) || nzchar(supplement_text) || has_pdf) && !isTRUE(confirm_transfer)) {
    stop("Set `confirm_transfer=TRUE` after confirming that the selected queue may receive the publication text.")
  }
  if (task == "assess" && is.null(extraction)) stop("An assessment job requires `extraction`.")
  if (task %in% c("parse", "dual_extract", "adjudicate") && !has_pdf) {
    stop("This task requires an existing `pdf_path`.", call. = FALSE)
  }
  if (task == "adjudicate" && (is.null(text_lane) || is.null(vision_lane) || is.null(comparison))) {
    stop("An adjudication job requires text_lane, vision_lane, and comparison.", call. = FALSE)
  }
  cfg <- .library_job_config(if (is.null(cfg)) ingest_load_config() else cfg)
  cfg$llm$allow_remote_content <- TRUE
  pdf_base64 <- if (has_pdf) {
    bytes <- readBin(pdf_path, what = "raw", n = file.info(pdf_path)$size)
    jsonlite::base64_enc(bytes)
  } else ""
  payload <- list(metadata = metadata, extraction = extraction, full_text = full_text,
                  supplement_text = supplement_text, pdf_base64 = pdf_base64,
                  pdf_name = if (has_pdf) basename(pdf_path) else "",
                  text_lane = text_lane, vision_lane = vision_lane, comparison = comparison)
  create_job <- getExportedValue("LibeRties", "ls_library_job")
  create_job(
    paste0("library_", task), payload,
    arguments = list(cfg = cfg, run_assessment = isTRUE(run_assessment)), label = label
  )
}

#' Execute a typed LibeRary worker task
#'
#' This is the only LibeRary entry point callable by LibeRties workers.
#' @param type A typed LibeRary pipeline job.
#' @param payload Validated data payload.
#' @param arguments Named task controls.
#' @return Extraction or assessment result.
#' @export
library_worker_task <- function(type, payload, arguments = list()) {
  type <- match.arg(type, c("library_triage", "library_parse", "library_index",
                            "library_dual_extract", "library_assess", "library_adjudicate"))
  if (!is.list(payload) || is.null(payload$metadata)) stop("Invalid LibeRary worker payload.")
  if (!is.list(arguments) || (length(arguments) && is.null(names(arguments)))) stop("Invalid LibeRary worker arguments.")
  cfg <- ingest_validate_config(arguments$cfg %||% ingest_load_config())
  cfg$llm$allow_remote_content <- TRUE
  worker_root <- tempfile("liberary-worker-")
  dir.create(worker_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(worker_root, recursive = TRUE, force = TRUE), add = TRUE)
  cfg$data_dir <- worker_root
  cfg$inbox_dir <- file.path(worker_root, "inbox")
  cfg$cache_dir <- file.path(worker_root, "cache")
  cfg$catalog_dir <- file.path(worker_root, "catalog")
  ingest_ensure_dirs(cfg)
  make_bundle <- function() {
    encoded <- as.character(payload$pdf_base64 %||% "")[[1L]]
    if (!nzchar(encoded)) stop("Worker task requires a PDF payload.", call. = FALSE)
    path <- file.path(worker_root, "article.pdf")
    writeBin(jsonlite::base64_dec(encoded), path)
    ingest_document_bundle(payload$metadata, path, cfg)
  }
  if (type == "library_triage") return(ingest_triage_abstract(payload$metadata, cfg))
  if (type == "library_parse") {
    bundle <- make_bundle()
    return(list(bundle = bundle, document_text = .library_bundle_text(bundle, cfg$ollama$max_pdf_chars)))
  }
  if (type == "library_dual_extract") {
    return(ingest_dual_extract(payload$metadata, make_bundle(), cfg,
                               adjudicate = isTRUE(arguments$adjudicate %||% TRUE)))
  }
  if (type == "library_adjudicate") {
    return(ingest_adjudicate_extractions(payload$metadata, payload$text_lane,
      payload$vision_lane, payload$comparison, make_bundle(), cfg))
  }
  if (type == "library_index") {
    return(ingest_extract_model(
      payload$metadata, cfg = cfg, assess = isTRUE(arguments$run_assessment %||% TRUE),
      full_text = payload$full_text %||% "", supplement_text = payload$supplement_text %||% ""
    ))
  }
  ingest_assess_model(payload$metadata, payload$extraction, cfg,
                      payload$full_text %||% "", payload$supplement_text %||% "")
}
