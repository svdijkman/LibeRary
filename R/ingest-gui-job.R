#' Background worker for LibeRary GUI jobs
#'
#' @param params List with job settings (see Shiny app).
#' @return List with `ok` and result or error.
#' @export
ingest_gui_worker <- function(params) {
  if (!isTRUE(params$skip_package_load)) {
    if (nzchar(params$lib_path %||% "") && dir.exists(params$lib_path)) {
      if (requireNamespace("devtools", quietly = TRUE)) {
        devtools::load_all(params$lib_path, quiet = TRUE)
      }
    } else if (requireNamespace("LibeRary", quietly = TRUE)) {
      library(LibeRary)
    } else {
      stop("LibeRary is not loaded in this R session.", call. = FALSE)
    }
  }

  cfg <- ingest_load_config(params$config_path)
  if (nzchar(params$entrez_email %||% "")) {
    cfg$entrez$email <- params$entrez_email
    cfg$unpaywall$email <- params$entrez_email
  }
  if (nzchar(params$entrez_api_key %||% "")) {
    cfg$entrez$api_key <- params$entrez_api_key
  }
  if (nzchar(params$data_dir %||% "")) {
    cfg$data_dir <- params$data_dir
    cfg$inbox_dir <- file.path(params$data_dir, "inbox")
    cfg$cache_dir <- file.path(params$data_dir, "cache")
    cfg$catalog_dir <- file.path(params$data_dir, "catalog")
  }
  if (nzchar(params$indexing_provider %||% "")) cfg$llm$indexing$provider <- params$indexing_provider
  if (nzchar(params$indexing_model %||% "")) cfg$llm$indexing$model <- params$indexing_model
  if (nzchar(params$assessment_provider %||% "")) cfg$llm$assessment$provider <- params$assessment_provider
  if (nzchar(params$assessment_model %||% "")) cfg$llm$assessment$model <- params$assessment_model
  for (role in c("triage", "vision", "adjudication")) {
    provider <- params[[paste0(role, "_provider")]] %||% ""
    model <- params[[paste0(role, "_model")]] %||% ""
    if (nzchar(provider)) cfg$llm[[role]]$provider <- provider
    if (nzchar(model)) cfg$llm[[role]]$model <- model
  }
  if (!is.null(params$triage_high_threshold)) cfg$triage$high_threshold <- as.numeric(params$triage_high_threshold)
  if (!is.null(params$triage_intermediate_threshold)) cfg$triage$intermediate_threshold <- as.numeric(params$triage_intermediate_threshold)
  cfg$llm$require_independent_extraction_models <- isTRUE(params$require_independent_extraction_models)
  cfg$llm$allow_remote_content <- isTRUE(params$allow_remote_content)
  if (!is.null(params$deliberative_enabled)) {
    cfg$deliberative$enabled <- isTRUE(params$deliberative_enabled)
  }
  if (!is.null(params$deliberative_visual_verification)) {
    cfg$deliberative$visual_verification <- isTRUE(params$deliberative_visual_verification)
  }
  if (!is.null(params$deliberative_cache_stages)) {
    cfg$deliberative$cache_stages <- isTRUE(params$deliberative_cache_stages)
  }
  if (!is.null(params$deliberative_max_gap_rounds)) {
    cfg$deliberative$max_gap_rounds <- as.integer(params$deliberative_max_gap_rounds)
  }
  if (!is.null(params$deliberative_max_chunks_per_stage)) {
    cfg$deliberative$max_chunks_per_stage <- as.integer(params$deliberative_max_chunks_per_stage)
  }
  cfg <- ingest_validate_config(cfg)

  log <- ingest_make_logger(params$log_path)
  progress <- function(value, message, step = NA_integer_, total = NA_integer_) {
    ingest_write_progress(params$progress_path, value, message, step, total, "running")
  }
  article_progress <- function(value, message, step = NA_integer_, total = NA_integer_,
                               pmid = "", stage = "", status = "running") {
    .ingest_write_current_progress(
      params$progress_path, value, message, step, total, pmid, stage, status
    )
  }

  ingest_write_progress(params$progress_path, 0, "Starting job...", status = "running")
  log(sprintf("Job: %s", params$job), "INFO")

  tryCatch({
    out <- switch(params$job,
      discover = {
        ingest_discover(
          query = params$query,
          limit = as.integer(params$limit),
          cfg = cfg,
          download_oa = isTRUE(params$download_oa),
          log = log,
          progress = progress
        )
      },
      fetch = {
        manifest <- params$manifest
        if (!nzchar(manifest)) {
          manifest <- ingest_latest_manifest(cfg)
        }
        if (!nzchar(manifest)) stop("No manifest found. Run Discover first.")
        ingest_fetch_institutional(
          manifest,
          cfg = cfg,
          classes = params$fetch_classes %||% "needs_institutional",
          tiers = params$tiers %||% c("high", "intermediate"),
          use_chromote_fallback = isTRUE(params$use_chromote_fallback),
          log = log,
          progress = progress
        )
      },
      extract = {
        manifest <- params$manifest
        if (!nzchar(manifest)) {
          manifest <- ingest_latest_manifest(cfg)
        }
        if (!nzchar(manifest)) stop("No manifest found. Run Discover first.")
        ingest_extract_batch(
          manifest,
          cfg = cfg,
          limit = as.integer(params$limit),
          use_ollama = TRUE,
          assess = isTRUE(params$run_assessment),
          resume = isTRUE(params$resume %||% TRUE),
          log = log,
          progress = progress
        )
      },
      process = {
        manifest <- params$manifest
        if (!nzchar(manifest)) manifest <- ingest_latest_manifest(cfg)
        if (!nzchar(manifest)) stop("No manifest found. Run Discover & triage first.")
        ingest_process_batch(
          manifest, cfg = cfg, tiers = params$tiers %||% c("high", "intermediate"),
          limit = as.integer(params$limit), resume = isTRUE(params$resume %||% TRUE),
          adjudicate = isTRUE(params$adjudicate %||% TRUE), log = log, progress = progress,
          article_progress = article_progress
        )
      },
      count = {
        log("Querying PubMed...", "INFO")
        progress(0.05, "Querying PubMed...", 1L, 2L)
        n <- ingest_entrez_count(params$query, cfg)
        count_msg <- sprintf("Count: %s hits", format(as.integer(n), big.mark = ",", scientific = FALSE))
        log(sprintf("PubMed count: %s", format(as.integer(n), big.mark = ",", scientific = FALSE)), "INFO")
        progress(1, count_msg, 2L, 2L)
        list(count = n, query = params$query, message = count_msg)
      },
      stop("Unknown job: ", params$job)
    )
    final_msg <- if (identical(params$job, "count") && is.list(out) && nzchar(out$message %||% "")) {
      out$message
    } else {
      "Complete"
    }
    ingest_write_progress(params$progress_path, 1, final_msg, status = "done")
    log("Job finished successfully", "INFO")
    list(ok = TRUE, result = out)
  }, error = function(e) {
    log(conditionMessage(e), "ERROR")
    ingest_write_progress(params$progress_path, 0, conditionMessage(e), status = "error")
    list(ok = FALSE, error = conditionMessage(e))
  })
}

#' Bootstrap a GUI worker in a clean background R session
#'
#' Installed packages must be loaded normally. Only a genuine package source
#' tree should be passed to `devtools::load_all()`; treating an installed package
#' directory as source caused GUI jobs to terminate before their progress/error
#' handlers were available.
#' @param params GUI worker parameters.
#' @return The result from [ingest_gui_worker()].
#' @keywords internal
ingest_gui_background_worker <- function(params) {
  source_root <- isTRUE(params$source_root)
  lib_path <- if (is.null(params$lib_path)) "" else as.character(params$lib_path)[[1L]]

  if (source_root) {
    if (!nzchar(lib_path) || !dir.exists(lib_path)) {
      stop("The LibeRary source directory is unavailable to the background worker.", call. = FALSE)
    }
    if (!requireNamespace("devtools", quietly = TRUE)) {
      stop("Install 'devtools' to run the GUI directly from a LibeRary source tree.", call. = FALSE)
    }
    devtools::load_all(lib_path, quiet = TRUE, compile = FALSE)
  } else if (!requireNamespace("LibeRary", quietly = TRUE)) {
    stop("LibeRary is not installed in the background R session.", call. = FALSE)
  }

  params$skip_package_load <- TRUE
  getExportedValue("LibeRary", "ingest_gui_worker")(params)
}

.ingest_gui_cancel_worker <- function(supervisor, progress_path, log_path,
                                      reason = "Cancelled by user") {
  if (is.null(supervisor) || !is.function(supervisor$is_alive) ||
      !isTRUE(supervisor$is_alive())) {
    return(list(cancelled = FALSE, message = "No background job is running."))
  }
  current <- ingest_read_progress(progress_path)
  value <- suppressWarnings(as.numeric(current$value %||% 0))
  if (!length(value) || !is.finite(value[[1L]])) value <- 0
  killed <- tryCatch(supervisor$kill_tree(), error = identity)
  if (inherits(killed, "error") && isTRUE(supervisor$is_alive())) {
    fallback <- tryCatch(supervisor$kill(), error = identity)
    if (inherits(fallback, "error")) {
      return(list(
        cancelled = FALSE,
        message = paste("Unable to stop the background job:", conditionMessage(fallback))
      ))
    }
  }
  try(supervisor$wait(timeout = 2000L), silent = TRUE)
  if (isTRUE(supervisor$is_alive())) {
    return(list(
      cancelled = FALSE,
      message = "The stop request was sent, but the background process is still running."
    ))
  }
  message <- trimws(as.character(reason %||% "Cancelled by user")[[1L]])
  if (!nzchar(message)) message <- "Cancelled by user"
  ingest_write_progress(
    progress_path, value[[1L]], message,
    step = current$step %||% NA_integer_, total = current$total %||% NA_integer_,
    status = "cancelled"
  )
  if (nzchar(log_path %||% "")) {
    ingest_make_logger(log_path)(message, "WARN")
  }
  list(
    cancelled = TRUE,
    message = message,
    value = max(0, min(1, value[[1L]])),
    killed = if (inherits(killed, "error")) NULL else killed
  )
}
