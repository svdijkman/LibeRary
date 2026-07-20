#' Default PubMed query for pharmacometric literature
#'
#' @return Character scalar PubMed query string.
#' @export
ingest_default_query <- function() {
  DEFAULT_PM_QUERY
}

#' Create a file-backed logger for GUI / background jobs
#'
#' @param log_path Path to append log lines.
#' @return Function `function(msg, level = "INFO")`.
#' @export
ingest_make_logger <- function(log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  function(msg, level = "INFO") {
    line <- sprintf("[%s] %s: %s", format(Sys.time(), "%H:%M:%S"), level, msg)
    cat(line, "\n", file = log_path, append = TRUE)
    invisible(line)
  }
}

#' Write GUI progress state
#'
#' @param progress_path JSON file path.
#' @param value Progress fraction 0–1.
#' @param message Status message.
#' @param step Optional current step index.
#' @param total Optional total steps.
#' @param status One of `idle`, `running`, `done`, `error`, or `cancelled`.
#' @param current Optional nested progress record for the current PMID. When
#'   omitted, an existing current-PMID record is retained.
#' @export
ingest_write_progress <- function(
    progress_path,
    value,
    message,
    step = NA_integer_,
    total = NA_integer_,
    status = "running",
    current = NULL) {
  allowed_status <- c("idle", "running", "done", "error", "cancelled")
  if (!status %in% allowed_status) {
    stop("Unknown GUI progress status: ", status, call. = FALSE)
  }
  dir.create(dirname(progress_path), recursive = TRUE, showWarnings = FALSE)
  value <- suppressWarnings(as.numeric(value))
  if (!length(value) || !is.finite(value[[1L]])) value <- 0
  batch <- list(
    value = max(0, min(1, value[[1L]])),
    message = .library_semantic_scalar(message),
    step = step,
    total = total,
    status = status
  )
  if (is.null(current) && file.exists(progress_path)) {
    previous <- ingest_read_progress(progress_path)
    current <- previous$current %||% NULL
  }
  if (is.null(current)) {
    current <- list(value = 0, message = "Waiting for an article stage", step = NA_integer_,
                    total = NA_integer_, pmid = "", stage = "idle", status = "idle")
  }
  if (status %in% c("done", "error", "cancelled") && identical(current$status %||% "", "running")) {
    current$status <- status
  }
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  # Keep the legacy top-level fields for callers and older GUIs while exposing
  # explicit batch/current channels to newer clients.
  .library_atomic_write(c(batch, list(batch = batch, current = current, updated_at = now)), progress_path)
  invisible(NULL)
}

.ingest_write_current_progress <- function(progress_path, value, message,
                                            step = NA_integer_, total = NA_integer_,
                                            pmid = "", stage = "", status = "running") {
  previous <- ingest_read_progress(progress_path)
  batch <- previous$batch %||% previous %||% list(
    value = 0, message = "Starting job...", step = NA_integer_, total = NA_integer_, status = "running"
  )
  value <- suppressWarnings(as.numeric(value))
  if (!length(value) || !is.finite(value[[1L]])) value <- 0
  current <- list(
    value = max(0, min(1, value[[1L]])),
    message = .library_semantic_scalar(message),
    step = step,
    total = total,
    pmid = .library_semantic_scalar(pmid),
    stage = .library_semantic_scalar(stage),
    status = status
  )
  ingest_write_progress(
    progress_path, batch$value %||% 0, batch$message %||% "",
    batch$step %||% NA_integer_, batch$total %||% NA_integer_,
    batch$status %||% "running", current = current
  )
}

#' Read GUI progress state
#'
#' @param progress_path JSON file path.
#' @return List or NULL.
#' @export
ingest_read_progress <- function(progress_path) {
  if (!file.exists(progress_path)) return(NULL)
  tryCatch(jsonlite::fromJSON(progress_path, simplifyVector = FALSE), error = function(e) NULL)
}

#' Find most recent discover manifest
#'
#' @param cfg Ingest config list.
#' @return File path or `""`.
#' @export
ingest_latest_manifest <- function(cfg) {
  man_dir <- file.path(cfg$data_dir, "manifests")
  if (!dir.exists(man_dir)) return("")
  files <- list.files(man_dir, pattern = "^discover_.*\\.csv$", full.names = TRUE)
  if (!length(files)) return("")
  files[which.max(file.info(files)$mtime)]
}
