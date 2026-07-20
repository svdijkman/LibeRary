#' Load ingest configuration
#'
#' Reads YAML config when the **yaml** package is installed; otherwise uses
#' defaults merged with environment variables.
#'
#' @param path Path to YAML config file. If missing, uses env vars only.
#' @return A normalized config list.
#' @export
ingest_load_config <- function(path = NULL) {
  cfg <- DEFAULT_CONFIG
  if (is.null(path)) {
    candidate <- library_config_path()
    if (file.exists(candidate)) path <- candidate
  }
  if (!is.null(path) && nzchar(path) && file.exists(path)) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("Install the 'yaml' package to load config files, or pass a list to ingest_discover().")
    }
    file_cfg <- yaml::read_yaml(path)
    cfg <- ingest_merge_config(cfg, file_cfg)
  }
  cfg <- ingest_apply_env(cfg)
  cfg <- ingest_validate_config(cfg)
  cfg
}

ingest_merge_config <- function(base, override) {
  if (is.null(override)) return(base)
  for (nm in names(override)) {
    if (is.list(override[[nm]]) && is.list(base[[nm]])) {
      base[[nm]] <- ingest_merge_config(base[[nm]], override[[nm]])
    } else {
      base[[nm]] <- override[[nm]]
    }
  }
  base
}

ingest_apply_env <- function(cfg) {
  if (nzchar(Sys.getenv("LIBERARY_ENTREZ_EMAIL", ""))) {
    cfg$entrez$email <- Sys.getenv("LIBERARY_ENTREZ_EMAIL")
  }
  if (nzchar(Sys.getenv("LIBERARY_ENTREZ_TOOL", ""))) {
    cfg$entrez$tool <- Sys.getenv("LIBERARY_ENTREZ_TOOL")
  }
  if (nzchar(Sys.getenv("ENTREZ_KEY", "")) || nzchar(Sys.getenv("LIBERARY_ENTREZ_API_KEY", ""))) {
    cfg$entrez$api_key <- Sys.getenv("ENTREZ_KEY", Sys.getenv("LIBERARY_ENTREZ_API_KEY", ""))
  }
  if (nzchar(Sys.getenv("LIBERARY_UNPAYWALL_EMAIL", ""))) {
    cfg$unpaywall$email <- Sys.getenv("LIBERARY_UNPAYWALL_EMAIL")
  }
  if (nzchar(Sys.getenv("LIBERARY_DATA_DIR", ""))) {
    cfg$data_dir <- Sys.getenv("LIBERARY_DATA_DIR")
  }
  if (nzchar(Sys.getenv("LIBERARY_INDEXING_PROVIDER", ""))) {
    cfg$llm$indexing$provider <- Sys.getenv("LIBERARY_INDEXING_PROVIDER")
  }
  if (nzchar(Sys.getenv("LIBERARY_INDEXING_MODEL", ""))) {
    cfg$llm$indexing$model <- Sys.getenv("LIBERARY_INDEXING_MODEL")
  }
  if (nzchar(Sys.getenv("LIBERARY_ASSESSMENT_PROVIDER", ""))) {
    cfg$llm$assessment$provider <- Sys.getenv("LIBERARY_ASSESSMENT_PROVIDER")
  }
  if (nzchar(Sys.getenv("LIBERARY_ASSESSMENT_MODEL", ""))) {
    cfg$llm$assessment$model <- Sys.getenv("LIBERARY_ASSESSMENT_MODEL")
  }
  for (role in c("triage", "vision", "adjudication")) {
    prefix <- paste0("LIBERARY_", toupper(role), "_")
    provider <- Sys.getenv(paste0(prefix, "PROVIDER"), "")
    model <- Sys.getenv(paste0(prefix, "MODEL"), "")
    if (nzchar(provider)) cfg$llm[[role]]$provider <- provider
    if (nzchar(model)) cfg$llm[[role]]$model <- model
  }
  if (nzchar(Sys.getenv("LIBERARY_DOCLING", ""))) {
    cfg$docling$executable <- Sys.getenv("LIBERARY_DOCLING")
  }
  if (nzchar(Sys.getenv("LIBERARY_OLLAMA_URL", ""))) {
    cfg$llm$providers$ollama$base_url <- Sys.getenv("LIBERARY_OLLAMA_URL")
  }
  cfg
}

#' Validate and normalize LibeRary configuration
#' @param cfg Configuration list.
#' @return Normalized configuration.
#' @export
ingest_validate_config <- function(cfg) {
  data_root <- library_home()
  cfg$data_dir <- path.expand(cfg$data_dir %||% "")
  if (!nzchar(cfg$data_dir)) cfg$data_dir <- data_root
  if (!nzchar(cfg$unpaywall$email)) {
    cfg$unpaywall$email <- cfg$entrez$email
  }
  cfg$inbox_dir <- path.expand(cfg$inbox_dir %||% "")
  cfg$cache_dir <- path.expand(cfg$cache_dir %||% "")
  cfg$catalog_dir <- path.expand(cfg$catalog_dir %||% "")
  if (!nzchar(cfg$inbox_dir)) cfg$inbox_dir <- file.path(cfg$data_dir, "inbox")
  if (!nzchar(cfg$cache_dir)) cfg$cache_dir <- file.path(cfg$data_dir, "cache")
  if (!nzchar(cfg$catalog_dir)) cfg$catalog_dir <- file.path(cfg$data_dir, "catalog")
  cfg$llm <- ingest_merge_config(DEFAULT_CONFIG$llm, cfg$llm %||% list())
  structured_retries <- suppressWarnings(as.integer(cfg$llm$structured_retries %||% 1L))
  if (!length(structured_retries) || !is.finite(structured_retries[[1L]])) structured_retries <- 1L
  cfg$llm$structured_retries <- max(0L, min(3L, structured_retries[[1L]]))
  for (role in .library_llm_roles()) {
    instruction <- as.character(cfg$llm[[role]]$instruction %||% "")
    cfg$llm[[role]]$instruction <- if (length(instruction) && !is.na(instruction[[1L]])) instruction[[1L]] else ""
  }
  cfg$ollama <- ingest_merge_config(DEFAULT_CONFIG$ollama, cfg$ollama %||% list())
  cfg$triage <- ingest_merge_config(DEFAULT_CONFIG$triage, cfg$triage %||% list())
  cfg$docling <- ingest_merge_config(DEFAULT_CONFIG$docling, cfg$docling %||% list())
  cfg$reproduction <- ingest_merge_config(DEFAULT_CONFIG$reproduction, cfg$reproduction %||% list())
  cfg$deliberative <- ingest_merge_config(DEFAULT_CONFIG$deliberative, cfg$deliberative %||% list())
  high <- suppressWarnings(as.numeric(cfg$triage$high_threshold))
  intermediate <- suppressWarnings(as.numeric(cfg$triage$intermediate_threshold))
  if (!is.finite(high) || !is.finite(intermediate) || intermediate < 0 ||
      high > 1 || intermediate >= high) {
    stop("Triage thresholds must satisfy 0 <= intermediate < high <= 1.", call. = FALSE)
  }
  cfg$triage$high_threshold <- high
  cfg$triage$intermediate_threshold <- intermediate
  tiers <- unique(tolower(as.character(cfg$triage$first_pass_tiers %||% c("high", "intermediate"))))
  if (!length(tiers) || any(!tiers %in% LIBRARY_TRIAGE_TIERS)) {
    stop("`triage$first_pass_tiers` contains an unknown tier.", call. = FALSE)
  }
  cfg$triage$first_pass_tiers <- tiers
  cfg$docling$max_vision_pages <- max(1L, as.integer(cfg$docling$max_vision_pages %||% 12L))
  cfg$docling$render_dpi <- max(72L, as.integer(cfg$docling$render_dpi %||% 140L))
  cfg$docling$timeout_seconds <- max(30L, as.integer(cfg$docling$timeout_seconds %||% 1800L))
  cfg$reproduction$enabled <- isTRUE(cfg$reproduction$enabled)
  cfg$reproduction$auto_run <- isTRUE(cfg$reproduction$auto_run)
  cfg$reproduction$allow_generated_defaults <- isTRUE(cfg$reproduction$allow_generated_defaults)
  cfg$reproduction$nsim <- max(1L, as.integer(cfg$reproduction$nsim %||% 200L))
  cfg$reproduction$seed <- as.integer(cfg$reproduction$seed %||% 20260716L)
  cfg$reproduction$n_cores <- max(1L, as.integer(cfg$reproduction$n_cores %||% 1L))
  cfg$deliberative$enabled <- isTRUE(cfg$deliberative$enabled)
  cfg$deliberative$cache_stages <- isTRUE(cfg$deliberative$cache_stages)
  cfg$deliberative$visual_verification <- isTRUE(cfg$deliberative$visual_verification)
  cfg$deliberative$max_document_chars <- max(50000L,
    as.integer(cfg$deliberative$max_document_chars %||% 500000L))
  cfg$deliberative$chunk_chars <- max(1500L, min(12000L,
    as.integer(cfg$deliberative$chunk_chars %||% 4200L)))
  cfg$deliberative$chunk_overlap <- max(0L, min(
    as.integer(cfg$deliberative$chunk_overlap %||% 350L),
    cfg$deliberative$chunk_chars %/% 3L
  ))
  cfg$deliberative$max_chunks_per_stage <- max(2L, min(20L,
    as.integer(cfg$deliberative$max_chunks_per_stage %||% 8L)))
  cfg$deliberative$max_gap_rounds <- max(0L, min(3L,
    as.integer(cfg$deliberative$max_gap_rounds %||% 1L)))
  cfg$deliberative$ledger_context_chars <- max(8000L,
    as.integer(cfg$deliberative$ledger_context_chars %||% 24000L))
  cfg$deliberative$visual_context_chars <- max(8000L,
    as.integer(cfg$deliberative$visual_context_chars %||% 16000L))
  cfg$deliberative$visual_num_ctx <- max(16384L,
    as.integer(cfg$deliberative$visual_num_ctx %||% 32768L))
  cfg$deliberative$visual_num_predict <- max(4096L,
    as.integer(cfg$deliberative$visual_num_predict %||% 12288L))
  cfg$deliberative$synthesis_context_chars <- max(16000L,
    as.integer(cfg$deliberative$synthesis_context_chars %||% 24000L))
  cfg$deliberative$synthesis_num_ctx <- max(16384L,
    as.integer(cfg$deliberative$synthesis_num_ctx %||% 32768L))
  cfg$deliberative$synthesis_num_predict <- max(4096L,
    as.integer(cfg$deliberative$synthesis_num_predict %||% 16384L))
  cfg$ollama$num_ctx <- max(4096L, as.integer(cfg$ollama$num_ctx %||% 16384L))
  cfg$ollama$num_predict <- max(512L, as.integer(cfg$ollama$num_predict %||% 8192L))
  # Keep prototype configuration working while migrating it to role-based LLMs.
  if (nzchar(cfg$ollama$base_url %||% "") &&
      identical(cfg$llm$providers$ollama$base_url, DEFAULT_CONFIG$llm$providers$ollama$base_url)) {
    cfg$llm$providers$ollama$base_url <- cfg$ollama$base_url
  }
  if (nzchar(cfg$ollama$model %||% "") && !nzchar(cfg$llm$indexing$model %||% "")) {
    cfg$llm$indexing$model <- cfg$ollama$model
  }
  cfg
}

#' Upgrade-safe LibeRary configuration path
#'
#' Secrets are deliberately read from environment variables rather than saved
#' in package files. The returned YAML path survives package upgrades.
#' @param create Create its parent directory.
#' @return Normalized configuration path.
#' @export
library_config_path <- function(create = FALSE) {
  directory <- library_home()
  if (isTRUE(create) && !dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(file.path(directory, "config.yml"), winslash = "/", mustWork = FALSE)
}

#' Save upgrade-safe LibeRary settings
#'
#' API keys are removed before writing; configure their environment-variable
#' names in YAML and put the actual secrets in the worker environment.
#' @param cfg LibeRary configuration.
#' @param path Destination YAML path.
#' @return Path, invisibly.
#' @export
library_save_config <- function(cfg, path = library_config_path(create = TRUE)) {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Install 'yaml' to save LibeRary configuration.")
  cfg <- ingest_validate_config(cfg)
  cfg$entrez$api_key <- ""
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("config-", tmpdir = directory, fileext = ".yml")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  yaml::write_yaml(cfg, temporary)
  if (file.exists(path)) {
    previous <- paste0(path, ".previous"); unlink(previous, force = TRUE)
    if (!file.rename(path, previous)) stop("Unable to rotate LibeRary configuration.")
  }
  if (!file.rename(temporary, path)) {
    if (file.exists(paste0(path, ".previous"))) file.rename(paste0(path, ".previous"), path)
    stop("Unable to save LibeRary configuration.")
  }
  unlink(paste0(path, ".previous"), force = TRUE)
  if (.Platform$OS.type != "windows") Sys.chmod(path, "0600")
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Persistent LibeRary home
#'
#' Defaults to `Documents/LibeR/library` on Windows and `~/LibeR/library` on
#' Linux/macOS, alongside the default LibeRation workspace. Set
#' `LIBERARY_HOME` to override it.
#' @param create Create the directory.
#' @return Normalized directory path.
#' @export
library_home <- function(create = FALSE) {
  root <- Sys.getenv("LIBERARY_HOME", "")
  if (!nzchar(root)) {
    root <- if (.Platform$OS.type == "windows") {
      file.path(Sys.getenv("USERPROFILE", path.expand("~")), "Documents", "LibeR", "library")
    } else file.path(path.expand("~"), "LibeR", "library")
  }
  root <- path.expand(root)
  if (isTRUE(create) && !dir.exists(root)) dir.create(root, recursive = TRUE, showWarnings = FALSE)
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || (is.atomic(x) && length(x) == 1L && is.na(x))) y else x
}

ingest_ensure_dirs <- function(cfg) {
  dirs <- c(
    cfg$data_dir,
    cfg$inbox_dir,
    cfg$cache_dir,
    file.path(cfg$cache_dir, "metadata"),
    file.path(cfg$cache_dir, "unpaywall"),
    file.path(cfg$data_dir, "manifests"),
    file.path(cfg$data_dir, "logs"),
    file.path(cfg$data_dir, "documents"),
    file.path(cfg$data_dir, "triage")
  )
  for (d in unique(dirs)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(cfg)
}
