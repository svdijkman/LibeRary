.library_file_sha256 <- function(path) {
  if (!file.exists(path)) stop("File does not exist: ", path, call. = FALSE)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

.library_docling_cache <- new.env(parent = emptyenv())

.library_find_docling <- function(executable) {
  if (file.exists(executable)) return(executable)
  resolved <- Sys.which(executable)
  if (nzchar(resolved)) return(resolved)
  if (.Platform$OS.type != "windows" || !identical(tolower(executable), "docling")) return("")
  local <- Sys.getenv("LOCALAPPDATA", "")
  roaming <- Sys.getenv("APPDATA", "")
  patterns <- c(
    file.path(local, "Python", "*", "Scripts", "docling.exe"),
    file.path(local, "Programs", "Python", "*", "Scripts", "docling.exe"),
    file.path(roaming, "Python", "*", "Scripts", "docling.exe")
  )
  candidates <- unlist(lapply(patterns[nzchar(patterns)], Sys.glob), use.names = FALSE)
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates)) candidates[[1L]] else ""
}

.library_find_conversion <- function(directory, extension) {
  files <- list.files(directory, pattern = paste0("\\.", extension, "$"),
                      recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(files)) normalizePath(files[[1L]], winslash = "/", mustWork = TRUE) else ""
}

#' Check whether the configured Docling command is available
#'
#' @param cfg LibeRary configuration.
#' @return A list containing `available`, `executable`, and `version`.
#' @export
ingest_docling_available <- function(cfg = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  executable <- as.character(cfg$docling$executable %||% "docling")[[1L]]
  resolved <- .library_find_docling(executable)
  runner <- getOption("LibeRary.docling_runner")
  available <- nzchar(resolved) || is.function(runner)
  version <- if (nzchar(resolved)) .library_docling_cache[[resolved]] %||% "" else ""
  if (available && is.null(runner) && !nzchar(version)) {
    version <- tryCatch(
      paste(suppressWarnings(system2(resolved, "--version", stdout = TRUE, stderr = TRUE,
                                     timeout = 60L)), collapse = " "),
      error = function(e) ""
    )
    if (nzchar(version)) .library_docling_cache[[resolved]] <- version
  }
  list(available = available, executable = if (nzchar(resolved)) resolved else executable,
       version = version)
}

#' Parse a PDF with Docling's standard document pipeline
#'
#' This lane deliberately uses Docling's standard parser rather than its VLM
#' pipeline, keeping it independent from the raw-page vision extraction lane.
#' A test or deployment runner can be supplied as
#' `options(LibeRary.docling_runner = function(executable, args, stdout, stderr) ...)`.
#'
#' @param pdf_path PDF to convert.
#' @param output_dir Conversion output directory.
#' @param cfg LibeRary configuration.
#' @param force Re-run when converted output already exists.
#' @return Conversion manifest.
#' @export
ingest_docling_parse <- function(pdf_path, output_dir, cfg = NULL, force = FALSE) {
  if (!file.exists(pdf_path)) stop("PDF does not exist: ", pdf_path, call. = FALSE)
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  existing_json <- .library_find_conversion(output_dir, "json")
  existing_md <- .library_find_conversion(output_dir, "md")
  if (!isTRUE(force) && (nzchar(existing_json) || nzchar(existing_md))) {
    return(list(success = TRUE, reused = TRUE, parser = "docling_standard",
                json_path = existing_json, markdown_path = existing_md,
                html_path = .library_find_conversion(output_dir, "html"), log_path = ""))
  }

  availability <- ingest_docling_available(cfg)
  if (!isTRUE(availability$available)) {
    return(list(success = FALSE, reused = FALSE, parser = "docling_standard",
                error = "Configured Docling executable is unavailable.",
                executable = availability$executable, version = ""))
  }
  log_path <- file.path(output_dir, "docling.stdout.log")
  error_path <- file.path(output_dir, "docling.stderr.log")
  formats <- intersect(tolower(as.character(cfg$docling$output_formats)), c("json", "md", "html"))
  if (!length(formats)) formats <- c("json", "md")
  args <- c(
    normalizePath(pdf_path, winslash = "/", mustWork = TRUE),
    "--pipeline", as.character(cfg$docling$pipeline %||% "standard"),
    unlist(lapply(formats, function(x) c("--to", x)), use.names = FALSE),
    "--output", normalizePath(output_dir, winslash = "/", mustWork = TRUE),
    "--image-export-mode", "referenced",
    if (isTRUE(cfg$docling$ocr)) "--ocr" else "--no-ocr",
    if (isTRUE(cfg$docling$tables)) "--tables" else "--no-tables",
    "--table-mode", as.character(cfg$docling$table_mode %||% "accurate"),
    "--document-timeout", as.character(cfg$docling$timeout_seconds)
  )
  runner <- getOption("LibeRary.docling_runner")
  status <- tryCatch({
    if (is.function(runner)) {
      runner(availability$executable, args, log_path, error_path)
    } else {
      system2(availability$executable, args, stdout = log_path, stderr = error_path,
              timeout = cfg$docling$timeout_seconds + 30L)
    }
  }, error = identity)
  if (inherits(status, "error")) {
    return(list(success = FALSE, reused = FALSE, parser = "docling_standard",
                error = conditionMessage(status), executable = availability$executable,
                version = availability$version, log_path = log_path, error_path = error_path))
  }
  status <- as.integer(status %||% 0L)
  json_path <- .library_find_conversion(output_dir, "json")
  markdown_path <- .library_find_conversion(output_dir, "md")
  success <- identical(status, 0L) && (nzchar(json_path) || nzchar(markdown_path))
  list(
    success = success, reused = FALSE, parser = "docling_standard",
    status = status, executable = availability$executable, version = availability$version,
    json_path = json_path, markdown_path = markdown_path,
    html_path = .library_find_conversion(output_dir, "html"),
    log_path = log_path, error_path = error_path,
    error = if (success) "" else paste0("Docling conversion failed with status ", status, ".")
  )
}

.library_select_vision_pages <- function(pdf_path, max_pages = 12L) {
  if (!requireNamespace("pdftools", quietly = TRUE)) return(integer())
  total <- as.integer(pdftools::pdf_info(pdf_path)$pages %||% 0L)
  if (!is.finite(total) || total < 1L) return(integer())
  if (total <= max_pages) return(seq_len(total))
  text <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) rep("", total))
  terms <- c("model", "equation", "clearance", "volume", "omega", "sigma",
             "covariate", "nonmem", "parameter estimate", "structural")
  scores <- vapply(text, function(page) sum(vapply(terms, grepl, logical(1),
                                                   x = tolower(page), fixed = TRUE)), numeric(1))
  anchors <- unique(c(1L, total, 2L, max(1L, total - 1L)))
  ranked <- order(scores, decreasing = TRUE)
  unique(sort(utils::head(c(anchors, ranked), max_pages)))
}

#' Render selected original PDF pages for the independent vision lane
#' @param pdf_path PDF path.
#' @param output_dir Page image directory.
#' @param pages One-based page numbers, or NULL for relevance-based selection.
#' @param dpi Image resolution.
#' @param max_pages Maximum automatically selected pages.
#' @return Normalized paths to PNG page images.
#' @export
ingest_render_pdf_pages <- function(pdf_path, output_dir, pages = NULL, dpi = 140L,
                                    max_pages = 12L) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Install the suggested 'pdftools' package to render vision-lane pages.", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(pages)) pages <- .library_select_vision_pages(pdf_path, max_pages)
  pages <- sort(unique(as.integer(pages[is.finite(pages) & pages > 0L])))
  if (!length(pages)) return(character())
  filenames <- file.path(output_dir, sprintf("page-%04d.png", pages))
  missing <- !file.exists(filenames)
  if (any(missing)) {
    pdftools::pdf_convert(pdf_path, format = "png", pages = pages[missing],
                          filenames = file.path(output_dir, "page-%04d.%s"),
                          dpi = as.integer(dpi), verbose = FALSE)
  }
  normalizePath(filenames[file.exists(filenames)], winslash = "/", mustWork = TRUE)
}

#' Build a canonical, content-addressed document bundle
#'
#' The bundle retains the source PDF, standard-parser outputs, selected raw page
#' images, hashes, parser provenance, and fallback status. It is safe to resume:
#' an unchanged PDF reuses its existing bundle.
#'
#' @param metadata Publication metadata.
#' @param pdf_path Full-text PDF.
#' @param cfg LibeRary configuration.
#' @param force Rebuild an existing bundle.
#' @return Bundle manifest.
#' @export
ingest_document_bundle <- function(metadata, pdf_path, cfg = NULL, force = FALSE) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  ingest_ensure_dirs(cfg)
  meta <- ingest_coalesce_metadata(metadata)
  identifier <- if (nzchar(meta$pmid)) meta$pmid else substr(.library_file_sha256(pdf_path), 1L, 16L)
  sha256 <- .library_file_sha256(pdf_path)
  root <- file.path(cfg$data_dir, "documents", identifier, substr(sha256, 1L, 16L))
  manifest_path <- file.path(root, "bundle.json")
  if (!isTRUE(force) && file.exists(manifest_path)) {
    existing <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
    existing$reused <- TRUE
    existing$manifest_path <- normalizePath(manifest_path, winslash = "/", mustWork = TRUE)
    return(existing)
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  source_pdf <- file.path(root, "source.pdf")
  if (!file.exists(source_pdf) || isTRUE(force)) {
    if (!file.copy(pdf_path, source_pdf, overwrite = TRUE, copy.date = TRUE)) {
      stop("Unable to copy PDF into document bundle.", call. = FALSE)
    }
  }
  parse_dir <- file.path(root, "docling")
  parsed <- ingest_docling_parse(source_pdf, parse_dir, cfg, force = force)
  fallback <- FALSE
  if (!isTRUE(parsed$success)) {
    if (!isTRUE(cfg$docling$allow_pdf_text_fallback)) {
      stop(parsed$error %||% "Docling conversion failed.", call. = FALSE)
    }
    fallback <- TRUE
    text <- ingest_pdf_text(source_pdf, max_chars = Inf)
    markdown_path <- file.path(root, "document.fallback.md")
    .library_atomic_write_lines(text, markdown_path)
    fallback_json <- file.path(root, "document.fallback.json")
    .library_atomic_write(list(
      parser = "pdftools_fallback", text = text,
      warning = "Docling unavailable or failed; layout and table structure may be degraded."
    ), fallback_json)
    parsed$markdown_path <- normalizePath(markdown_path, winslash = "/", mustWork = TRUE)
    parsed$json_path <- normalizePath(fallback_json, winslash = "/", mustWork = TRUE)
    parsed$parser <- "pdftools_fallback"
    parsed$primary_error <- parsed$error %||% ""
    parsed$success <- TRUE
  }
  page_dir <- file.path(root, "raw-pages")
  pages <- tryCatch(
    ingest_render_pdf_pages(source_pdf, page_dir, dpi = cfg$docling$render_dpi,
                            max_pages = cfg$docling$max_vision_pages),
    error = function(e) structure(character(), warning = conditionMessage(e))
  )
  page_numbers <- suppressWarnings(as.integer(sub(".*page-([0-9]+)\\.png$", "\\1", pages)))
  manifest <- list(
    schema_version = LIBRARY_SCHEMA_VERSION,
    bundle_version = "1.0.0",
    identifier = identifier,
    pmid = meta$pmid,
    doi = meta$doi,
    title = meta$title,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    source = list(
      path = normalizePath(source_pdf, winslash = "/", mustWork = TRUE),
      sha256 = sha256,
      bytes = unname(file.info(source_pdf)$size)
    ),
    parser = list(
      name = parsed$parser,
      success = isTRUE(parsed$success),
      fallback = fallback,
      version = parsed$version %||% "",
      json_path = parsed$json_path %||% "",
      markdown_path = parsed$markdown_path %||% "",
      html_path = parsed$html_path %||% "",
      error = parsed$error %||% ""
    ),
    vision = list(
      source = "original_pdf_pages",
      page_numbers = page_numbers,
      image_paths = unname(pages),
      dpi = cfg$docling$render_dpi,
      warning = attr(pages, "warning") %||% ""
    ),
    provenance = list(
      acquisition_path = normalizePath(pdf_path, winslash = "/", mustWork = TRUE),
      content_hash = sha256
    ),
    bundle_path = normalizePath(root, winslash = "/", mustWork = TRUE),
    reused = FALSE,
    manifest_path = normalizePath(manifest_path, winslash = "/", mustWork = FALSE)
  )
  .library_atomic_write(manifest, manifest_path)
  manifest$manifest_path <- normalizePath(manifest_path, winslash = "/", mustWork = TRUE)
  manifest
}

#' Read a canonical document bundle
#' @param path Bundle directory or `bundle.json` path.
#' @return Bundle manifest.
#' @export
ingest_read_document_bundle <- function(path) {
  if (dir.exists(path)) path <- file.path(path, "bundle.json")
  if (!file.exists(path)) stop("Bundle manifest not found: ", path, call. = FALSE)
  value <- jsonlite::read_json(path, simplifyVector = FALSE)
  value$manifest_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  value
}
