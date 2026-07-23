#' Launch the LibeRary catalog browser (Shiny)
#'
#' Browse pharmacometric models in the catalog: search, filter, inspect manifests,
#' preview control streams, and import entries into a LibeRation workspace.
#'
#' Requires Suggested packages \pkg{shiny} and \pkg{DT}.
#'
#' @param catalog Root directory of the catalog (see [library_catalog_root()]).
#' @param mode Open the catalogue browser, literature-ingestion workflow, or
#'   reference comparison and curation workflow.
#' @param host Passed to \code{\link[shiny]{runApp}}.
#' @param port Port; \code{NULL} picks a random free port.
#' @param launch.browser Open browser when \code{TRUE}.
#' @param reference_root Optional reference-corpus root used in `reference` mode.
#' @param predictions Optional benchmark prediction directory used in
#'   `reference` mode.
#' @param source_dir Optional source-review directory used to open publication
#'   PDFs in `reference` mode.
#' @return Invisibly, the Shiny app object.
#' @export
library_shiny <- function(
    catalog = NULL,
    mode = c("catalog", "ingest", "reference"),
    host = "127.0.0.1",
    port = NULL,
    launch.browser = TRUE,
    reference_root = NULL,
    predictions = NULL,
    source_dir = NULL) {
  mode <- match.arg(mode)
  if (mode == "ingest") return(ingest_shiny(host, port, launch.browser))
  if (mode == "reference") {
    return(library_reference_shiny(
      corpus = reference_root, predictions = predictions, source_dir = source_dir,
      host = host, port = port, launch.browser = launch.browser
    ))
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install the 'shiny' package to use library_shiny().")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Install the 'DT' package to use library_shiny().")
  }

  app_dir <- system.file("shiny", package = "LibeRary")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    pkg_root <- library_shiny_pkg_root()
    if (nzchar(pkg_root)) {
      app_dir <- file.path(pkg_root, "inst", "shiny")
    }
  }
  if (!dir.exists(app_dir)) {
    stop(
      "Shiny app not found. Run devtools::load_all() from the LibeRary ",
      "package root, then call library_shiny() again."
    )
  }

  pkg_root <- library_shiny_pkg_root()
  if (nzchar(pkg_root)) {
    Sys.setenv(LIBERARY_PKG_ROOT = pkg_root)
  }
  if (!is.null(catalog) && nzchar(catalog)) {
    Sys.setenv(LIBERARY_CATALOG = normalizePath(catalog, winslash = "/", mustWork = FALSE))
  }

  app <- shiny::shinyAppDir(app_dir)
  if (is.null(launch.browser)) return(app)
  shiny::runApp(
    app,
    host = host,
    port = port,
    launch.browser = launch.browser
  )
}

#' Launch the LibeRary reference comparison and curation application
#'
#' Displays the historical systematic-review appendix, normalized reference,
#' and a selected LibeRary extraction side by side. Field decisions are kept in
#' session until the reviewer explicitly creates an immutable successor corpus.
#'
#' @param corpus Reference-corpus root. When `NULL`, the application tries
#'   `LIBERARY_REFERENCE_ROOT` and the local validation directory.
#' @param predictions Prediction directory produced by
#'   [library_reference_run()].
#' @param source_dir Original systematic-review directory used to locate PDFs.
#' @param host Passed to [shiny::runApp()].
#' @param port Port or `NULL`.
#' @param launch.browser Open a browser.
#' @return Invisibly, the Shiny app object.
#' @export
library_reference_shiny <- function(corpus = NULL, predictions = NULL,
                                    source_dir = NULL, host = "127.0.0.1",
                                    port = NULL, launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install the 'shiny' package to use library_reference_shiny().")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Install the 'DT' package to use library_reference_shiny().")
  }
  app_dir <- system.file("shiny-reference", package = "LibeRary")
  pkg_root <- library_shiny_pkg_root()
  if ((!nzchar(app_dir) || !dir.exists(app_dir)) && nzchar(pkg_root)) {
    app_dir <- file.path(pkg_root, "inst", "shiny-reference")
  }
  if (!dir.exists(app_dir)) stop("The LibeRary reference application was not found.")
  if (nzchar(pkg_root)) Sys.setenv(LIBERARY_PKG_ROOT = pkg_root)
  if (!is.null(corpus) && nzchar(corpus)) {
    Sys.setenv(LIBERARY_REFERENCE_ROOT = normalizePath(corpus, winslash = "/", mustWork = FALSE))
  }
  if (!is.null(predictions) && nzchar(predictions)) {
    Sys.setenv(LIBERARY_REFERENCE_PREDICTIONS = normalizePath(predictions, winslash = "/", mustWork = FALSE))
  }
  if (!is.null(source_dir) && nzchar(source_dir)) {
    Sys.setenv(LIBERARY_REFERENCE_SOURCE = normalizePath(source_dir, winslash = "/", mustWork = FALSE))
  }
  app <- shiny::shinyAppDir(app_dir)
  if (is.null(launch.browser)) return(app)
  shiny::runApp(app, host = host, port = port, launch.browser = launch.browser)
}

#' @keywords internal
.library_valid_pkg_root <- function(path) {
  path <- as.character(path %||% "")
  if (!nzchar(path) || !dir.exists(path)) {
    return(FALSE)
  }
  desc_path <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return(FALSE)
  }
  desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  !is.null(desc) &&
    "Package" %in% colnames(desc) &&
    identical(as.character(desc[1, "Package"]), "LibeRary")
}

#' @keywords internal
library_shiny_pkg_root <- function() {
  root <- Sys.getenv("LIBERARY_PKG_ROOT", "")
  if (.library_valid_pkg_root(root)) {
    return(normalizePath(root, winslash = "/", mustWork = FALSE))
  }
  sf <- system.file("", package = "LibeRary")
  if (nzchar(sf)) {
    cand <- normalizePath(sf, winslash = "/", mustWork = FALSE)
    if (.library_valid_pkg_root(cand)) {
      return(cand)
    }
  }
  cand <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (.library_valid_pkg_root(cand)) {
    return(cand)
  }
  ""
}
