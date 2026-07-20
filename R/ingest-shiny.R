#' Launch the LibeRary literature-ingestion application
#'
#' Provides PubMed discovery and abstract triage, full-text acquisition,
#' Docling parsing, independent text/vision extraction, automated adjudication,
#' reviewable catalogue publication,
#' and live background-job logs.
#' @param host Host passed to [shiny::runApp()].
#' @param port Port or `NULL`.
#' @param launch.browser Open a browser.
#' @return The Shiny application, invisibly.
#' @export
ingest_shiny <- function(host = "127.0.0.1", port = NULL, launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) stop("Install 'shiny' to use ingest_shiny().")
  app_dir <- system.file("shiny-ingest", package = "LibeRary")
  root <- library_shiny_pkg_root()
  if ((!nzchar(app_dir) || !dir.exists(app_dir)) && nzchar(root)) app_dir <- file.path(root, "inst", "shiny-ingest")
  if (!dir.exists(app_dir)) stop("The LibeRary ingestion application was not found.")
  if (nzchar(root)) Sys.setenv(LIBERARY_PKG_ROOT = root)
  shiny::runApp(app_dir, host = host, port = port, launch.browser = launch.browser)
}

#' @rdname ingest_shiny
#' @export
library_ingest_shiny <- ingest_shiny
