#' Rate limiter for external HTTP APIs
#'
#' @param name Throttle namespace (e.g. `"entrez"`, `"unpaywall"`).
#' @param interval Minimum seconds between requests.
#' @keywords internal
ingest_throttle <- local({
  state <- new.env(parent = emptyenv())
  function(name, interval = 1) {
    key <- paste0("last_", name)
    last <- state[[key]]
    if (!is.null(last)) {
      wait <- interval - as.numeric(difftime(Sys.time(), last, units = "secs"))
      if (wait > 0) Sys.sleep(wait)
    }
    state[[key]] <- Sys.time()
    invisible(NULL)
  }
})

ingest_configure_entrez <- function(cfg) {
  if (!nzchar(cfg$entrez$email %||% "")) {
    stop("Set `entrez$email` in LibeRary config or LIBERARY_ENTREZ_EMAIL before querying NCBI.", call. = FALSE)
  }
  Sys.setenv(
    ENTREZ_TOOL = cfg$entrez$tool,
    ENTREZ_EMAIL = cfg$entrez$email
  )
  api_key <- cfg$entrez$api_key %||% ""
  if (nzchar(api_key)) {
    Sys.setenv(ENTREZ_KEY = api_key)
  }
  invisible(cfg)
}

ingest_entrez_interval <- function(cfg) {
  rps <- cfg$entrez$requests_per_second %||% 1
  if (!is.numeric(rps) || rps <= 0) return(1)
  1 / rps
}

ingest_unpaywall_interval <- function(cfg) {
  rps <- cfg$unpaywall$requests_per_second %||% 1
  if (!is.numeric(rps) || rps <= 0) return(1)
  1 / rps
}

ingest_europe_pmc_interval <- function(cfg) {
  rps <- cfg$europe_pmc$requests_per_second %||% 1
  if (!is.numeric(rps) || rps <= 0) return(1)
  1 / rps
}
