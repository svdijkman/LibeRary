#' Look up open-access locations via Unpaywall
#'
#' @param doi DOI string.
#' @param cfg Ingest config.
#' @param use_cache Use disk cache under cache/unpaywall.
#' @return List with is_oa, url_for_pdf, url, host_type, or NULL on failure.
#' @export
ingest_unpaywall_lookup <- function(doi, cfg, use_cache = TRUE) {
  doi <- ingest_normalize_doi(doi)
  if (!nzchar(doi)) return(NULL)

  cache_dir <- file.path(cfg$cache_dir, "unpaywall")
  safe_name <- gsub("[^A-Za-z0-9._-]", "_", doi)
  cache_path <- file.path(cache_dir, paste0(safe_name, ".json"))
  if (use_cache && file.exists(cache_path)) {
    return(jsonlite::fromJSON(cache_path, simplifyVector = FALSE))
  }

  ingest_throttle("unpaywall", ingest_unpaywall_interval(cfg))
  req <- httr2::request("https://api.unpaywall.org/v2/") |>
    httr2::req_url_path_append(doi) |>
    httr2::req_url_query(email = cfg$unpaywall$email) |>
    httr2::req_user_agent(cfg$fetch$user_agent %||% "LibeRary/0.7.3") |>
    httr2::req_timeout(cfg$fetch$timeout_seconds %||% 120L)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) return(NULL)

  body <- httr2::resp_body_json(resp)
  loc <- body$best_oa_location
  out <- list(
    doi = doi,
    is_oa = isTRUE(body$is_oa),
    url_for_pdf = loc$url_for_pdf %||% "",
    url = loc$url %||% "",
    host_type = loc$host_type %||% "",
    license = loc$license %||% "",
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  )
  if (use_cache) {
    jsonlite::write_json(out, cache_path, auto_unbox = TRUE, pretty = TRUE)
  }
  out
}

#' Check PMC open-access availability and PDF link
#'
#' @param pmcid PMC ID with or without prefix.
#' @param cfg Ingest config.
#' @return List with is_oa, pdf_url.
#' @export
ingest_pmc_oa_lookup <- function(pmcid, cfg) {
  pmcid <- sub("^PMC", "", pmcid, ignore.case = TRUE)
  if (!nzchar(pmcid)) {
    return(list(is_oa = FALSE, pdf_url = ""))
  }
  ingest_throttle("entrez", ingest_entrez_interval(cfg))
  req <- httr2::request("https://www.ncbi.nlm.nih.gov/pmc/utils/oa/oa.fcgi") |>
    httr2::req_url_query(id = paste0("PMC", pmcid)) |>
    httr2::req_user_agent(cfg$fetch$user_agent %||% "LibeRary/0.7.3") |>
    httr2::req_timeout(cfg$fetch$timeout_seconds %||% 120L)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) {
    return(list(is_oa = FALSE, pdf_url = ""))
  }
  doc <- xml2::read_xml(httr2::resp_body_string(resp))
  link <- xml2::xml_find_first(doc, ".//link[@format='pdf']")
  if (inherits(link, "xml_missing")) {
    return(list(is_oa = FALSE, pdf_url = ""))
  }
  href <- xml2::xml_attr(link, "href")
  href <- as.character(href %||% "")[[1L]]
  # PMC's OA API still emits FTP URLs. HTTPS serves the same archive and is
  # considerably more reliable on institutional and restricted networks.
  href <- sub("^ftp://", "https://", href, ignore.case = TRUE)
  list(is_oa = nzchar(href), pdf_url = href)
}

#' Build deterministic PMC PDF URL
#'
#' @param pmcid PMC ID with or without PMC prefix.
#' @return URL string or empty if no PMCID.
#' @export
ingest_pmc_pdf_url <- function(pmcid) {
  pmcid <- sub("^PMC", "", pmcid, ignore.case = TRUE)
  if (!nzchar(pmcid)) return("")
  sprintf("https://pmc.ncbi.nlm.nih.gov/articles/PMC%s/pdf/", pmcid)
}

#' Europe PMC lookup for full-text PDF hints
#'
#' @param pmid PubMed ID.
#' @param doi Optional DOI.
#' @param cfg Ingest config.
#' @return List with pdf_url, is_open_access, pmcid.
#' @export
ingest_europe_pmc_lookup <- function(pmid = "", doi = "", cfg) {
  query <- if (nzchar(pmid)) {
    sprintf("EXT_ID:%s AND SRC:MED", pmid)
  } else if (nzchar(doi)) {
    sprintf("DOI:\"%s\"", ingest_normalize_doi(doi))
  } else {
    return(list(pdf_url = "", is_open_access = FALSE, pmcid = ""))
  }

  ingest_throttle("europe_pmc", ingest_europe_pmc_interval(cfg))
  req <- httr2::request("https://www.ebi.ac.uk/europepmc/webservices/rest/search") |>
    httr2::req_url_query(
      query = query,
      format = "json",
      pageSize = 1L,
      resultType = "core"
    ) |>
    httr2::req_user_agent(cfg$fetch$user_agent %||% "LibeRary/0.7.3") |>
    httr2::req_timeout(cfg$fetch$timeout_seconds %||% 120L)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) {
    return(list(pdf_url = "", is_open_access = FALSE, pmcid = ""))
  }

  body <- httr2::resp_body_json(resp)
  results <- body$resultList$result
  if (is.null(results) || length(results) < 1L) {
    return(list(pdf_url = "", is_open_access = FALSE, pmcid = ""))
  }
  hit <- results[[1]]

  pmcid <- hit$pmcid %||% ""
  pdf_url <- ""
  if (isTRUE(hit$isOpenAccess == "Y") && nzchar(pmcid)) {
    pdf_url <- ingest_pmc_pdf_url(pmcid)
  } else if (nzchar(hit$pmcid %||% "")) {
    pdf_url <- ingest_pmc_pdf_url(pmcid)
  }

  list(
    pdf_url = pdf_url,
    is_open_access = isTRUE(hit$isOpenAccess == "Y"),
    pmcid = sub("^PMC", "", pmcid, ignore.case = TRUE),
    source = "europe_pmc",
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  )
}

ingest_normalize_doi <- function(doi) {
  doi <- trimws(doi)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi
}

#' Download a PDF to inbox for a PMID
#'
#' @param pmid PMID string.
#' @param url URL to download.
#' @param cfg Ingest config.
#' @param dest_name Destination filename inside `inbox/<pmid>/`.
#' @return List with success, path, final_url, error.
#' @export
ingest_download_pdf <- function(pmid, url, cfg, dest_name = "article.pdf") {
  inbox <- file.path(cfg$inbox_dir, pmid)
  if (!dir.exists(inbox)) dir.create(inbox, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(inbox, dest_name)

  if (file.exists(dest) && file.info(dest)$size > 1000) {
    return(list(success = TRUE, path = dest, final_url = url, skipped = TRUE, error = ""))
  }

  req <- httr2::request(url) |>
    httr2::req_user_agent(cfg$fetch$user_agent %||% "LibeRary/0.7.3") |>
    httr2::req_timeout(cfg$fetch$timeout_seconds %||% 120L) |>
    httr2::req_headers(Accept = "application/pdf,*/*")

  resp <- tryCatch(
    httr2::req_perform(req, path = dest),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    if (file.exists(dest)) unlink(dest)
    return(list(success = FALSE, path = NA_character_, final_url = url, error = conditionMessage(resp)))
  }

  status <- httr2::resp_status(resp)
  final_url <- tryCatch(httr2::resp_url(resp), error = function(e) url)
  size <- if (file.exists(dest)) file.info(dest)$size else 0L
  ok <- status < 400L && size > 1000L && ingest_is_probably_pdf(dest)

  if (!ok) {
    err <- sprintf("HTTP %s; downloaded %s bytes (not a PDF?)", status, size)
    if (file.exists(dest)) unlink(dest)
    return(list(success = FALSE, path = NA_character_, final_url = final_url, error = err))
  }

  list(success = TRUE, path = dest, final_url = final_url, skipped = FALSE, error = "")
}

ingest_is_probably_pdf <- function(path) {
  if (!file.exists(path)) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = raw(0), n = 5L)
  identical(as.character(sig), as.character(charToRaw("%PDF-")))
}
