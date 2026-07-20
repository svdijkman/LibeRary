#' Fetch pending institutional (paywalled) PDFs
#'
#' Attempts direct PDF URLs when known, otherwise follows DOI redirects on
#' the institutional/VPN network. Optional chromote fallback for publisher
#' pages that require JavaScript.
#'
#' @param manifest Path to manifest CSV or data frame.
#' @param cfg Ingest config.
#' @param classes Character vector of acquisition_class values to fetch.
#'   Default: `"needs_institutional"`.
#' @param tiers Probability tiers to fetch. Defaults to the configured first
#'   pass (`high` and `intermediate`). Use `"low"` to process the retained
#'   backlog later.
#' @param use_chromote_fallback Use chromote when direct download fails.
#' @param log Optional logger function.
#' @param progress Optional progress callback.
#' @return List with results, summary, failures, log_path.
#' @export
ingest_fetch_institutional <- function(
    manifest,
    cfg = NULL,
    classes = "needs_institutional",
    tiers = NULL,
    use_chromote_fallback = NULL,
    log = NULL,
    progress = NULL) {
  log <- log %||% function(...) invisible(NULL)
  prog <- function(value, message, step = NA_integer_, total = NA_integer_) {
    if (!is.null(progress)) progress(value, message, step, total)
  }
  if (is.character(cfg) && length(cfg) == 1L) {
    cfg <- ingest_load_config(cfg)
  } else if (is.null(cfg)) {
    cfg <- ingest_load_config()
  }
  ingest_ensure_dirs(cfg)

  if (is.character(manifest) && length(manifest) == 1L && file.exists(manifest)) {
    df <- ingest_read_manifest(manifest)
  } else if (is.data.frame(manifest)) {
    df <- manifest
  } else {
    stop("`manifest` must be a CSV path or data frame.")
  }

  if (is.null(use_chromote_fallback)) {
    use_chromote_fallback <- isTRUE(cfg$fetch$use_chromote_fallback)
  }

  tiers <- tolower(as.character(tiers %||% cfg$triage$first_pass_tiers))
  bad_tiers <- setdiff(tiers, LIBRARY_TRIAGE_TIERS)
  if (length(bad_tiers)) stop("Unknown triage tier(s): ", paste(bad_tiers, collapse = ", "))

  todo <- df[df$acquisition_class %in% classes & df$status != "fetched", , drop = FALSE]
  if ("triage_tier" %in% names(todo)) {
    todo <- todo[tolower(todo$triage_tier) %in% tiers, , drop = FALSE]
  }
  log(sprintf("Institutional fetch: %d row(s) to process", nrow(todo)), "INFO")
  if (!"fetch_error" %in% names(df)) df$fetch_error <- ""
  if (!"final_url" %in% names(df)) df$final_url <- ""
  results <- vector("list", nrow(todo))

  for (i in seq_len(nrow(todo))) {
    row <- todo[i, , drop = FALSE]
    pmid <- row$pmid
    prog((i - 1) / max(1, nrow(todo)), sprintf("Fetching PMID %s (%d/%d)", pmid, i, nrow(todo)), i, nrow(todo))
    log(sprintf("Fetching PMID %s via %s", pmid, row$strategy), "INFO")
    existing <- ingest_inbox_pdf_path(cfg, pmid)
    if (nzchar(existing)) {
      log(sprintf("  -> skipped (already have PDF) %s", pmid), "INFO")
      results[[i]] <- list(pmid = pmid, success = TRUE, path = existing, skipped = TRUE, strategy = row$strategy)
      next
    }

    res <- ingest_fetch_one_row(row, cfg, use_chromote_fallback = use_chromote_fallback)
    log(sprintf("  -> %s", if (isTRUE(res$success)) "OK" else res$error), if (isTRUE(res$success)) "INFO" else "WARN")
    results[[i]] <- res
    match_idx <- which(df$pmid == pmid)[1]
    if (!is.na(match_idx)) {
      df$status[match_idx] <- if (res$success) "fetched" else "fetch_failed"
      df$local_path[match_idx] <- res$path %||% ""
      if (nzchar(res$error %||% "")) {
        df$fetch_error[match_idx] <- res$error
      }
      if (nzchar(res$final_url %||% "")) {
        df$final_url[match_idx] <- res$final_url
      }
    }
  }

  if (is.character(manifest) && file.exists(manifest)) {
    ingest_write_manifest(df, manifest)
  }

  failures <- Filter(function(x) !isTRUE(x$success), results)
  summary <- list(
    attempted = length(results),
    success = sum(vapply(results, function(x) isTRUE(x$success), logical(1))),
    failed = length(failures),
    skipped = sum(vapply(results, function(x) isTRUE(x$skipped), logical(1)))
  )

  log_path <- file.path(cfg$data_dir, "logs", sprintf("fetch_%s.json", format(Sys.time(), "%Y%m%d_%H%M%S")))
  jsonlite::write_json(
    list(summary = summary, results = results),
    log_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  log(sprintf("Fetch done: %d success, %d failed", summary$success, summary$failed), "INFO")
  prog(1, "Fetch complete", nrow(todo), nrow(todo))

  list(results = results, summary = summary, failures = failures, log_path = log_path, manifest = df)
}

ingest_fetch_one_row <- function(row, cfg, use_chromote_fallback = FALSE) {
  pmid <- row$pmid
  strategy <- row$strategy
  url <- row$suggested_url

  if (!nzchar(url)) {
    return(list(pmid = pmid, success = FALSE, path = NA_character_, error = "No suggested_url", strategy = strategy))
  }

  if (strategy %in% c("pmc_pdf", "oa_pdf")) {
    dl <- ingest_download_pdf(pmid, url, cfg)
    if (dl$success) {
      return(list(pmid = pmid, success = TRUE, path = dl$path, final_url = dl$final_url, strategy = strategy, error = ""))
    }
  }

  if (strategy == "doi_follow" || strategy == "publisher_landing") {
    dl <- ingest_download_pdf(pmid, url, cfg)
    if (dl$success) {
      return(list(pmid = pmid, success = TRUE, path = dl$path, final_url = dl$final_url, strategy = strategy, error = ""))
    }
    if (use_chromote_fallback) {
      fb <- ingest_fetch_via_chromote(pmid, url, cfg)
      if (fb$success) return(c(list(pmid = pmid, strategy = strategy), fb))
    }
    return(list(pmid = pmid, success = FALSE, path = NA_character_, final_url = dl$final_url, strategy = strategy, error = dl$error))
  }

  list(pmid = pmid, success = FALSE, path = NA_character_, error = sprintf("Unsupported strategy: %s", strategy), strategy = strategy)
}

#' Fetch PDF via headless Chrome (optional fallback)
#'
#' @param pmid PMID.
#' @param url Landing page URL (typically DOI).
#' @param cfg Config.
#' @return List with success, path, error.
#' @keywords internal
ingest_fetch_via_chromote <- function(pmid, url, cfg) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return(list(success = FALSE, path = NA_character_, error = "chromote not installed", final_url = url))
  }
  inbox <- file.path(cfg$inbox_dir, pmid)
  if (!dir.exists(inbox)) dir.create(inbox, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(inbox, "article.pdf")

  b <- NULL
  ok <- FALSE
  err <- ""
  final_url <- url
  tryCatch({
    b <- chromote::ChromoteSession$new()
    b$Page$navigate(url)
    Sys.sleep(3)
    final_url <- b$Runtime$evaluate("window.location.href")$result$value
    # Try common PDF link patterns
    pdf_href <- b$Runtime$evaluate(
      "(() => {",
      "  const sel = ['a[href*=\".pdf\"]', 'a[data-article-url*=\"pdf\"]', 'a[title=\"PDF\"]'];",
      "  for (const s of sel) { const a = document.querySelector(s); if (a && a.href) return a.href; }",
      "  return '';",
      "})()"
    )$result$value
    if (nzchar(pdf_href)) {
      dl <- ingest_download_pdf(pmid, pdf_href, cfg)
      ok <- dl$success
      err <- dl$error
      if (ok) return(list(success = TRUE, path = dl$path, final_url = dl$final_url, error = ""))
    }
    err <- "No PDF link found via chromote"
  }, error = function(e) {
    err <<- conditionMessage(e)
  }, finally = {
    if (!is.null(b)) try(b$close(), silent = TRUE)
  })

  list(success = ok, path = if (ok) dest else NA_character_, final_url = final_url, error = err)
}
