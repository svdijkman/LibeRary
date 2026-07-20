#' Discover PubMed entries and classify acquisition routes
#'
#' Runs E-utilities search (rate-limited), fetches metadata, resolves OA
#' sources, downloads open-access PDFs immediately, and returns manifest rows.
#'
#' @param query PubMed query. Defaults to [ingest_default_query()].
#' @param limit Maximum PMIDs to process.
#' @param cfg Config from [ingest_load_config()] or path to YAML.
#' @param triage If TRUE, assess model probability from title and abstract
#'   before acquisition routing. High and intermediate tiers enter the first
#'   pass; low-probability records are retained in a separate backlog.
#' @param download_oa If TRUE, download `oa_auto` PDFs during discovery.
#' @param log Optional logger `function(msg, level)`.
#' @param progress Optional `function(value, message, step, total)`.
#' @return List with pmids, entries (manifest rows), metadata, summary.
#' @export
ingest_discover <- function(
    query = DEFAULT_PM_QUERY,
    limit = 20L,
    cfg = NULL,
    triage = TRUE,
    download_oa = TRUE,
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

  log("Starting PubMed discover", "INFO")
  log(sprintf("Query: %s", query), "DEBUG")
  log(sprintf("Processing limit: %d", limit), "INFO")
  prog(0.02, "Searching PubMed...", 0L, 4L)

  pmids <- ingest_entrez_search(query, retmax = limit, cfg = cfg)
  log(sprintf("Retrieved %d PMID(s)", length(pmids)), "INFO")
  prog(0.25, "Fetching metadata...", 1L, 4L)

  metadata <- ingest_entrez_fetch_metadata(pmids, cfg = cfg, use_cache = TRUE)
  log("Metadata fetch complete", "INFO")
  prog(0.42, "Triaging titles and abstracts...", 2L, 5L)

  triage_results <- if (isTRUE(triage) && isTRUE(cfg$triage$enabled)) {
    ingest_triage_batch(metadata, cfg = cfg, log = log, progress = function(value, message, step, total) {
      prog(0.42 + 0.22 * value, message, step, total)
    })
  } else {
    stats::setNames(lapply(metadata, function(m) {
      rel <- ingest_score_model_relevance(paste(m$title, m$abstract))
      probability <- min(0.95, rel$score / 4)
      tier <- .library_triage_tier(probability, cfg)
      list(
        model_probability = probability,
        recoverability_probability = probability,
        tier = tier,
        recommended_action = if (tier %in% cfg$triage$first_pass_tiers) "first_pass" else "defer",
        method = "keyword_fallback",
        provider = "none",
        model = "keyword",
        evidence = rel$keywords,
        uncertainty = "LLM triage disabled."
      )
    }), names(metadata))
  }
  prog(0.65, "Classifying acquisition routes...", 3L, 5L)

  n <- length(pmids)
  entries <- lapply(seq_along(pmids), function(i) {
    pid <- pmids[[i]]
    m <- metadata[[pid]]
    if (is.null(m)) return(NULL)
    if (n > 1L) {
      prog(0.65 + 0.2 * (i / n), sprintf("Classifying PMID %s (%d/%d)", pid, i, n), i, n)
    }
    row <- ingest_classify_entry(m, cfg, triage = triage_results[[pid]])
    if (download_oa && identical(row$acquisition_class, "oa_auto") && nzchar(row$suggested_url)) {
      log(sprintf("Downloading OA PDF for PMID %s", row$pmid), "INFO")
      dl <- ingest_download_pdf(row$pmid, row$suggested_url, cfg)
      row$status <- if (dl$success) "fetched" else "fetch_failed"
      row$local_path <- dl$path %||% ""
      row$fetch_error <- dl$error %||% ""
      row$final_url <- dl$final_url %||% row$suggested_url
      log(sprintf("  -> %s", if (dl$success) "OK" else dl$error), if (dl$success) "INFO" else "WARN")
    } else {
      row$local_path <- ingest_inbox_pdf_path(cfg, row$pmid)
      if (nzchar(row$local_path)) row$status <- "fetched"
    }
    row
  })
  entries <- Filter(Negate(is.null), entries)

  entries_df <- ingest_entries_to_df(entries)
  summary <- ingest_summarize_entries(entries_df)
  prog(0.9, "Writing manifest and triage backlog...", 4L, 5L)

  out <- list(
    query = query,
    pmids = pmids,
    entries = entries,
    entries_df = entries_df,
    metadata = metadata,
    triage = triage_results,
    summary = summary,
    discovered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  )

  manifest_path <- file.path(cfg$data_dir, "manifests", sprintf("discover_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")))
  ingest_write_manifest(entries_df, manifest_path)
  out$manifest_path <- manifest_path

  low_backlog <- entries_df[entries_df$triage_tier == "low", , drop = FALSE]
  backlog_path <- file.path(cfg$data_dir, "triage", sprintf("deferred_low_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")))
  ingest_write_manifest(low_backlog, backlog_path)
  out$low_backlog_path <- backlog_path

  log_path <- file.path(cfg$data_dir, "logs", sprintf("discover_%s.json", format(Sys.time(), "%Y%m%d_%H%M%S")))
  jsonlite::write_json(
    list(
      query = query,
      summary = summary,
      manifest_path = manifest_path,
      low_backlog_path = backlog_path,
      pmids = pmids,
      triage = lapply(triage_results, function(x) x$audit %||% x)
    ),
    log_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  out$log_path <- log_path

  log(sprintf(
    "Discover complete: %d total; %d high, %d intermediate, %d deferred low",
    summary$total, summary$high, summary$intermediate, summary$low
  ), "INFO")
  prog(1, "Discover complete", 5L, 5L)

  out
}

ingest_inbox_pdf_path <- function(cfg, pmid) {
  p <- file.path(cfg$inbox_dir, pmid, "article.pdf")
  if (file.exists(p) && file.info(p)$size > 1000) p else ""
}

ingest_entries_to_df <- function(entries) {
  if (!length(entries)) {
    return(data.frame(
      pmid = character(),
      doi = character(),
      pmcid = character(),
      title = character(),
      publisher = character(),
      year = character(),
      suggested_url = character(),
      strategy = character(),
      acquisition_class = character(),
      status = character(),
      relevance_score = integer(),
      relevance_keywords = character(),
      triage_probability = numeric(),
      recoverability_probability = numeric(),
      triage_tier = character(),
      triage_action = character(),
      triage_method = character(),
      triage_provider = character(),
      triage_model = character(),
      triage_evidence = character(),
      triage_uncertainty = character(),
      is_oa = logical(),
      has_abstract = logical(),
      local_path = character(),
      stringsAsFactors = FALSE
    ))
  }
  df <- do.call(rbind, lapply(entries, function(e) {
    data.frame(
      pmid = e$pmid %||% "",
      doi = e$doi %||% "",
      pmcid = e$pmcid %||% "",
      title = e$title %||% "",
      publisher = e$publisher %||% "",
      year = e$year %||% "",
      suggested_url = e$suggested_url %||% "",
      strategy = e$strategy %||% "",
      acquisition_class = e$acquisition_class %||% "",
      status = e$status %||% "pending",
      relevance_score = as.integer(e$relevance_score %||% 0L),
      relevance_keywords = e$relevance_keywords %||% "",
      triage_probability = as.numeric(e$triage_probability %||% 0),
      recoverability_probability = as.numeric(e$recoverability_probability %||% 0),
      triage_tier = e$triage_tier %||% "low",
      triage_action = e$triage_action %||% "defer",
      triage_method = e$triage_method %||% "unknown",
      triage_provider = e$triage_provider %||% "none",
      triage_model = e$triage_model %||% "",
      triage_evidence = e$triage_evidence %||% "",
      triage_uncertainty = e$triage_uncertainty %||% "",
      is_oa = isTRUE(e$is_oa),
      has_abstract = isTRUE(e$has_abstract),
      local_path = e$local_path %||% "",
      stringsAsFactors = FALSE
    )
  }))
  rownames(df) <- NULL
  df
}

ingest_summarize_entries <- function(df) {
  if (!nrow(df)) {
    return(list(total = 0L))
  }
  tier <- if ("triage_tier" %in% names(df)) tolower(df$triage_tier) else rep("", nrow(df))
  list(
    total = nrow(df),
    oa_auto = sum(df$acquisition_class == "oa_auto"),
    needs_institutional = sum(df$acquisition_class == "needs_institutional"),
    stub = sum(df$acquisition_class == "stub"),
    deferred_low = sum(df$acquisition_class == "deferred_low"),
    skipped = sum(df$acquisition_class == "skipped"),
    high = sum(tier == "high"),
    intermediate = sum(tier == "intermediate"),
    low = sum(tier == "low"),
    first_pass = sum(tier %in% c("high", "intermediate")),
    fetched = sum(df$status == "fetched"),
    fetch_failed = sum(df$status == "fetch_failed"),
    pending = sum(df$status == "pending")
  )
}
