#' Search PubMed via E-utilities
#'
#' @param query PubMed query string.
#' @param retmax Maximum PMIDs to return.
#' @param cfg Ingest config from [ingest_load_config()].
#' @return Character vector of PMIDs.
#' @export
ingest_entrez_search <- function(query, retmax = 20L, cfg) {
  ingest_configure_entrez(cfg)
  ingest_throttle("entrez", ingest_entrez_interval(cfg))
  res <- rentrez::entrez_search(
    db = "pubmed",
    term = query,
    retmax = as.integer(retmax),
    use_history = FALSE
  )
  unlist(res$ids, use.names = FALSE)
}

#' Count PubMed hits for a query (no metadata fetch)
#'
#' @param query PubMed query string.
#' @param cfg Ingest config.
#' @return Integer total hit count reported by NCBI.
#' @export
ingest_entrez_count <- function(query, cfg) {
  ingest_configure_entrez(cfg)
  ingest_throttle("entrez", ingest_entrez_interval(cfg))
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(60, old_timeout %||% 60))
  res <- tryCatch(
    rentrez::entrez_search(
      db = "pubmed",
      term = query,
      retmax = 0L,
      use_history = FALSE
    ),
    error = function(e) {
      stop("PubMed count request failed: ", conditionMessage(e), call. = FALSE)
    }
  )
  as.integer(res$count)
}

#' Fetch PubMed metadata for PMIDs
#'
#' @param pmids Character vector of PMIDs.
#' @param cfg Ingest config.
#' @param use_cache If TRUE, read/write per-PMID JSON cache.
#' @return A list of metadata records (named by PMID).
#' @export
ingest_entrez_fetch_metadata <- function(pmids, cfg, use_cache = TRUE) {
  pmids <- unique(as.character(pmids))
  pmids <- pmids[nzchar(pmids)]
  if (!length(pmids)) return(list())

  ingest_configure_entrez(cfg)
  cache_dir <- file.path(cfg$cache_dir, "metadata")
  out <- vector("list", length(pmids))
  names(out) <- pmids

  pending <- pmids
  if (use_cache) {
    for (pid in pmids) {
      cache_path <- file.path(cache_dir, paste0(pid, ".json"))
      if (file.exists(cache_path)) {
        out[[pid]] <- jsonlite::fromJSON(cache_path, simplifyVector = FALSE)
        pending <- setdiff(pending, pid)
      }
    }
  }

  if (length(pending)) {
    batch_size <- 50L
    idx <- seq_len(length(pending))
    batches <- split(pending, ceiling(seq_along(idx) / batch_size))
    for (batch in batches) {
      ingest_throttle("entrez", ingest_entrez_interval(cfg))
      xml <- rentrez::entrez_fetch(
        db = "pubmed",
        id = batch,
        rettype = "xml",
        parsed = FALSE
      )
      parsed <- ingest_parse_pubmed_xml(xml)
      for (pid in names(parsed)) {
        out[[pid]] <- parsed[[pid]]
        if (use_cache) {
          cache_path <- file.path(cache_dir, paste0(pid, ".json"))
          jsonlite::write_json(parsed[[pid]], cache_path, auto_unbox = TRUE, pretty = TRUE)
        }
      }
    }
  }
  out
}

ingest_parse_pubmed_xml <- function(xml) {
  doc <- xml2::read_xml(xml)
  articles <- xml2::xml_find_all(doc, ".//PubmedArticle")
  records <- lapply(articles, ingest_parse_pubmed_article)
  pmids <- vapply(records, function(r) r$pmid %||% "", character(1))
  names(records) <- pmids
  records
}

ingest_parse_pubmed_article <- function(article) {
  pmid <- xml2::xml_text(xml2::xml_find_first(article, ".//PMID"))
  title <- ingest_xml_text_flat(xml2::xml_find_first(article, ".//ArticleTitle"))
  abstract_nodes <- xml2::xml_find_all(article, ".//AbstractText")
  abstract <- if (length(abstract_nodes)) {
    paste(vapply(abstract_nodes, ingest_xml_text_flat, character(1)), collapse = " ")
  } else {
    ""
  }
  journal <- xml2::xml_text(xml2::xml_find_first(article, ".//Journal/Title"))
  year <- xml2::xml_text(xml2::xml_find_first(article, ".//PubDate/Year"))
  if (!nzchar(year)) {
    medline_date <- xml2::xml_text(xml2::xml_find_first(article, ".//PubDate/MedlineDate"))
    year <- sub("^([0-9]{4}).*", "\\1", medline_date)
  }

  doi <- ingest_extract_article_id(article, "doi")
  pmcid_raw <- ingest_extract_article_id(article, "pmc")
  pmcid <- if (nzchar(pmcid_raw)) sub("^PMC", "", pmcid_raw, ignore.case = TRUE) else ""

  authors <- xml2::xml_find_all(article, ".//Author")
  author_list <- vapply(authors, function(a) {
    last <- xml2::xml_text(xml2::xml_find_first(a, "LastName"))
    fore <- xml2::xml_text(xml2::xml_find_first(a, "ForeName"))
    if (nzchar(last) && nzchar(fore)) paste(fore, last) else last
  }, character(1))
  author_list <- author_list[nzchar(author_list)]

  list(
    pmid = pmid,
    title = title,
    abstract = abstract,
    journal = journal,
    year = year,
    doi = doi,
    pmcid = pmcid,
    authors = author_list,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  )
}

ingest_extract_article_id <- function(article, id_type) {
  nodes <- xml2::xml_find_all(article, ".//ArticleId")
  for (node in nodes) {
    type <- xml2::xml_attr(node, "IdType")
    if (tolower(type) == tolower(id_type)) {
      return(xml2::xml_text(node))
    }
  }
  ""
}

ingest_xml_text_flat <- function(node) {
  if (inherits(node, "xml_missing")) return("")
  gsub("\\s+", " ", xml2::xml_text(node))
}
