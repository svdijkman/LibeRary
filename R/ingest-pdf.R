#' Extract text from a PDF file
#'
#' Uses **pdftools** when available.
#'
#' @param path Path to PDF.
#' @param max_chars Truncate extracted text (LLM context limit).
#' @return Character scalar of extracted text.
#' @export
ingest_pdf_text <- function(path, max_chars = 120000L) {
  if (!file.exists(path)) {
    stop("PDF not found: ", path)
  }
  if (!ingest_is_probably_pdf(path)) {
    stop("File is not a valid PDF: ", path)
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Install the 'pdftools' package for PDF text extraction.")
  }
  pages <- pdftools::pdf_text(path)
  pages <- gsub("[ \t]+", " ", pages)
  pages <- gsub("\n{3,}", "\n\n", pages)
  # Long papers are sampled by pharmacometric relevance rather than chopped at
  # an arbitrary character boundary. Front/back matter and the Methods/results
  # pages most likely to contain model details are retained in document order.
  page_numbers <- seq_along(pages)
  if (sum(nchar(pages)) > max_chars) {
    terms <- c("method", "population pharmacokinetic", "pharmacodynamic", "nonmem",
               "model", "parameter", "covariate", "omega", "residual error",
               "clearance", "volume", "supplement")
    score <- vapply(pages, function(page) sum(vapply(terms, grepl, logical(1),
      x = tolower(page), fixed = TRUE)), numeric(1))
    priority <- unique(c(1L, length(pages), order(score, decreasing = TRUE)))
    chosen <- integer(); used <- 0L
    for (index in priority) {
      cost <- nchar(pages[[index]]) + 40L
      if (!length(chosen) || used + cost <= max_chars) {
        chosen <- c(chosen, index); used <- used + cost
      }
    }
    chosen <- sort(unique(chosen))
    pages <- pages[chosen]
    page_numbers <- page_numbers[chosen]
  }
  txt <- paste(sprintf("[PAGE %d]\n%s", page_numbers, pages), collapse = "\n\n")
  txt <- gsub("[ \t]+", " ", txt)
  txt <- gsub("\n{3,}", "\n\n", txt)
  if (nchar(txt) > max_chars) {
    txt <- paste0(substr(txt, 1L, max_chars), "\n\n[TRUNCATED]")
  }
  txt
}

#' Detect supplement control streams in a directory
#'
#' @param dir Directory to scan (for example, `inbox/<pmid>`).
#' @return Character vector of matching file paths.
#' @export
ingest_find_supplements <- function(dir) {
  if (!dir.exists(dir)) return(character())
  pats <- c("\\.ctl$", "\\.mod$", "\\.txt$", "\\.csv$", "\\.zip$")
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  files[grepl(paste(pats, collapse = "|"), files, ignore.case = TRUE)]
}
