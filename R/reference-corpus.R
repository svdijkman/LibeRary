.library_reference_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

.library_reference_text <- function(value) {
  value <- paste(as.character(unlist(value %||% "", use.names = FALSE)), collapse = " ")
  value <- gsub("[\r\n\t]+", " ", value)
  trimws(gsub("[[:space:]]+", " ", value))
}

.library_reference_relpath <- function(path, root) {
  if (is.null(path) || !length(path) || is.na(path[[1L]]) || !nzchar(path[[1L]])) return("")
  path <- normalizePath(path[[1L]], winslash = "/", mustWork = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(tolower(root), "/")
  if (!startsWith(tolower(path), prefix)) stop("Reference source path escaped its root: ", path, call. = FALSE)
  substring(path, nchar(root) + 2L)
}

.library_reference_hash <- function(path) {
  if (!file.exists(path)) return("")
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

.library_reference_schema_dir <- function() {
  installed <- system.file("schema", package = "LibeRary")
  if (nzchar(installed) && dir.exists(installed)) return(installed)
  candidates <- c(file.path(getwd(), "inst", "schema"),
                  file.path(getwd(), "LibeRary", "inst", "schema"))
  hit <- candidates[dir.exists(candidates)][1L]
  if (is.na(hit)) "" else normalizePath(hit, winslash = "/", mustWork = TRUE)
}

.library_reference_docx_tables <- function(path) {
  if (!file.exists(path)) stop("Appendix document not found: ", path, call. = FALSE)
  temporary <- tempfile("liberary-reference-docx-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  extracted <- utils::unzip(path, files = "word/document.xml", exdir = temporary)
  if (!length(extracted) || !file.exists(extracted[[1L]])) {
    stop("Unable to read word/document.xml from ", path, call. = FALSE)
  }
  document <- xml2::read_xml(extracted[[1L]])
  namespace <- xml2::xml_ns(document)
  tables <- xml2::xml_find_all(document, ".//w:tbl", namespace)
  lapply(tables, function(table) {
    rows <- xml2::xml_find_all(table, "./w:tr", namespace)
    lapply(rows, function(row) {
      cells <- xml2::xml_find_all(row, "./w:tc", namespace)
      vapply(cells, function(cell) {
        paragraphs <- xml2::xml_find_all(cell, ".//w:p", namespace)
        text <- vapply(paragraphs, function(paragraph) {
          paste(xml2::xml_text(xml2::xml_find_all(paragraph, ".//w:t", namespace)), collapse = "")
        }, character(1))
        .library_reference_text(paste(text, collapse = " "))
      }, character(1))
    })
  })
}

.library_reference_key <- function(value) {
  value <- .library_reference_text(value)
  pmid_match <- regexpr("(?<![0-9])([0-9]{5,9})(?![0-9])", value, perl = TRUE)
  if (pmid_match[[1L]] < 0L) return(NULL)
  pmid <- regmatches(value, pmid_match)[[1L]]
  prefix <- trimws(substr(value, 1L, pmid_match[[1L]] - 1L))
  year_match <- gregexpr("(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])", prefix, perl = TRUE)[[1L]]
  year <- ""
  author <- prefix
  if (year_match[[1L]] > 0L) {
    position <- utils::tail(year_match, 1L)
    year <- substr(prefix, position, position + attr(year_match, "match.length")[[length(year_match)]] - 1L)
    author <- trimws(substr(prefix, 1L, position - 1L))
  }
  list(pmid = pmid, year = year, first_author = author, raw = value)
}

.library_reference_rows <- function(table) {
  if (!length(table)) return(list())
  rows <- vector("list", 0L)
  for (index in seq_along(table)[-1L]) {
    cells <- table[[index]]
    if (!length(cells)) next
    key <- .library_reference_key(cells[[1L]])
    if (is.null(key)) next
    rows[[length(rows) + 1L]] <- c(key, list(row = index, cells = as.list(cells)))
  }
  rows
}

.library_reference_csv_metadata <- function(source_dir) {
  paths <- list.files(source_dir, pattern = "^pubmed_result.*[.]csv$", recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  output <- list()
  for (path in paths) {
    data <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                                     fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
    if (is.null(data)) next
    names_lower <- tolower(names(data))
    pmid_hits <- which(names_lower == "pmid")
    pmid_column <- if (length(pmid_hits)) pmid_hits[[1L]] else NA_integer_
    if (!is.finite(pmid_column)) next
    title_hits <- which(names_lower == "title")
    abstract_hits <- which(names_lower %in% c("abstract", "abstracttext"))
    title_column <- if (length(title_hits)) title_hits[[1L]] else NA_integer_
    abstract_column <- if (length(abstract_hits)) abstract_hits[[1L]] else NA_integer_
    for (row in seq_len(nrow(data))) {
      pmid <- gsub("[^0-9]", "", as.character(data[[pmid_column]][[row]] %||% ""))
      if (!grepl("^[0-9]{5,9}$", pmid) || !is.null(output[[pmid]])) next
      output[[pmid]] <- list(
        title = if (is.finite(title_column)) .library_reference_text(data[[title_column]][[row]]) else "",
        abstract = if (is.finite(abstract_column)) .library_reference_text(data[[abstract_column]][[row]]) else "",
        source = .library_reference_relpath(path, source_dir)
      )
    }
  }
  output
}

.library_reference_pdf_index <- function(source_dir) {
  paths <- list.files(source_dir, pattern = "[.]pdf$", recursive = TRUE, full.names = TRUE,
                      ignore.case = TRUE)
  index <- list()
  for (path in paths) {
    matches <- regmatches(basename(path), gregexpr("(?<![0-9])([0-9]{5,9})(?![0-9])",
                                                   basename(path), perl = TRUE))[[1L]]
    if (!length(matches)) next
    for (pmid in matches) index[[pmid]] <- c(index[[pmid]], path)
  }
  index
}

.library_reference_preferred_pdf <- function(paths) {
  if (is.null(paths) || !length(paths)) return("")
  excluded <- grepl("[/\\](exclude|excluded)[/\\]", paths, ignore.case = TRUE)
  candidates <- if (any(!excluded)) paths[!excluded] else paths
  candidates[[which.min(nchar(candidates))]]
}

.library_reference_route <- function(text) {
  text <- tolower(.library_reference_text(text))
  if (grepl("intravenous|\biv\b|i[.]v[.]", text, perl = TRUE)) return("intravenous")
  if (grepl("oral|\bpo\b|per os", text, perl = TRUE)) return("oral")
  if (grepl("subcutaneous|\bsc\b|s[.]c[.]", text, perl = TRUE)) return("subcutaneous")
  NULL
}

.library_reference_n <- function(text) {
  match <- regexec("(?i)(?:^|[^A-Za-z])N\\s*[:=]\\s*([0-9]+)", text, perl = TRUE)
  values <- regmatches(text, match)[[1L]]
  if (length(values) < 2L) NULL else as.numeric(values[[2L]])
}

.library_reference_compartments <- function(text) {
  match <- regexec("(?i)\\b(one|two|three|1|2|3)[ -]?compartment", text, perl = TRUE)
  values <- regmatches(text, match)[[1L]]
  if (length(values) < 2L) return(NULL)
  switch(tolower(values[[2L]]), one = 1L, two = 2L, three = 3L,
         `1` = 1L, `2` = 2L, `3` = 3L, NULL)
}

.library_reference_parameter_names <- function(equations) {
  pattern <- "(?i)\\b([A-Za-z][A-Za-z0-9_.\\/-]{0,39})\\s*=\\s*theta\\s*([0-9]+)"
  tokens <- regmatches(equations, gregexpr(pattern, equations, perl = TRUE))[[1L]]
  output <- list()
  for (token in tokens) {
    values <- regmatches(token, regexec(pattern, token, perl = TRUE))[[1L]]
    if (length(values) >= 3L && is.null(output[[values[[3L]]]])) output[[values[[3L]]]] <- values[[2L]]
  }
  output
}

.library_reference_eta_distribution <- function(equations, index) {
  compact <- gsub("[[:space:]]+", "", tolower(equations))
  if (grepl(paste0("exp\\(eta\\(?", index, "\\)?\\)"), compact, perl = TRUE)) return("log_normal")
  if (grepl(paste0("\\(1[+]eta\\(?", index, "\\)?\\)"), compact, perl = TRUE) ||
      grepl(paste0("[+]eta\\(?", index, "\\)?"), compact, perl = TRUE)) return("normal")
  "unknown"
}

.library_reference_error_model <- function(text) {
  text <- tolower(text)
  proportional <- grepl("cobs\\s*=.*\\(1\\s*[+]\\s*eps|y\\s*=.*\\(1\\s*[+]\\s*err", text, perl = TRUE)
  combined <- grepl("\\(1\\s*[+]\\s*(?:eps|err)[^)]*\\)\\s*[+]\\s*(?:eps|err)", text, perl = TRUE)
  additive <- !proportional && grepl("cobs\\s*=.*[+]\\s*eps|y\\s*=.*[+]\\s*err", text, perl = TRUE)
  if (combined) "combined" else if (proportional) "proportional" else if (additive) "additive" else "unknown"
}

.library_reference_parameters <- function(values_text, equations) {
  number <- "-?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][-+]?[0-9]+)?"
  pattern <- paste0("(?i)\\b(theta|omega|sigma)\\s*([0-9]+)",
                    "(?:\\s*[,]\\s*([0-9]+))?\\s*=\\s*(", number, ")")
  tokens <- regmatches(values_text, gregexpr(pattern, values_text, perl = TRUE))[[1L]]
  parsed <- lapply(tokens, function(token) {
    values <- regmatches(token, regexec(pattern, token, perl = TRUE))[[1L]]
    list(type = tolower(values[[2L]]), first = as.integer(values[[3L]]),
         second = if (nzchar(values[[4L]])) as.integer(values[[4L]]) else NULL,
         value = as.numeric(values[[5L]]), raw = token)
  })
  names_by_theta <- .library_reference_parameter_names(equations)
  theta <- lapply(Filter(function(item) item$type == "theta", parsed), function(item) {
    list(name = names_by_theta[[as.character(item$first)]] %||% paste0("THETA", item$first),
         typical = item$value, se = NULL, unit = NULL)
  })
  theta_order <- vapply(Filter(function(item) item$type == "theta", parsed), `[[`, integer(1), "first")
  if (length(theta)) theta <- theta[order(theta_order)]

  omega_items <- Filter(function(item) item$type == "omega" && is.null(item$second), parsed)
  omega <- lapply(omega_items, function(item) {
    distribution <- .library_reference_eta_distribution(equations, item$first)
    list(description = paste("Appendix OMEGA for ETA", item$first),
         parameter = names_by_theta[[as.character(item$first)]] %||% "",
         eta_index = item$first, eta_distribution = distribution,
         eta_expression = if (distribution == "unknown") NULL else paste0("ETA(", item$first, ") in appendix equation"),
         variability_level = "iiv", reported_value = item$value,
         reported_metric = "variance", value = item$value,
         conversion = "Appendix NONMEM-scale OMEGA value retained verbatim; source-paper verification pending")
  })
  omega_order <- vapply(omega_items, `[[`, integer(1), "first")
  if (length(omega)) omega <- omega[order(omega_order)]

  covariance_items <- Filter(function(item) item$type == "omega" && !is.null(item$second), parsed)
  omega_covariance <- lapply(covariance_items, function(item) {
    list(row_eta = max(item$first, item$second), col_eta = min(item$first, item$second),
         reported_value = item$value, reported_metric = "covariance", value = item$value,
         conversion = "Appendix covariance retained verbatim; source-paper verification pending")
  })

  error_model <- .library_reference_error_model(equations)
  sigma_items <- Filter(function(item) item$type == "sigma", parsed)
  sigma <- lapply(sigma_items, function(item) {
    list(description = paste("Appendix SIGMA", item$first), error_model = error_model,
         reported_value = item$value, reported_metric = "variance", value = item$value,
         conversion = "Appendix NONMEM-scale SIGMA value retained verbatim; source-paper verification pending")
  })
  sigma_order <- vapply(sigma_items, `[[`, integer(1), "first")
  if (length(sigma)) sigma <- sigma[order(sigma_order)]
  list(theta = theta, omega = omega, omega_covariance = omega_covariance, sigma = sigma)
}

.library_reference_covariates <- function(text) {
  text <- .library_reference_text(text)
  tested <- sub("(?is)^.*?Tested\\s*:\\s*", "", text, perl = TRUE)
  tested <- sub("(?is)\\s*Included\\s*:.*$", "", tested, perl = TRUE)
  included <- if (grepl("(?i)Included\\s*:", text, perl = TRUE)) {
    sub("(?is)^.*?Included\\s*:\\s*", "", text, perl = TRUE)
  } else ""
  clean <- function(value) {
    if (!nzchar(value) || grepl("^(none|-|not reported)$", trimws(value), ignore.case = TRUE)) return(character())
    parts <- trimws(unlist(strsplit(value, "\\s*[,;]\\s*", perl = TRUE)))
    unique(parts[nzchar(parts)])
  }
  list(tested = clean(tested), included = clean(included), raw = text)
}

.library_reference_partition <- function(group, seed, validation_fraction, test_fraction) {
  hash <- digest::digest(paste(seed, group, sep = ":"), algo = "sha256", serialize = FALSE)
  score <- strtoi(substr(hash, 1L, 7L), base = 16L) / (16^7 - 1)
  if (score < test_fraction) "test" else if (score < test_fraction + validation_fraction) "validation" else "train"
}

.library_reference_id <- function(drug, pmid, model_index) {
  paste0("aed_", tolower(gsub("[^A-Za-z0-9]", "", drug)), "_", pmid,
         "_m", sprintf("%02d", model_index))
}

.library_reference_model_record <- function(drug, meta_row, model_row, model_index,
                                             source_dir, appendix_path, metadata, pdf_index,
                                             partition_seed, validation_fraction, test_fraction,
                                             pdf_hash_cache) {
  pmid <- model_row$pmid
  matching_meta <- Filter(function(item) identical(item$pmid, pmid), meta_row)
  study_cells <- if (length(matching_meta)) matching_meta[[1L]]$cells else as.list(rep("", 6L))
  model_cells <- model_row$cells
  length(study_cells) <- max(6L, length(study_cells)); length(model_cells) <- max(6L, length(model_cells))
  study_cells[vapply(study_cells, is.null, logical(1))] <- ""
  model_cells[vapply(model_cells, is.null, logical(1))] <- ""
  publication <- metadata[[pmid]] %||% list(title = "", abstract = "", source = "")
  title <- publication$title %||% ""
  dose <- .library_reference_text(model_cells[[2L]])
  structure <- .library_reference_text(model_cells[[3L]])
  equations <- .library_reference_text(model_cells[[4L]])
  parameter_values <- .library_reference_text(model_cells[[5L]])
  covariates <- .library_reference_covariates(study_cells[[5L]])
  parameters <- .library_reference_parameters(parameter_values, equations)
  pdf_path <- .library_reference_preferred_pdf(pdf_index[[pmid]])
  pdf_relative <- if (nzchar(pdf_path)) .library_reference_relpath(pdf_path, source_dir) else ""
  pdf_hash <- ""
  if (nzchar(pdf_path)) {
    key <- normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
    pdf_hash <- pdf_hash_cache[[key]]
    if (is.null(pdf_hash)) {
      pdf_hash <- .library_reference_hash(pdf_path)
      pdf_hash_cache[[key]] <- pdf_hash
    }
  }
  quality_issues <- character()
  if (!nzchar(title)) quality_issues <- c(quality_issues, "publication_title_missing")
  if (!nzchar(equations)) quality_issues <- c(quality_issues, "model_equations_missing")
  if (!nzchar(parameter_values)) quality_issues <- c(quality_issues, "parameter_values_missing")
  if (!nzchar(pdf_relative)) quality_issues <- c(quality_issues, "source_pdf_missing")
  tier <- if (!nzchar(equations) && !nzchar(parameter_values)) "D" else "C"
  partition <- .library_reference_partition(pmid, partition_seed, validation_fraction, test_fraction)
  reference_id <- .library_reference_id(drug, pmid, model_index)
  target <- list(
    title = title,
    compound = drug,
    population = if (nzchar(.library_reference_text(study_cells[[2L]]))) .library_reference_text(study_cells[[2L]]) else NULL,
    route = .library_reference_route(dose),
    n_subjects = .library_reference_n(study_cells[[2L]]),
    software = NULL,
    estimation_method = NULL,
    model_type = if (nzchar(.library_reference_text(study_cells[[3L]]))) .library_reference_text(study_cells[[3L]]) else NULL,
    population_details = library_population_normalize(
      .library_reference_text(study_cells[[2L]]), .library_reference_n(study_cells[[2L]]),
      paste0("appendix study row ", if (length(matching_meta)) matching_meta[[1L]]$row else "unknown")
    ),
    dosing = library_dosing_normalize(
      dose, .library_reference_route(dose), paste0("appendix model row ", model_row$row)
    ),
    structural_model = list(advan = NULL, trans = NULL,
                            compartments = .library_reference_compartments(structure),
                            description = structure),
    parameters = parameters,
    covariates = covariates$included,
    residual_error = if (grepl("(?i)eps|err", equations, perl = TRUE)) equations else NULL,
    reproduction_targets = list(),
    confidence = list(overall = 1, fields = list(structure = 1, parameters = 1,
                                                 population = 1, software = 0)),
    evidence_quotes = character(),
    notes = "Appendix-derived silver reference; compare with the publication before promotion to a gold tier."
  )
  target <- library_model_enrich(
    target, source_locator = paste0("appendix model row ", model_row$row)
  )
  list(
    schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION,
    corpus_id = "aed-pkpd-reference",
    reference_id = reference_id,
    article_id = paste0("pmid_", pmid),
    pmid = pmid,
    model_index = model_index,
    compound_group = drug,
    family_id = paste0("pmid_", pmid),
    partition = partition,
    source = list(
      appendix = list(path = .library_reference_relpath(appendix_path, source_dir),
                      sha256 = .library_reference_hash(appendix_path),
                      study_table = if (length(matching_meta)) 1L else NULL,
                      study_row = if (length(matching_meta)) matching_meta[[1L]]$row else NULL,
                      model_table = 2L, model_row = model_row$row),
      publication_pdf = list(path = pdf_relative, sha256 = pdf_hash),
      metadata_csv = publication$source %||% ""
    ),
    raw = list(
      citation = model_row$raw,
      population = .library_reference_text(study_cells[[2L]]),
      model_type = .library_reference_text(study_cells[[3L]]),
      objective = .library_reference_text(study_cells[[4L]]),
      covariates = covariates$raw,
      observations = .library_reference_text(study_cells[[6L]]),
      dose = dose, structure = structure, equations = equations,
      parameter_values = parameter_values,
      validation = .library_reference_text(model_cells[[6L]])
    ),
    reference = list(
      extraction_target = target,
      study = list(first_author = model_row$first_author, year = model_row$year,
                   dose = dose, population = .library_reference_text(study_cells[[2L]]),
                   population_details = target$population_details,
                   dosing = target$dosing,
                   objective = .library_reference_text(study_cells[[4L]])),
      covariates = covariates,
      validation = list(raw = .library_reference_text(model_cells[[6L]]))
    ),
    provenance = list(
      status = "appendix_transcription",
      field_tiers = list(article_identity = "B", study = tier, structure = tier,
                         theta = tier, omega = tier, sigma = tier,
                         covariates = tier, validation = tier),
      normalization = list(
        omega = "Values labelled OMEGA in the appendix are retained as NONMEM variances; no CV reconversion is applied.",
        eta = "ETA distribution is inferred only when the appendix equation explicitly uses exp(ETA) or additive ETA.",
        implementation = "Canonical structure is normalized first; ADVAN/TRANS is then deterministically inferred with provenance and alternatives.",
        population = "Free-text cohorts and summary statistics are retained and additionally normalized into structured cohort descriptors.",
        source_text = "The appendix row is preserved verbatim in raw; normalized fields never replace it."
      )
    ),
    quality = list(tier = tier, review_status = "unreviewed",
                   strict_score_eligible = FALSE, training_eligible = FALSE,
                   issues = unique(quality_issues)),
    created_at = .library_reference_now()
  )
}

.library_reference_screening_records <- function(source_dir, model_records, partition_seed,
                                                  validation_fraction, test_fraction) {
  positives <- lapply(model_records, function(record) list(
    schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION,
    screening_id = paste("screen", tolower(record$compound_group), record$pmid, sep = "_"),
    pmid = record$pmid, compound_group = record$compound_group,
    partition = record$partition, relevant_model = TRUE, recoverable_model = TRUE,
    exclusion_reason = NULL, label_quality = "included_appendix_model",
    source = list(path = record$source$appendix$path, sha256 = record$source$appendix$sha256)
  ))
  pdfs <- list.files(source_dir, pattern = "[.]pdf$", recursive = TRUE, full.names = TRUE,
                     ignore.case = TRUE)
  excluded <- pdfs[grepl("[/\\](exclude|excluded)[/\\]", pdfs, ignore.case = TRUE)]
  negatives <- list()
  for (path in excluded) {
    match <- regmatches(basename(path), regexpr("(?<![0-9])([0-9]{5,9})(?![0-9])",
                                               basename(path), perl = TRUE))
    if (!length(match) || !nzchar(match[[1L]])) next
    pmid <- match[[1L]]
    relative <- .library_reference_relpath(path, source_dir)
    parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
    supplement <- match("Supplements", parts)
    drug <- if (!is.na(supplement) && length(parts) > supplement) parts[[supplement + 1L]] else "unknown"
    id <- paste("screen", tolower(drug), pmid, sep = "_")
    prefix <- regmatches(basename(path), regexpr("^[0-4](?=_)", basename(path), perl = TRUE))
    prefix <- if (length(prefix)) prefix[[1L]] else ""
    reason <- switch(prefix,
      `1` = "wrong_aed", `2` = "non_human", `3` = "no_pkpd", `4` = "no_model",
      "explicitly_excluded_reason_unavailable")
    negatives[[length(negatives) + 1L]] <- list(
      schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION, screening_id = id,
      pmid = pmid, compound_group = drug,
      partition = .library_reference_partition(pmid, partition_seed, validation_fraction, test_fraction),
      relevant_model = FALSE, recoverable_model = FALSE, exclusion_reason = reason,
      label_quality = if (identical(reason, "explicitly_excluded_reason_unavailable")) "explicit_exclusion" else "coded_exclusion",
      source = list(path = relative, sha256 = .library_reference_hash(path))
    )
  }
  records <- c(positives, negatives)
  ids <- vapply(records, `[[`, character(1), "screening_id")
  records[!duplicated(ids)]
}

#' Build a versioned AED PK/PD reference corpus
#'
#' Converts the v2 systematic-review supplement tables into a separate,
#' auditable LibeRary-style JSON corpus. Appendix text is retained verbatim and
#' normalized fields are marked as silver-tier until checked against the source
#' publication. Article-level deterministic partitions prevent the same PMID
#' from leaking between training, validation, and test data.
#'
#' @param source_dir Directory containing `Supplements` and the review files.
#' @param output_dir New directory for this immutable corpus version.
#' @param version Corpus version. Use a new directory for every version.
#' @param partition_seed Stable string or integer used for article partitions.
#' @param validation_fraction Fraction assigned to validation.
#' @param test_fraction Fraction assigned to the locked test set.
#' @return Corpus manifest invisibly.
#' @export
library_reference_build <- function(source_dir, output_dir, version = "0.1.0",
                                    partition_seed = 20260716L,
                                    validation_fraction = 0.15,
                                    test_fraction = 0.15) {
  source_dir <- normalizePath(path.expand(source_dir), winslash = "/", mustWork = TRUE)
  output_dir <- path.expand(output_dir)
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Reference output already exists and is not empty. Use a new version directory: ", output_dir,
         call. = FALSE)
  }
  fractions <- c(validation_fraction, test_fraction)
  if (any(!is.finite(fractions)) || any(fractions < 0) || sum(fractions) >= 1) {
    stop("Validation and test fractions must be non-negative and sum to less than one.", call. = FALSE)
  }
  supplement_root <- file.path(source_dir, "Supplements")
  documents <- list.files(supplement_root, pattern = "^Supplement_[A-Za-z0-9]+_v2[.]docx$",
                          recursive = TRUE, full.names = TRUE)
  documents <- documents[!grepl("[/\\](old|base)[/\\]", documents, ignore.case = TRUE)]
  if (!length(documents)) stop("No drug-specific v2 supplement documents were found.", call. = FALSE)
  metadata <- .library_reference_csv_metadata(source_dir)
  pdf_index <- .library_reference_pdf_index(source_dir)
  pdf_hash_cache <- new.env(parent = emptyenv())
  records <- list()
  build_warnings <- character()
  for (document in sort(documents)) {
    drug <- basename(dirname(document))
    tables <- .library_reference_docx_tables(document)
    if (length(tables) < 2L) {
      build_warnings <- c(build_warnings, paste("Fewer than two tables:", basename(document)))
      next
    }
    study_rows <- .library_reference_rows(tables[[1L]])
    model_rows <- .library_reference_rows(tables[[2L]])
    counts <- integer()
    for (model_row in model_rows) {
      previous <- if (model_row$pmid %in% names(counts)) counts[[model_row$pmid]] else 0L
      counts[[model_row$pmid]] <- previous + 1L
      records[[length(records) + 1L]] <- .library_reference_model_record(
        drug, study_rows, model_row, counts[[model_row$pmid]], source_dir, document,
        metadata, pdf_index, partition_seed, validation_fraction, test_fraction,
        pdf_hash_cache
      )
    }
  }
  if (!length(records)) stop("No keyed appendix model records were parsed.", call. = FALSE)
  ids <- vapply(records, `[[`, character(1), "reference_id")
  if (anyDuplicated(ids)) stop("Duplicate reference ids were generated: ",
                               paste(unique(ids[duplicated(ids)]), collapse = ", "), call. = FALSE)

  articles <- list()
  for (record in records) {
    article_id <- record$article_id
    if (is.null(articles[[article_id]])) {
      meta <- metadata[[record$pmid]] %||% list(title = "", abstract = "", source = "")
      articles[[article_id]] <- list(
        schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION,
        article_id = article_id, pmid = record$pmid,
        title = meta$title %||% "", abstract = meta$abstract %||% "",
        first_author = record$reference$study$first_author,
        year = record$reference$study$year,
        compounds = record$compound_group,
        model_ids = record$reference_id,
        partition = record$partition,
        publication_pdf = record$source$publication_pdf,
        metadata_source = meta$source %||% ""
      )
    } else {
      articles[[article_id]]$compounds <- unique(c(articles[[article_id]]$compounds, record$compound_group))
      articles[[article_id]]$model_ids <- unique(c(articles[[article_id]]$model_ids, record$reference_id))
    }
  }
  screening <- .library_reference_screening_records(source_dir, records, partition_seed,
                                                     validation_fraction, test_fraction)
  for (item in screening) {
    article_id <- paste0("pmid_", item$pmid)
    if (is.null(articles[[article_id]])) {
      meta <- metadata[[item$pmid]] %||% list(title = "", abstract = "", source = "")
      articles[[article_id]] <- list(
        schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION,
        article_id = article_id, pmid = item$pmid,
        title = meta$title %||% "", abstract = meta$abstract %||% "",
        first_author = "", year = "", compounds = item$compound_group,
        model_ids = character(), partition = item$partition,
        publication_pdf = item$source,
        metadata_source = meta$source %||% ""
      )
    } else {
      articles[[article_id]]$compounds <- unique(c(articles[[article_id]]$compounds, item$compound_group))
    }
  }
  directories <- c("articles", "models", "screening", "partitions", "schemas", "reports")
  for (directory in directories) dir.create(file.path(output_dir, directory), recursive = TRUE, showWarnings = FALSE)
  schema_dir <- .library_reference_schema_dir()
  if (nzchar(schema_dir)) {
    schema_files <- list.files(schema_dir, pattern = "^reference-.*[.]schema[.]json$", full.names = TRUE)
    if (length(schema_files)) file.copy(schema_files, file.path(output_dir, "schemas"), copy.mode = TRUE)
  }
  for (record in records) .library_atomic_write(record, file.path(output_dir, "models", paste0(record$reference_id, ".json")))
  for (article in articles) .library_atomic_write(article, file.path(output_dir, "articles", paste0(article$article_id, ".json")))
  for (item in screening) .library_atomic_write(item, file.path(output_dir, "screening", paste0(item$screening_id, ".json")))

  partitions <- lapply(c("train", "validation", "test"), function(partition) {
    list(partition = partition,
         model_ids = vapply(Filter(function(x) identical(x$partition, partition), records), `[[`, character(1), "reference_id"),
         article_ids = unique(vapply(Filter(function(x) identical(x$partition, partition), articles), `[[`, character(1), "article_id")),
         screening_ids = vapply(Filter(function(x) identical(x$partition, partition), screening), `[[`, character(1), "screening_id"))
  })
  names(partitions) <- c("train", "validation", "test")
  for (partition in partitions) .library_atomic_write(partition, file.path(output_dir, "partitions", paste0(partition$partition, ".json")))
  source_hashes <- sort(unique(c(
    vapply(records, function(x) x$source$appendix$sha256, character(1)),
    vapply(records, function(x) x$source$publication_pdf$sha256 %||% "", character(1))
  )))
  source_hashes <- source_hashes[nzchar(source_hashes)]
  implementation_status <- vapply(records, function(record) {
    implementations <- record$reference$extraction_target$structural_model$implementations %||% list()
    if (length(implementations)) implementations[[1L]]$status %||% "unresolved" else "unresolved"
  }, character(1))
  structured_populations <- sum(vapply(records, function(record) {
    length(record$reference$extraction_target$population_details$cohorts %||% list()) > 0L
  }, logical(1)))
  pharmacogenetic_populations <- sum(vapply(records, function(record) {
    cohorts <- record$reference$extraction_target$population_details$cohorts %||% list()
    any(vapply(cohorts, function(cohort) length(cohort$pharmacogenetics %||% list()) > 0L, logical(1)))
  }, logical(1)))
  reproduction_targets <- sum(vapply(records, function(record) {
    length(record$reference$extraction_target$reproduction_targets %||% list())
  }, integer(1)))
  manifest <- list(
    schema_version = LIBRARY_REFERENCE_SCHEMA_VERSION,
    corpus_id = "aed-pkpd-reference", version = version,
    status = "silver_reference", created_at = .library_reference_now(),
    source = list(name = basename(source_dir), source_hash_digest = digest::digest(source_hashes, algo = "sha256"),
                  documents = vapply(sort(documents), .library_reference_relpath, character(1), root = source_dir)),
    partitioning = list(unit = "PMID/article", seed = as.character(partition_seed),
                        train_fraction = 1 - validation_fraction - test_fraction,
                        validation_fraction = validation_fraction, test_fraction = test_fraction,
                        locked_test = TRUE),
    counts = list(
      models = length(records), articles = length(articles), screening = length(screening),
      tiers = as.list(table(vapply(records, function(x) x$quality$tier, character(1)))),
      partitions = lapply(partitions, function(x) length(x$model_ids)),
      semantics = list(
        implementation_status = as.list(table(implementation_status)),
        structured_populations = structured_populations,
        pharmacogenetic_populations = pharmacogenetic_populations,
        reproduction_targets = reproduction_targets
      )
    ),
    safeguards = list(
      live_catalog_isolated = TRUE,
      test_must_not_be_indexed_or_exported_for_training = TRUE,
      appendix_raw_preserved = TRUE,
      strict_scoring_requires_tier_a_or_b = TRUE,
      training_requires_explicit_eligibility = TRUE
    ),
    limitations = c(
      "The v2 appendices are a silver reference and explicitly contain reconstructions and assumptions.",
      "One appendix row is currently represented as one model unless multiple keyed rows exist.",
      "ADVAN/TRANS mappings are derived from appendix structural descriptions and remain silver-tier until checked against each publication.",
      "Structured demographics are parsed from preserved appendix text and require source review before promotion to a strict tier.",
      "The appendices do not contain digitized concentration targets; reproduction scoring becomes available only after source tables or figures are extracted.",
      "Exclusion reasons are unavailable for some explicitly excluded PDFs.",
      "Publication titles and abstracts are present only where recoverable from local CSV metadata."
    ),
    warnings = unique(build_warnings)
  )
  .library_atomic_write(manifest, file.path(output_dir, "manifest.json"))
  readme <- c(
    "# AED-PKPD Reference Corpus", "",
    paste0("Version: ", version), "",
    "This corpus is deliberately separate from the live LibeRary catalogue.",
    "The appendix-derived records are silver-tier until independently checked against the source paper.",
    "ADVAN/TRANS and structured demographic fields are inferred from preserved appendix text and must be source-reviewed before strict scoring.",
    "Reproduction targets are intentionally empty until concentration data are extracted from the source figures or tables.",
    "The locked test partition must never be included in RAG indexes, prompts, demonstrations, or training exports.", "",
    "Use `LibeRary::library_reference_validate()` before benchmarking and",
    "`LibeRary::library_reference_training_export()` to create leakage-checked training data."
  )
  .library_atomic_write_lines(readme, file.path(output_dir, "README.md"))
  validation <- library_reference_validate(output_dir)
  if (!validation$valid) stop("Generated reference corpus failed validation: ",
                              paste(validation$errors, collapse = "; "), call. = FALSE)
  invisible(manifest)
}

#' List reference-corpus models
#' @param root Reference corpus root.
#' @param partition Optional `train`, `validation`, or `test` filter.
#' @param tiers Optional quality-tier filter.
#' @return Data frame with one row per model.
#' @export
library_reference_list <- function(root, partition = NULL, tiers = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  paths <- list.files(file.path(root, "models"), pattern = "[.]json$", full.names = TRUE)
  records <- lapply(paths, jsonlite::read_json, simplifyVector = FALSE)
  if (!is.null(partition)) records <- Filter(function(x) x$partition %in% partition, records)
  if (!is.null(tiers)) records <- Filter(function(x) x$quality$tier %in% tiers, records)
  if (!length(records)) return(data.frame())
  data.frame(
    reference_id = vapply(records, `[[`, character(1), "reference_id"),
    article_id = vapply(records, `[[`, character(1), "article_id"),
    pmid = vapply(records, `[[`, character(1), "pmid"),
    compound = vapply(records, `[[`, character(1), "compound_group"),
    partition = vapply(records, `[[`, character(1), "partition"),
    tier = vapply(records, function(x) x$quality$tier, character(1)),
    training_eligible = vapply(records, function(x) isTRUE(x$quality$training_eligible), logical(1)),
    stringsAsFactors = FALSE
  )
}

#' Read a reference-corpus model
#' @param reference_id Stable reference id.
#' @param root Reference corpus root.
#' @return Parsed model record.
#' @export
library_reference_get <- function(reference_id, root) {
  id <- .library_valid_id(reference_id)
  path <- file.path(normalizePath(root, winslash = "/", mustWork = TRUE), "models", paste0(id, ".json"))
  if (!file.exists(path)) stop("Reference model not found: ", id, call. = FALSE)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

#' Validate a reference corpus and its leakage safeguards
#' @param root Reference corpus root.
#' @param check_hashes Recompute locally available appendix/PDF hashes when `source_dir` is supplied.
#' @param source_dir Optional original AED PK/PD review directory.
#' @return A list containing `valid`, `errors`, `warnings`, and counts.
#' @export
library_reference_validate <- function(root, check_hashes = FALSE, source_dir = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  errors <- character(); warnings <- character()
  manifest_path <- file.path(root, "manifest.json")
  if (!file.exists(manifest_path)) return(list(valid = FALSE, errors = "manifest.json is missing", warnings = character()))
  manifest <- tryCatch(jsonlite::read_json(manifest_path, simplifyVector = FALSE), error = identity)
  if (inherits(manifest, "error")) return(list(valid = FALSE, errors = conditionMessage(manifest), warnings = character()))
  if (!manifest$schema_version %in% LIBRARY_REFERENCE_SCHEMA_SUPPORTED) {
    errors <- c(errors, paste("Unsupported reference schema", manifest$schema_version %||% "missing"))
  }
  model_paths <- list.files(file.path(root, "models"), pattern = "[.]json$", full.names = TRUE)
  models <- lapply(model_paths, function(path) tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = identity))
  for (index in seq_along(models)) {
    model <- models[[index]]
    if (inherits(model, "error")) { errors <- c(errors, paste(basename(model_paths[[index]]), conditionMessage(model))); next }
    missing <- setdiff(c("schema_version", "reference_id", "article_id", "pmid", "partition",
                         "source", "raw", "reference", "provenance", "quality"), names(model))
    if (length(missing)) errors <- c(errors, paste(basename(model_paths[[index]]), "missing", paste(missing, collapse = ", ")))
    if (is.null(model$partition) || !model$partition %in% c("train", "validation", "test")) errors <- c(errors, paste(model$reference_id %||% basename(model_paths[[index]]), "has invalid partition"))
    if (identical(model$partition, "test") && isTRUE(model$quality$training_eligible)) {
      errors <- c(errors, paste(model$reference_id, "is test data but training_eligible is true"))
    }
    if (is.null(model$quality$tier) || !model$quality$tier %in% c("A", "B", "C", "D")) errors <- c(errors, paste(model$reference_id %||% basename(model_paths[[index]]), "has invalid tier"))
    article <- file.path(root, "articles", paste0(model$article_id, ".json"))
    if (!file.exists(article)) errors <- c(errors, paste(model$reference_id, "references a missing article"))
  }
  valid_models <- Filter(Negate(function(x) inherits(x, "error")), models)
  ids <- vapply(valid_models, `[[`, character(1), "reference_id")
  if (anyDuplicated(ids)) errors <- c(errors, "Duplicate reference ids are present")
  pmid_partitions <- split(vapply(valid_models, `[[`, character(1), "partition"),
                           vapply(valid_models, `[[`, character(1), "pmid"))
  leaking <- names(Filter(function(value) length(unique(value)) > 1L, pmid_partitions))
  if (length(leaking)) errors <- c(errors, paste("PMIDs cross partitions:", paste(leaking, collapse = ", ")))
  for (partition in c("train", "validation", "test")) {
    path <- file.path(root, "partitions", paste0(partition, ".json"))
    if (!file.exists(path)) errors <- c(errors, paste("Missing partition index:", partition))
  }
  if (isTRUE(check_hashes)) {
    if (is.null(source_dir)) stop("Supply `source_dir` when `check_hashes = TRUE`.", call. = FALSE)
    source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
    for (model in valid_models) {
      for (kind in c("appendix", "publication_pdf")) {
        source <- model$source[[kind]]
        if (is.null(source) || !nzchar(source$path %||% "")) next
        path <- file.path(source_dir, source$path)
        if (!file.exists(path)) warnings <- c(warnings, paste("Source missing:", source$path))
        else if (!identical(.library_reference_hash(path), source$sha256)) errors <- c(errors, paste("Hash mismatch:", source$path))
      }
    }
  }
  list(valid = !length(errors), errors = unique(errors), warnings = unique(warnings),
       counts = list(models = length(valid_models),
                     articles = length(list.files(file.path(root, "articles"), pattern = "[.]json$")),
                     partitions = as.list(table(vapply(valid_models, `[[`, character(1), "partition"))),
                     tiers = as.list(table(vapply(valid_models, function(x) x$quality$tier, character(1))))))
}
