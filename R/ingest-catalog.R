.library_template_spec <- function(advan) {
  switch(as.character(advan),
    `1` = list(trans = 2L, values = c(5, 50), names = c("CL", "V"), eta = 2L,
      pred = c("CL = THETA(1) * exp(ETA(1))", "V = THETA(2) * exp(ETA(2))", "S1 = V")),
    `2` = list(trans = 2L, values = c(1, 5, 50), names = c("KA", "CL", "V"), eta = 3L,
      pred = c("KA = THETA(1) * exp(ETA(1))", "CL = THETA(2) * exp(ETA(2))", "V = THETA(3) * exp(ETA(3))", "S2 = V")),
    `3` = list(trans = 4L, values = c(5, 30, 8, 70), names = c("CL", "V1", "Q", "V2"), eta = 2L,
      pred = c("CL = THETA(1) * exp(ETA(1))", "V1 = THETA(2) * exp(ETA(2))", "Q = THETA(3)", "V2 = THETA(4)", "S1 = V1")),
    `4` = list(trans = 4L, values = c(1, 5, 30, 8, 70), names = c("KA", "CL", "V1", "Q", "V2"), eta = 3L,
      pred = c("KA = THETA(1) * exp(ETA(1))", "CL = THETA(2) * exp(ETA(2))", "V1 = THETA(3) * exp(ETA(3))", "Q = THETA(4)", "V2 = THETA(5)", "S2 = V1")),
    `11` = list(trans = 4L, values = c(5, 20, 8, 40, 4, 80), names = c("CL", "V1", "Q2", "V2", "Q3", "V3"), eta = 2L,
      pred = c("CL = THETA(1) * exp(ETA(1))", "V1 = THETA(2) * exp(ETA(2))", "Q2 = THETA(3)", "V2 = THETA(4)", "Q3 = THETA(5)", "V3 = THETA(6)", "S1 = V1")),
    `12` = list(trans = 4L, values = c(1, 5, 20, 8, 40, 4, 80), names = c("KA", "CL", "V1", "Q2", "V2", "Q3", "V3"), eta = 3L,
      pred = c("KA = THETA(1) * exp(ETA(1))", "CL = THETA(2) * exp(ETA(2))", "V1 = THETA(3) * exp(ETA(3))", "Q2 = THETA(4)", "V2 = THETA(5)", "Q3 = THETA(6)", "V3 = THETA(7)", "S2 = V1")),
    NULL)
}

.library_metric_name <- function(value, default = "unknown") {
  value <- tolower(trimws(as.character(value %||% default)[[1L]]))
  value <- gsub("[^a-z0-9]+", "_", value)
  value <- gsub("^_|_$", "", value)
  if (nzchar(value)) value else default
}

.library_numeric_value <- function(value) {
  value <- suppressWarnings(as.numeric(value %||% NA_real_))
  if (length(value) && is.finite(value[[1L]])) value[[1L]] else NA_real_
}

.library_omega_variance <- function(item, typical = NA_real_, template_log_normal = TRUE) {
  description <- paste(as.character(item$description %||% ""),
                       as.character(item$parameter %||% ""))
  reported <- .library_numeric_value(item$reported_value)
  converted <- .library_numeric_value(item$value)
  metric <- .library_metric_name(item$reported_metric)
  distribution <- .library_metric_name(item$eta_distribution)
  expression <- tolower(as.character(item$eta_expression %||% "")[[1L]])
  metric_inferred <- FALSE
  distribution_inferred <- FALSE

  if (distribution %in% c("unknown", "")) {
    if (grepl("exp\\s*\\([^)]*eta", expression)) distribution <- "log_normal"
    else if (grepl("[+].*eta", expression)) distribution <- "normal"
    else if (isTRUE(template_log_normal)) distribution <- "log_normal"
    if (distribution != "unknown") distribution_inferred <- TRUE
  }

  raw <- if (is.finite(reported)) reported else converted
  if (!is.finite(raw) || raw < 0) {
    return(list(value = NA_real_, supported = FALSE, review = TRUE,
                metric = metric, distribution = distribution,
                note = "No usable variability value was reported."))
  }
  if (metric %in% c("unknown", "")) {
    if (!is.finite(reported) && is.finite(converted) && converted <= 1) {
      metric <- "variance"
    } else if (raw <= 1) {
      metric <- "variance"
    } else if (raw <= 500 && grepl("cv|percent|variability|iiv|bsv|interindividual|between.subject", description, ignore.case = TRUE)) {
      metric <- "cv_percent"
      metric_inferred <- TRUE
    }
  }

  value <- switch(metric,
    variance = raw,
    sd = raw^2,
    approximate_cv_percent = (raw / 100)^2,
    cv_fraction = {
      if (distribution == "log_normal") log1p(raw^2)
      else if (distribution == "normal" && is.finite(typical)) (typical * raw)^2
      else NA_real_
    },
    cv_percent = {
      fraction <- raw / 100
      if (distribution == "log_normal") log1p(fraction^2)
      else if (distribution == "normal" && is.finite(typical)) (typical * fraction)^2
      else NA_real_
    },
    NA_real_
  )
  supported <- is.finite(value) && value >= 0
  method <- switch(metric,
    variance = "used reported NONMEM variance",
    sd = "squared the reported ETA standard deviation",
    approximate_cv_percent = "used (CV/100)^2 as explicitly reported",
    cv_fraction = if (distribution == "log_normal") "used log(1+CV^2) for an exponential ETA" else "converted CV to absolute SD using TVP, then squared",
    cv_percent = if (distribution == "log_normal") "used log(1+(CV/100)^2) for an exponential ETA" else "converted CV% to absolute SD using TVP, then squared",
    "could not determine the variability metric"
  )
  list(
    value = value, supported = supported,
    review = !supported || metric_inferred || distribution_inferred || distribution %in% c("other", "unknown"),
    metric = metric, distribution = distribution,
    note = paste0(method,
                  if (metric_inferred) "; metric inferred from context" else "",
                  if (distribution_inferred) "; ETA distribution inferred from generated parameterization" else "")
  )
}

.library_sigma_variance <- function(item) {
  description <- paste(as.character(item$description %||% ""),
                       as.character(item$error_model %||% ""))
  reported <- .library_numeric_value(item$reported_value)
  converted <- .library_numeric_value(item$value)
  metric <- .library_metric_name(item$reported_metric)
  metric_inferred <- FALSE
  raw <- if (is.finite(reported)) reported else converted
  if (!is.finite(raw) || raw < 0) {
    return(list(value = NA_real_, supported = FALSE, review = TRUE, metric = metric,
                note = "No usable residual-variability value was reported."))
  }
  if (metric %in% c("unknown", "")) {
    if (raw <= 1) metric <- "variance"
    else if (raw <= 500 && grepl("cv|percent|proportional", description, ignore.case = TRUE)) {
      metric <- "cv_percent"
      metric_inferred <- TRUE
    }
  }
  value <- switch(metric,
    variance = raw,
    sd = raw^2,
    cv_fraction = raw^2,
    cv_percent = (raw / 100)^2,
    NA_real_
  )
  supported <- is.finite(value) && value >= 0
  method <- switch(metric,
    variance = "used reported NONMEM variance",
    sd = "squared the reported residual standard deviation",
    cv_fraction = "squared the reported proportional CV fraction",
    cv_percent = "used (CV/100)^2 for proportional residual error",
    "could not determine the residual-variability metric"
  )
  list(value = value, supported = supported,
       review = !supported || metric_inferred, metric = metric,
       note = paste0(method, if (metric_inferred) "; metric inferred from context" else ""))
}

.library_covariance_value <- function(item, variances) {
  reported <- .library_numeric_value(item$reported_value)
  converted <- .library_numeric_value(item$value)
  metric <- .library_metric_name(item$reported_metric)
  raw <- if (is.finite(reported)) reported else converted
  row <- suppressWarnings(as.integer(item$row_eta %||% NA_integer_))
  col <- suppressWarnings(as.integer(item$col_eta %||% NA_integer_))
  inferred <- FALSE
  if (metric == "unknown" && is.finite(converted)) metric <- "covariance"
  if (metric == "correlation" && is.finite(raw) && abs(raw) > 1 && abs(raw) <= 100) {
    raw <- raw / 100
    inferred <- TRUE
  }
  value <- if (metric == "correlation" && is.finite(raw) && abs(raw) <= 1 &&
               is.finite(row) && is.finite(col) && row <= length(variances) && col <= length(variances) &&
               is.finite(variances[[row]]) && is.finite(variances[[col]])) {
    raw * sqrt(variances[[row]] * variances[[col]])
  } else if (metric == "covariance") raw else NA_real_
  list(row = row, col = col, value = value, supported = is.finite(value),
       review = inferred || !is.finite(value), metric = metric,
       note = if (metric == "correlation") "converted correlation to covariance" else "used reported covariance")
}

#' Map extracted evidence to a reviewable NONMEM control stream
#'
#' Only established LibeRation templates (ADVAN1-4/11/12) are generated. Any
#' missing structural or parameter evidence is explicitly recorded in the
#' `mapping` attribute and the resulting catalogue entry cannot be promoted to
#' validated status without review.
#' @param extraction Parsed extraction list.
#' @param metadata Optional publication metadata.
#' @return Character control stream with a `mapping` attribute.
#' @export
ingest_map_to_ctl <- function(extraction, metadata = NULL) {
  meta <- if (is.null(metadata)) list(title = extraction$title %||% "Literature model") else ingest_coalesce_metadata(metadata)
  sm <- extraction$structural_model %||% list()
  implementations <- sm$implementations %||% list()
  implementation <- if (is.list(implementations) && length(implementations) && is.list(implementations[[1L]])) implementations[[1L]] else list()
  advan <- as.integer(.library_semantic_number(implementation$advan %||% sm$advan) %||% NA_integer_)
  inferred <- !identical(implementation$status %||% if (is.finite(advan)) "reported" else "unresolved", "reported")
  if (!is.finite(advan)) {
    ncomp <- as.integer(.library_semantic_number(sm$compartments) %||% NA_integer_)
    oral <- tolower(.library_semantic_scalar(extraction$route)) %in% c("oral", "po", "per os")
    advan <- if (is.finite(ncomp) && ncomp == 1L) {
      if (oral) 2L else 1L
    } else if (is.finite(ncomp) && ncomp == 2L) {
      if (oral) 4L else 3L
    } else NA_integer_
    inferred <- is.finite(advan)
  }
  spec <- if (is.finite(advan)) .library_template_spec(advan) else NULL
  if (is.null(spec)) {
    advan <- 2L; spec <- .library_template_spec(advan); inferred <- TRUE
  }
  trans <- as.integer(.library_semantic_number(implementation$trans %||% sm$trans %||% spec$trans) %||% NA_integer_)
  if (!is.finite(trans)) trans <- spec$trans
  extracted <- extraction$parameters$theta %||% list()
  values <- spec$values
  names <- spec$names
  supported <- logical(length(values))
  for (i in seq_along(values)) {
    if (i <= length(extracted)) {
      candidate <- suppressWarnings(as.numeric(extracted[[i]]$typical %||% NA_real_))
      if (is.finite(candidate) && candidate > 0) {
        values[[i]] <- candidate; supported[[i]] <- TRUE
      }
      label <- trimws(as.character(extracted[[i]]$name %||% ""))
      if (nzchar(label)) names[[i]] <- gsub("[^A-Za-z0-9_.-]", "_", label)
    }
  }
  theta_lines <- vapply(seq_along(values), function(i) {
    lower <- max(0, values[[i]] / 1000)
    upper <- values[[i]] * 1000
    sprintf(" (%s, %s, %s) ; %s%s", format(lower, digits = 12), format(values[[i]], digits = 12),
            format(upper, digits = 12), names[[i]], if (supported[[i]]) "" else " [GENERATED DEFAULT]")
  }, character(1))
  omega <- extraction$parameters$omega %||% list()
  omega_values <- rep(0.1, spec$eta)
  omega_supported <- rep(FALSE, spec$eta)
  omega_conversions <- vector("list", spec$eta)
  for (i in seq_along(omega)) {
    eta_index <- suppressWarnings(as.integer(omega[[i]]$eta_index %||% i))
    if (!is.finite(eta_index) || eta_index < 1L || eta_index > spec$eta) next
    theta_index <- min(eta_index, length(values))
    conversion <- .library_omega_variance(
      omega[[i]], typical = values[[theta_index]], template_log_normal = TRUE
    )
    omega_conversions[[eta_index]] <- conversion
    if (isTRUE(conversion$supported)) {
      omega_values[[eta_index]] <- conversion$value
      omega_supported[[eta_index]] <- TRUE
    }
  }
  sigma <- extraction$parameters$sigma %||% list()
  sigma_values <- rep(0.05, max(1L, length(sigma)))
  sigma_supported <- rep(FALSE, length(sigma_values))
  sigma_conversions <- vector("list", length(sigma_values))
  if (length(sigma)) {
    for (i in seq_along(sigma)) {
      conversion <- .library_sigma_variance(sigma[[i]])
      sigma_conversions[[i]] <- conversion
      if (isTRUE(conversion$supported)) {
        sigma_values[[i]] <- conversion$value
        sigma_supported[[i]] <- TRUE
      }
    }
  }
  covariance <- extraction$parameters$omega_covariance %||% list()
  covariance_conversions <- lapply(covariance, .library_covariance_value, variances = omega_values)
  covariance_matrix <- diag(omega_values, nrow = length(omega_values))
  covariance_supported <- matrix(FALSE, nrow = length(omega_values), ncol = length(omega_values))
  diag(covariance_supported) <- omega_supported
  for (conversion in covariance_conversions) {
    if (!isTRUE(conversion$supported) || conversion$row < 1L || conversion$col < 1L ||
        conversion$row > nrow(covariance_matrix) || conversion$col > ncol(covariance_matrix) ||
        conversion$row == conversion$col) next
    covariance_matrix[conversion$row, conversion$col] <- conversion$value
    covariance_matrix[conversion$col, conversion$row] <- conversion$value
    covariance_supported[conversion$row, conversion$col] <- TRUE
    covariance_supported[conversion$col, conversion$row] <- TRUE
  }
  use_omega_block <- length(covariance) > 0L
  omega_lines <- if (use_omega_block) {
    vapply(seq_len(nrow(covariance_matrix)), function(i) {
      paste(" ", paste(format(covariance_matrix[i, seq_len(i)], digits = 12), collapse = " "))
    }, character(1))
  } else paste0(" ", format(omega_values, digits = 12))
  omega_record <- if (use_omega_block) {
    sprintf("$OMEGA BLOCK(%d)", nrow(covariance_matrix))
  } else {
    "$OMEGA"
  }
  pk_lines <- spec$pred
  for (i in seq_along(omega_conversions)) {
    conversion <- omega_conversions[[i]]
    if (!is.null(conversion) && identical(conversion$distribution, "normal")) {
      pk_lines <- gsub(paste0(" * exp(ETA(", i, "))"), paste0(" + ETA(", i, ")"),
                       pk_lines, fixed = TRUE)
    }
  }
  unmapped_theta <- if (length(extracted) > length(values)) {
    vapply(extracted[(length(values) + 1L):length(extracted)], function(x) {
      as.character(x$name %||% "unnamed parameter")[[1L]]
    }, character(1))
  } else character()
  variability_defaults <- c(
    sprintf("OMEGA(%d)", which(!omega_supported)),
    sprintf("SIGMA(%d)", which(!sigma_supported)),
    if (use_omega_block) {
      missing_cov <- which(lower.tri(covariance_supported) & !covariance_supported, arr.ind = TRUE)
      if (nrow(missing_cov)) sprintf("OMEGA(%d,%d)", missing_cov[, 1L], missing_cov[, 2L]) else character()
    } else character()
  )
  residual <- trimws(as.character(extraction$residual_error %||% ""))
  if (!nzchar(residual) || !grepl("\\bY\\s*=", residual, ignore.case = TRUE)) residual <- "Y = F * (1 + ERR(1))"
  title <- gsub("[\r\n]+", " ", substr(meta$title %||% extraction$title %||% "Literature model", 1L, 180L))
  ctl <- c(
    paste("$PROBLEM", title), sprintf("$SUBROUTINES ADVAN%d TRANS%d", advan, trans),
    "$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV", "$DATA literature.csv IGNORE=@",
    "$THETA", theta_lines, omega_record, omega_lines,
    "$SIGMA", paste0(" ", format(sigma_values, digits = 12)), "$PK", pk_lines,
    "$ERROR", residual, "",
    paste0("; LibeRary generated review draft; evidence confidence ", extraction$confidence$overall %||% 0),
    "; Generated defaults are modelling suggestions, not reported publication values.",
    vapply(which(!vapply(omega_conversions, is.null, logical(1))), function(i) {
      paste0("; OMEGA(", i, "): ", omega_conversions[[i]]$note)
    }, character(1)),
    vapply(which(!vapply(sigma_conversions, is.null, logical(1))), function(i) {
      paste0("; SIGMA(", i, "): ", sigma_conversions[[i]]$note)
    }, character(1)),
    if (length(unmapped_theta)) paste0("; REVIEW REQUIRED: unmapped extracted THETA values: ", paste(unmapped_theta, collapse = ", ")) else NULL,
    if (length(variability_defaults)) paste0("; REVIEW REQUIRED: variability scale unresolved; defaults used for ", paste(variability_defaults, collapse = ", ")) else NULL
  )
  validation <- NULL
  if (requireNamespace("LibeRation", quietly = TRUE)) {
    validation <- tryCatch({ LibeRation::nm_control_read(ctl, strict = TRUE); NULL }, error = conditionMessage)
  }
  generated_defaults <- c(names[!supported], variability_defaults)
  variability_review <- any(vapply(c(omega_conversions, sigma_conversions, covariance_conversions),
                                   function(x) !is.null(x) && isTRUE(x$review), logical(1)))
  attr(ctl, "mapping") <- list(
    advan = advan, trans = trans, structure_inferred = inferred,
    implementation = implementation,
    generated_defaults = generated_defaults,
    unmapped_theta = unmapped_theta,
    omega_conversions = omega_conversions,
    sigma_conversions = sigma_conversions,
    covariance_conversions = covariance_conversions,
    eta_distributions = vapply(omega_conversions, function(x) x$distribution %||% "unknown", character(1)),
    validation_error = validation,
    review_required = inferred || isTRUE(implementation$review_required) || any(!supported) || length(variability_defaults) > 0L ||
      length(unmapped_theta) > 0L || variability_review || !is.null(validation)
  )
  ctl
}

.library_version_next <- function(version) {
  parts <- suppressWarnings(as.integer(strsplit(as.character(version %||% "0.0.0"), "[.]", fixed = FALSE)[[1L]]))
  if (length(parts) != 3L || anyNA(parts)) parts <- c(0L, 0L, 0L)
  paste(parts[[1L]], parts[[2L]], parts[[3L]] + 1L, sep = ".")
}

.library_bibtex <- function(metadata, library_id) {
  meta <- ingest_coalesce_metadata(metadata)
  authors <- as.character(unlist(metadata$authors %||% character()))
  author <- if (length(authors)) paste(authors, collapse = " and ") else "Unknown"
  clean <- function(x) gsub("[{}]", "", as.character(x %||% ""))
  c(paste0("@article{", library_id, ","), paste0("  title = {", clean(meta$title), "},"),
    paste0("  author = {", clean(author), "},"), paste0("  journal = {", clean(meta$journal), "},"),
    paste0("  year = {", clean(meta$year), "},"),
    if (nzchar(meta$doi)) paste0("  doi = {", clean(meta$doi), "},") else NULL,
    if (nzchar(meta$pmid)) paste0("  pmid = {", clean(meta$pmid), "}") else "  note = {LibeRary literature entry}",
    "}")
}

.library_copy_tree <- function(source, destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(source)) return(invisible(TRUE))
  items <- list.files(source, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(items) && !all(file.copy(items, destination, recursive = TRUE, copy.date = TRUE))) {
    stop("Unable to prepare a transactional catalogue copy.", call. = FALSE)
  }
  invisible(TRUE)
}

.library_swap_catalog_entry <- function(staged, destination, catalog_dir) {
  parent <- dirname(destination)
  backup <- file.path(parent, paste0(".", basename(destination), "-backup-", Sys.getpid(), "-", sample.int(1e9, 1L)))
  had_existing <- dir.exists(destination)
  if (had_existing && !file.rename(destination, backup)) {
    stop("Unable to move the current catalogue entry into transactional backup.", call. = FALSE)
  }
  if (!file.rename(staged, destination)) {
    if (had_existing) file.rename(backup, destination)
    stop("Unable to activate the staged catalogue entry.", call. = FALSE)
  }
  index_error <- tryCatch({
    .library_rebuild_index(catalog_dir)
    NULL
  }, error = identity)
  if (inherits(index_error, "error")) {
    failed <- file.path(parent, paste0(".", basename(destination), "-failed-", Sys.getpid(), "-", sample.int(1e9, 1L)))
    file.rename(destination, failed)
    if (had_existing) file.rename(backup, destination)
    try(.library_rebuild_index(catalog_dir), silent = TRUE)
    unlink(failed, recursive = TRUE, force = TRUE)
    stop("Catalogue index update failed; the prior entry was restored: ",
         conditionMessage(index_error), call. = FALSE)
  }
  if (had_existing) unlink(backup, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

#' Publish an extracted model to the catalogue
#' @param metadata Publication metadata.
#' @param extraction Extraction list.
#' @param cfg Ingest configuration.
#' @param status Initial workflow status.
#' @param raw_llm Optional immutable LLM audit object.
#' @param library_id Optional stable entry id.
#' @param assessment Optional independent evidence assessment.
#' @param overwrite Update an existing entry and archive its previous version.
#' @param document_bundle Optional canonical document-bundle manifest.
#' @return Published paths and mapping details.
#' @export
ingest_publish_catalog_entry <- function(metadata, extraction, cfg = NULL, status = "stub",
                                         raw_llm = NULL, library_id = NULL,
                                         assessment = NULL, overwrite = TRUE,
                                         document_bundle = NULL) {
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  # Defend the catalogue boundary as well as the LLM parser. This also repairs
  # callers supplying a previously cached extraction containing percentages.
  extraction <- .library_normalize_llm_probabilities(extraction, "extraction")
  extraction <- library_model_enrich(extraction)
  if (!is.null(assessment)) {
    assessment <- .library_normalize_llm_probabilities(assessment, "assessment")
  }
  ingest_ensure_dirs(cfg); .library_initialize_catalog(cfg$catalog_dir)
  meta <- ingest_coalesce_metadata(metadata)
  fallback <- substr(.library_text_fingerprint(paste(meta$title, meta$doi)), 1L, 12L)
  library_id <- .library_valid_id(library_id %||% paste0("pmid_", if (nzchar(meta$pmid)) meta$pmid else fallback))
  if (!status %in% LIBRARY_STATUS_LEVELS) stop("Unknown catalogue status: ", status)
  if (status %in% c("validated", "mbma_source")) {
    stop(
      "Automated publication cannot create a `", status,
      "` catalogue entry. Publish to draft/review first, then use `library_review()` ",
      "after the qualification gate has passed.", call. = FALSE
    )
  }
  .library_with_lock(cfg$catalog_dir, {
    final_entry_dir <- .library_entry_dir(library_id, cfg$catalog_dir)
    old_manifest <- if (file.exists(file.path(final_entry_dir, "manifest.json")))
      jsonlite::fromJSON(file.path(final_entry_dir, "manifest.json"), simplifyVector = FALSE) else NULL
    if (!is.null(old_manifest) && !isTRUE(overwrite)) stop("Catalogue entry already exists: ", library_id)
    if (!is.null(old_manifest) && identical(old_manifest$status, "validated") && status != "validated") {
      stop("A validated entry cannot be overwritten by an unreviewed extraction.")
    }
    version <- if (is.null(old_manifest)) "1.0.0" else .library_version_next(old_manifest$version)
    entry_dir <- file.path(dirname(final_entry_dir), paste0(".", library_id, "-stage-", Sys.getpid(), "-", sample.int(1e9, 1L)))
    if (is.null(old_manifest)) dir.create(entry_dir, recursive = TRUE, showWarnings = FALSE) else
      .library_copy_tree(final_entry_dir, entry_dir)
    stage_active <- TRUE
    on.exit(if (isTRUE(stage_active)) unlink(entry_dir, recursive = TRUE, force = TRUE), add = TRUE)
    if (!is.null(old_manifest)) {
      archive <- file.path(entry_dir, "versions", old_manifest$version %||% "unknown")
      dir.create(archive, recursive = TRUE, showWarnings = FALSE)
      for (name in c("manifest.json", "model.ctl", "references.bib")) {
        source <- file.path(entry_dir, name)
        if (file.exists(source) && !file.exists(file.path(archive, name))) file.copy(source, archive)
      }
      old_extraction <- file.path(entry_dir, "extraction")
      archived_extraction <- file.path(archive, "extraction")
      if (dir.exists(old_extraction) && !dir.exists(archived_extraction)) {
        dir.create(archived_extraction, recursive = TRUE, showWarnings = FALSE)
        artefacts <- list.files(old_extraction, all.files = TRUE, no.. = TRUE, full.names = TRUE)
        if (length(artefacts) && !all(file.copy(artefacts, archived_extraction, recursive = TRUE))) {
          stop("Unable to archive the prior extraction audit for ", library_id, ".")
        }
      }
      old_reproduction <- file.path(entry_dir, "reproduction")
      archived_reproduction <- file.path(archive, "reproduction")
      if (dir.exists(old_reproduction) && !dir.exists(archived_reproduction)) {
        dir.create(archived_reproduction, recursive = TRUE, showWarnings = FALSE)
        artefacts <- list.files(old_reproduction, all.files = TRUE, no.. = TRUE, full.names = TRUE)
        if (length(artefacts) && !all(file.copy(artefacts, archived_reproduction, recursive = TRUE))) {
          stop("Unable to archive the prior reproduction audit for ", library_id, ".")
        }
      }
    }
    # Never mix extraction or assessment artefacts from different versions.
    unlink(file.path(entry_dir, "extraction"), recursive = TRUE, force = TRUE)
    dir.create(file.path(entry_dir, "extraction"), recursive = TRUE, showWarnings = FALSE)
    ctl <- ingest_map_to_ctl(extraction, metadata = meta)
    mapping <- attr(ctl, "mapping")
    .library_atomic_write_lines(ctl, file.path(entry_dir, "model.ctl"))
    .library_atomic_write_lines(.library_bibtex(metadata, library_id), file.path(entry_dir, "references.bib"))
    if (!is.null(raw_llm)) .library_atomic_write(raw_llm, file.path(entry_dir, "extraction", "raw_llm.json"))
    evidence_ledger_source <- as.character(
      raw_llm$text$evidence_ledger_path %||% raw_llm$evidence_ledger_path %||% ""
    )[[1L]]
    evidence_ledger_artifact <- NULL
    if (nzchar(evidence_ledger_source) && file.exists(evidence_ledger_source)) {
      evidence_ledger_target <- file.path(entry_dir, "extraction", "evidence-ledger.json")
      if (!file.copy(evidence_ledger_source, evidence_ledger_target, overwrite = TRUE)) {
        stop("Unable to preserve the evidence ledger for ", library_id, ".")
      }
      evidence_ledger_artifact <- "extraction/evidence-ledger.json"
    }
    .library_atomic_write(extraction, file.path(entry_dir, "extraction", "parsed.json"))
    if (!is.null(assessment)) .library_atomic_write(assessment, file.path(entry_dir, "extraction", "assessment.json"))
    reproduction <- list(enabled = FALSE, eligible = FALSE, scorable = FALSE,
                         status = "not_planned", blockers = character())
    if (isTRUE(cfg$reproduction$enabled)) {
      reproduction_dir <- file.path(entry_dir, "reproduction")
      unlink(reproduction_dir, recursive = TRUE, force = TRUE)
      dir.create(reproduction_dir, recursive = TRUE, showWarnings = FALSE)
      plan <- tryCatch(library_reproduction_plan(
          extraction,
          allow_generated_defaults = cfg$reproduction$allow_generated_defaults
        ), error = identity)
      if (inherits(plan, "error")) {
        reproduction <- list(enabled = TRUE, eligible = FALSE, scorable = FALSE,
                             status = "failed", blockers = "reproduction_planning_failed",
                             error = conditionMessage(plan), error_artifact = "reproduction/error.json")
        .library_atomic_write(list(
          status = "failed", stage = "planning", error = conditionMessage(plan),
          created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
        ), file.path(reproduction_dir, "error.json"))
      } else {
        plan_audit <- plan
        plan_audit$source_extraction <- NULL
        .library_atomic_write(plan_audit, file.path(reproduction_dir, "plan.json"))
        reproduction <- list(
          enabled = TRUE, eligible = isTRUE(plan$eligible), scorable = isTRUE(plan$scorable),
          status = if (isTRUE(plan$eligible)) "planned" else "blocked",
          blockers = plan$blockers, assumptions = plan$assumptions,
          plan_artifact = "reproduction/plan.json"
        )
        if (isTRUE(cfg$reproduction$auto_run) && isTRUE(plan$eligible)) {
          run <- tryCatch(library_reproduction_run(
            plan, output_dir = reproduction_dir, nsim = cfg$reproduction$nsim,
            seed = cfg$reproduction$seed, n_cores = cfg$reproduction$n_cores,
            allow_generated_defaults = cfg$reproduction$allow_generated_defaults
          ), error = identity)
          if (inherits(run, "error")) {
            reproduction$status <- "failed"
            reproduction$error <- conditionMessage(run)
          } else {
            reproduction$status <- run$status
            reproduction$score <- run$score$summary
            reproduction$result_artifact <- "reproduction/reproduction.json"
            reproduction$figure_artifact <- "reproduction/reproduction.png"
          }
        }
      }
    }
    now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    manifest <- list(
      schema_version = LIBRARY_SCHEMA_VERSION, library_id = library_id, version = version,
      status = status, title = extraction$title %||% meta$title, created_at = old_manifest$created_at %||% now,
      updated_at = now,
      model = list(artifact = "model.ctl", advan = mapping$advan, trans = mapping$trans,
                   type = extraction$model_type %||% "unknown", use_ode = mapping$advan %in% c(6L, 13L),
                   canonical = extraction$structural_model$canonical,
                   implementations = extraction$structural_model$implementations,
                   generated_suggestion = TRUE, generated_defaults = mapping$generated_defaults,
                   eta_distributions = mapping$eta_distributions,
                   omega_conversions = mapping$omega_conversions,
                   sigma_conversions = mapping$sigma_conversions,
                   covariance_conversions = mapping$covariance_conversions,
                   validation_error = mapping$validation_error),
      study = list(compound = extraction$compound %||% "", population = extraction$population %||% "",
                   route = extraction$route %||% "", n_subjects = extraction$n_subjects %||% NULL,
                   population_details = extraction$population_details,
                   dosing = extraction$dosing,
                   keywords = ingest_score_model_relevance(paste(meta$title, meta$abstract))$keywords,
                   abstract = meta$abstract, dataset_fingerprint = paste0("pmid:", meta$pmid)),
      confidence = extraction$confidence %||% list(overall = 0, fields = list()),
      provenance = list(source_type = if (status == "stub") "literature_stub" else "literature",
                        pmid = meta$pmid, doi = meta$doi, journal = meta$journal, year = meta$year,
                        authors = metadata$authors %||% character(), software = extraction$software %||% NULL,
                        estimation_method = extraction$estimation_method %||% NULL,
                        source_text_stored = FALSE, llm_audit = !is.null(raw_llm),
                        extraction_pipeline = raw_llm$text$pipeline %||% NULL,
                        extraction_pipeline_version = raw_llm$text$pipeline_version %||% NULL,
                        evidence_ledger = evidence_ledger_artifact,
                        source_document_sha256 = document_bundle$source$sha256 %||% NULL,
                        document_parser = document_bundle$parser$name %||% NULL,
                        document_bundle = document_bundle$manifest_path %||% NULL),
      qualification = list(author_validated = FALSE, human_reviewed = FALSE,
                           automated_assessment = assessment %||% list(status = "not_run"),
                           mapping_review_required = mapping$review_required,
                           reproduction = reproduction, simulation_checks = list(reproduction)),
      relations = old_manifest$relations %||% list(same_compound = character(), prior_version = character(), mbma_pool = FALSE)
    )
    checks <- library_validate(manifest = manifest, root = cfg$catalog_dir, check_artifact = FALSE)
    if (!checks$valid) stop(paste(checks$errors, collapse = "; "))
    .library_atomic_write(manifest, file.path(entry_dir, "manifest.json"))
    .library_swap_catalog_entry(entry_dir, final_entry_dir, cfg$catalog_dir)
    stage_active <- FALSE
    list(library_id = library_id, entry_dir = final_entry_dir,
         manifest_path = file.path(final_entry_dir, "manifest.json"),
         ctl_path = file.path(final_entry_dir, "model.ctl"), version = version,
         mapping = mapping, reproduction = reproduction)
  })
}

#' Rebuild the catalogue index (compatibility helper)
#' @keywords internal
ingest_update_catalog_index <- function(catalog_dir, library_id = NULL, title = NULL, status = NULL) {
  .library_initialize_catalog(catalog_dir)
  .library_rebuild_index(catalog_dir)
  invisible(file.path(catalog_dir, "index.json"))
}

#' Batch extract, assess and publish discovered publications
#' @param manifest Discover manifest path or data frame.
#' @param cfg Ingest configuration.
#' @param limit Maximum records.
#' @param use_ollama Legacy switch; `FALSE` creates metadata stubs.
#' @param assess Run the independently configured assessment role.
#' @param resume Skip PMIDs already present in the target catalogue.
#' @param overwrite Update existing entries when `resume=FALSE`.
#' @param log Optional logger.
#' @param progress Optional progress callback.
#' @return Batch results and provider summary.
#' @export
ingest_extract_batch <- function(manifest, cfg = NULL, limit = 10L, use_ollama = TRUE,
                                 assess = TRUE, resume = TRUE, overwrite = TRUE,
                                 log = NULL, progress = NULL) {
  log <- log %||% function(...) invisible(NULL)
  prog <- function(value, message, step = NA_integer_, total = NA_integer_) if (!is.null(progress)) progress(value, message, step, total)
  cfg <- if (is.character(cfg) && length(cfg) == 1L) ingest_load_config(cfg) else if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  if (!isTRUE(use_ollama)) cfg$llm$indexing$provider <- "none"
  ingest_ensure_dirs(cfg); .library_initialize_catalog(cfg$catalog_dir)
  df <- if (is.character(manifest) && length(manifest) == 1L && file.exists(manifest)) ingest_read_manifest(manifest) else if (is.data.frame(manifest)) manifest else stop("`manifest` must be a CSV path or data frame.")
  df <- df[df$acquisition_class != "skipped", , drop = FALSE]
  if (nrow(df) > limit) df <- df[seq_len(limit), , drop = FALSE]
  existing <- tryCatch(library_list(root = cfg$catalog_dir)$library_id, error = function(e) character())
  results <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    pmid <- as.character(df$pmid[[i]])
    entry_id <- paste0("pmid_", pmid)
    if (isTRUE(resume) && entry_id %in% existing) {
      log(sprintf("Skipping PMID %s (already indexed)", pmid), "INFO")
      results[[i]] <- list(pmid = pmid, library_id = entry_id, skipped = TRUE)
      next
    }
    prog((i - 1) / max(1, nrow(df)), sprintf("Indexing PMID %s (%d/%d)", pmid, i, nrow(df)), i, nrow(df))
    metadata <- ingest_entrez_fetch_metadata(pmid, cfg = cfg, use_cache = TRUE)[[pmid]]
    if (is.null(metadata)) metadata <- list(pmid = pmid, title = df$title[[i]] %||% "", abstract = "",
      doi = df$doi[[i]] %||% "", journal = df$publisher[[i]] %||% "", year = df$year[[i]] %||% "", pmcid = df$pmcid[[i]] %||% "", authors = character())
    pdf <- file.path(cfg$inbox_dir, pmid, "article.pdf")
    ex <- ingest_extract_model(metadata, cfg, if (file.exists(pdf)) pdf else NULL, assess = assess)
    pub <- ingest_publish_catalog_entry(metadata, ex$extraction, cfg, ex$status, ex$raw_llm,
                                        entry_id, assessment = ex$assessment, overwrite = overwrite)
    results[[i]] <- c(pub, list(pmid = pmid, used_full_text = ex$used_full_text, skipped = FALSE))
    log(sprintf("Published %s (%s)", pub$library_id, ex$status), "INFO")
  }
  prog(1, "Indexing complete", nrow(df), nrow(df))
  list(results = results, summary = list(processed = sum(!vapply(results, function(x) isTRUE(x$skipped), logical(1))),
    skipped = sum(vapply(results, function(x) isTRUE(x$skipped), logical(1))), catalog_dir = cfg$catalog_dir,
    indexing = .library_llm_role(cfg, "indexing")[c("provider", "model")],
    assessment = .library_llm_role(cfg, "assessment")[c("provider", "model")]))
}
