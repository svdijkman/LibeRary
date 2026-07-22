#' List catalog entries
#'
#' @param status Optional status filter (e.g. \code{"validated"}, \code{"stub"}).
#' @param root Catalog root; default [library_catalog_root()].
#' @return Data frame of summary fields per entry.
#' @export
library_list <- function(status = NULL, root = library_catalog_root()) {
  idx <- .library_read_index(root)
  entries <- idx$entries
  if (!length(entries)) {
    return(data.frame(
      library_id = character(),
      title = character(),
      status = character(),
      compound = character(),
      population = character(),
      advan = integer(),
      trans = integer(),
      model_type = character(),
      version = character(),
      confidence_overall = numeric(),
      assessment = character(),
      reproduction = character(),
      qualified = logical(),
      qualification_blockers = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(entries, function(e) {
    manifest <- tryCatch(
      .library_read_manifest(e$library_id, root),
      error = function(err) NULL
    )
    if (is.null(manifest)) {
      return(data.frame(
        library_id = e$library_id %||% "",
        title = e$title %||% "",
        status = e$status %||% "",
        compound = "",
        population = "",
        advan = NA_integer_,
        trans = NA_integer_, model_type = "", version = "",
        confidence_overall = NA_real_,
        assessment = "", reproduction = "", qualified = FALSE,
        qualification_blockers = "manifest_unreadable", updated_at = "",
        stringsAsFactors = FALSE
      ))
    }
    gate <- manifest$qualification$gate %||% list(
      ready = FALSE, blockers = "qualification_not_run"
    )
    data.frame(
      library_id = manifest$library_id %||% e$library_id,
      title = manifest$title %||% "",
      status = manifest$status %||% "",
      compound = manifest$study$compound %||% "",
      population = manifest$study$population %||% "",
      advan = as.integer(manifest$model$advan %||% NA),
      trans = as.integer(manifest$model$trans %||% NA),
      model_type = as.character(manifest$model$type %||% ""),
      version = as.character(manifest$version %||% ""),
      confidence_overall = as.numeric(manifest$confidence$overall %||% NA),
      assessment = as.character(manifest$qualification$automated_assessment$verdict %||% ""),
      reproduction = as.character(manifest$qualification$reproduction$status %||% "not_planned"),
      qualified = isTRUE(gate$ready),
      qualification_blockers = paste(as.character(gate$blockers %||% character()), collapse = "; "),
      updated_at = as.character(manifest$updated_at %||% ""),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  if (!is.null(status) && nzchar(status)) {
    df <- df[df$status == status, , drop = FALSE]
  }
  df
}

.library_qualification_events <- function(model) {
  dose_cmp <- as.integer(model$DOSECMP %||% 1L)
  obs_cmp <- as.integer(model$OBSCMP %||% dose_cmp)
  events <- data.frame(
    ID = 1L, TIME = c(0, 0.5, 1, 2, 4, 8, 24),
    EVID = c(1L, rep(0L, 6L)), AMT = c(100, rep(0, 6L)),
    RATE = 0, II = 0, SS = 0L,
    CMT = c(dose_cmp, rep(obs_cmp, 6L)),
    DV = NA_real_, MDV = c(1L, rep(0L, 6L)),
    stringsAsFactors = FALSE
  )
  required <- unique(as.character(model$INPUT %||% character()))
  for (name in setdiff(required, names(events))) {
    upper <- toupper(name)
    events[[name]] <- if (upper %in% c("WT", "WEIGHT")) 70 else
      if (upper %in% c("AGE")) 40 else if (upper %in% c("SEX", "MALE")) 0 else 1
  }
  events
}

#' Evaluate whether an extracted catalogue model is ready for validation
#'
#' This deterministic gate keeps machine-extracted entries quarantined until
#' their evidence, mapping, control stream, and executable prediction path are
#' internally consistent. Passing the gate establishes computational
#' qualification; it is not evidence that the publication or model is correct.
#' @param library_id Catalogue identifier.
#' @param root Catalogue root.
#' @param min_confidence Minimum overall extraction confidence.
#' @param simulate Run a deterministic, residual-free simulation smoke test.
#' @return A `library_qualification` report.
#' @export
library_qualification_check <- function(library_id, root = library_catalog_root(),
                                        min_confidence = 0.8, simulate = TRUE) {
  min_confidence <- as.numeric(min_confidence)
  if (length(min_confidence) != 1L || !is.finite(min_confidence) ||
      min_confidence < 0 || min_confidence > 1) {
    stop("`min_confidence` must be one number between zero and one.", call. = FALSE)
  }
  entry <- library_get(library_id, root)
  manifest <- entry$manifest
  blockers <- character()
  checks <- entry$validation
  if (!isTRUE(checks$valid)) blockers <- c(blockers, "catalogue_schema_invalid")
  confidence <- suppressWarnings(as.numeric(manifest$confidence$overall %||% NA_real_))
  if (!is.finite(confidence) || confidence < min_confidence) {
    blockers <- c(blockers, "confidence_below_threshold")
  }
  if (isTRUE(manifest$qualification$mapping_review_required)) {
    blockers <- c(blockers, "mapping_review_required")
  }
  mapping_error <- as.character(manifest$model$validation_error %||% "")
  if (length(mapping_error) && nzchar(mapping_error[[1L]])) {
    blockers <- c(blockers, "control_stream_validation_failed")
  }
  parsed_path <- file.path(entry$paths$extraction, "parsed.json")
  parsed <- if (file.exists(parsed_path)) {
    tryCatch(jsonlite::fromJSON(parsed_path, simplifyVector = FALSE), error = function(e) NULL)
  } else NULL
  evidence <- c(
    as.character(unlist(parsed$evidence_quotes %||% character(), use.names = FALSE)),
    as.character(unlist(parsed$evidence %||% character(), use.names = FALSE))
  )
  ledger <- manifest$provenance$evidence_ledger %||% ""
  ledger_path <- if (nzchar(as.character(ledger)[[1L]])) {
    file.path(entry$paths$entry_dir, as.character(ledger)[[1L]])
  } else ""
  if (!any(nzchar(trimws(evidence))) && !(nzchar(ledger_path) && file.exists(ledger_path))) {
    blockers <- c(blockers, "evidence_missing")
  }
  compile <- list(attempted = FALSE, passed = FALSE, error = NULL)
  simulation <- list(attempted = FALSE, passed = FALSE, error = NULL,
                     finite_predictions = 0L)
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    blockers <- c(blockers, "liberation_unavailable")
  } else {
    compile$attempted <- TRUE
    control <- tryCatch(
      LibeRation::nm_control_read(library_model(library_id, root), strict = TRUE),
      error = identity
    )
    if (inherits(control, "error")) {
      compile$error <- conditionMessage(control)
      blockers <- c(blockers, "control_stream_compile_failed")
    } else {
      compile$passed <- TRUE
      if (isTRUE(simulate)) {
        simulation$attempted <- TRUE
        result <- tryCatch(
          LibeRation::nm_simulate(
            control$model, .library_qualification_events(control$model),
            random_effects = FALSE, residual = FALSE, seed = 1L
          ),
          error = identity
        )
        if (inherits(result, "error")) {
          simulation$error <- conditionMessage(result)
          blockers <- c(blockers, "simulation_smoke_test_failed")
        } else {
          observed <- result$EVID == 0L
          predicted <- suppressWarnings(as.numeric(result$IPRED[observed]))
          simulation$finite_predictions <- sum(is.finite(predicted))
          simulation$passed <- length(predicted) > 0L && all(is.finite(predicted))
          if (!simulation$passed) blockers <- c(blockers, "non_finite_predictions")
        }
      } else {
        simulation$passed <- NA
      }
    }
  }
  blockers <- unique(blockers)
  report <- structure(list(
    schema_version = "1.0.0", library_id = library_id,
    checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    ready = !length(blockers), blockers = blockers,
    criteria = list(min_confidence = min_confidence, simulate = isTRUE(simulate)),
    confidence = confidence, compile = compile, simulation = simulation,
    interpretation = paste(
      "Computational qualification checks internal consistency only; it does not",
      "independently validate the publication, estimates, or clinical suitability."
    )
  ), class = c("library_qualification", "list"))
  report
}

#' @export
print.library_qualification <- function(x, ...) {
  cat("LibeRary computational qualification\n")
  cat("  entry:", x$library_id, "\n")
  cat("  ready:", if (isTRUE(x$ready)) "yes" else "no", "\n")
  if (length(x$blockers)) cat("  blockers:", paste(x$blockers, collapse = ", "), "\n")
  invisible(x)
}

#' Get a catalog entry
#'
#' @param library_id Entry identifier.
#' @param root Catalog root.
#' @return List with \code{manifest}, \code{paths}, and \code{summary}.
#' @export
library_get <- function(library_id, root = library_catalog_root()) {
  manifest <- .library_read_manifest(library_id, root)
  paths <- .library_entry_paths(library_id, root)
  list(
    library_id = library_id,
    manifest = manifest,
    paths = paths,
    validation = library_validate(library_id, root = root),
    summary = list(
      title = manifest$title,
      status = manifest$status,
      compound = manifest$study$compound %||% "",
      population = manifest$study$population %||% "",
      confidence = manifest$confidence$overall %||% NA_real_
    )
  )
}

#' Read model control stream for an entry
#'
#' @param library_id Entry identifier.
#' @param root Catalog root.
#' @return Character vector of \code{model.ctl} lines.
#' @export
library_model <- function(library_id, root = library_catalog_root()) {
  paths <- .library_entry_paths(library_id, root)
  if (!file.exists(paths$ctl)) {
    stop("Model artifact missing for entry: ", library_id)
  }
  readLines(paths$ctl, warn = FALSE)
}

#' Locate the source PDF for a catalogue entry
#'
#' Resolves the immutable document bundle recorded in the entry provenance and
#' falls back to the persistent ingest inbox for older entries.
#' @param library_id Entry identifier.
#' @param root Catalog root.
#' @return Normalized PDF path, or an empty string when no source PDF is available.
#' @export
library_source_pdf <- function(library_id, root = library_catalog_root()) {
  manifest <- .library_read_manifest(library_id, root)
  bundle_path <- as.character(manifest$provenance$document_bundle %||% "")[[1L]]
  candidates <- character()
  if (nzchar(bundle_path) && file.exists(bundle_path)) {
    bundle <- tryCatch(jsonlite::fromJSON(bundle_path, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (!is.null(bundle)) {
      candidates <- c(candidates, bundle$source$path %||% "",
                      bundle$provenance$acquisition_path %||% "")
    }
  }
  pmid <- as.character(manifest$provenance$pmid %||% "")[[1L]]
  if (nzchar(pmid)) {
    candidates <- c(candidates, file.path(dirname(root), "inbox", pmid, "article.pdf"))
  }
  candidates <- unique(path.expand(as.character(candidates)))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates) &
                             tolower(tools::file_ext(candidates)) == "pdf"]
  if (!length(candidates)) return("")
  normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
}

#' Provenance and audit trail for an entry
#'
#' @param library_id Entry identifier.
#' @param root Catalog root.
#' @return List with manifest provenance block and optional extraction files.
#' @export
library_provenance <- function(library_id, root = library_catalog_root()) {
  entry <- library_get(library_id, root)
  m <- entry$manifest
  out <- list(
    library_id = library_id,
    provenance = m$provenance %||% list(),
    confidence = m$confidence %||% list(),
    qualification = m$qualification %||% list(),
    relations = m$relations %||% list()
  )
  raw_llm <- file.path(entry$paths$extraction, "raw_llm.json")
  if (file.exists(raw_llm)) {
    out$raw_llm <- jsonlite::fromJSON(raw_llm, simplifyVector = FALSE)
  }
  assessment <- file.path(entry$paths$extraction, "assessment.json")
  if (file.exists(assessment)) {
    out$assessment <- jsonlite::fromJSON(assessment, simplifyVector = FALSE)
  }
  ledger <- file.path(entry$paths$extraction, "evidence-ledger.json")
  if (file.exists(ledger)) {
    out$evidence_ledger <- jsonlite::fromJSON(ledger, simplifyVector = FALSE)
  }
  out
}

#' Record a human review decision
#'
#' Status changes are auditable. Promotion to `validated` requires an explicit
#' confirmation when any model code or parameter value was generated.
#' @param library_id Catalogue id.
#' @param status New workflow status.
#' @param reviewer Reviewer name or identifier.
#' @param notes Review notes.
#' @param confirm_generated Confirm that generated suggestions were reviewed.
#' @param root Catalogue root.
#' @return Updated manifest, invisibly.
#' @export
library_review <- function(library_id, status = c("review", "validated", "deprecated"),
                           reviewer, notes = "", confirm_generated = FALSE,
                           root = library_catalog_root()) {
  status <- match.arg(status)
  reviewer <- trimws(as.character(reviewer))
  if (length(reviewer) != 1L || is.na(reviewer) || !nzchar(reviewer)) stop("`reviewer` is required.")
  .library_with_lock(root, {
    manifest <- .library_read_manifest(library_id, root)
    if (status == "validated" && isTRUE(manifest$model$generated_suggestion) && !isTRUE(confirm_generated)) {
      stop("Set `confirm_generated=TRUE` only after reviewing every generated suggestion.")
    }
    gate <- if (status == "validated") {
      library_qualification_check(library_id, root = root)
    } else NULL
    if (status == "validated" && !isTRUE(gate$ready)) {
      stop(
        "The entry is not computationally qualified: ",
        paste(gate$blockers, collapse = ", "), ".", call. = FALSE
      )
    }
    checks <- library_validate(library_id, root = root)
    if (!checks$valid) stop(paste(checks$errors, collapse = "; "))
    now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    decision <- list(status = status, reviewer = reviewer, notes = as.character(notes), at = now)
    manifest$review_history <- c(manifest$review_history %||% list(), list(decision))
    manifest$status <- status
    manifest$updated_at <- now
    manifest$qualification$human_reviewed <- TRUE
    manifest$qualification$reviewer <- reviewer
    if (!is.null(gate)) {
      manifest$qualification$gate <- unclass(gate)
      manifest$qualification$author_validated <- TRUE
    }
    .library_atomic_write(manifest, file.path(.library_entry_dir(library_id, root), "manifest.json"))
    .library_rebuild_index(root)
    invisible(manifest)
  })
}
