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
        assessment = "", reproduction = "", updated_at = "",
        stringsAsFactors = FALSE
      ))
    }
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
    checks <- library_validate(library_id, root = root)
    if (!checks$valid) stop(paste(checks$errors, collapse = "; "))
    now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    decision <- list(status = status, reviewer = reviewer, notes = as.character(notes), at = now)
    manifest$review_history <- c(manifest$review_history %||% list(), list(decision))
    manifest$status <- status
    manifest$updated_at <- now
    manifest$qualification$human_reviewed <- TRUE
    manifest$qualification$reviewer <- reviewer
    .library_atomic_write(manifest, file.path(.library_entry_dir(library_id, root), "manifest.json"))
    .library_rebuild_index(root)
    invisible(manifest)
  })
}
