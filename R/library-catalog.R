#' Packaged read-only catalogue root
#' @return A directory path, or an empty string in an incomplete source tree.
#' @keywords internal
.library_packaged_root <- function() {
  root <- system.file("catalog", package = "LibeRary")
  if (nzchar(root) && dir.exists(root)) return(normalizePath(root, winslash = "/"))
  candidates <- c(
    file.path(Sys.getenv("LIBERARY_PKG_ROOT", ""), "inst", "catalog"),
    file.path(getwd(), "inst", "catalog"),
    file.path(getwd(), "LibeRary", "inst", "catalog")
  )
  hit <- candidates[dir.exists(candidates)][1L]
  if (is.na(hit)) "" else normalizePath(hit, winslash = "/", mustWork = TRUE)
}

#' Catalogue root directory
#'
#' The mutable catalogue lives in the user's application-data directory, so it
#' survives upgrades and reinstalls. Packaged validated examples are seeded on
#' first use. `options(LibeRary.catalog=)` or `LIBERARY_CATALOG` can select a
#' shared or project-specific catalogue.
#' @param create Create and seed the catalogue when it does not exist.
#' @return Normalized directory path.
#' @export
library_catalog_root <- function(create = TRUE) {
  root <- getOption("LibeRary.catalog", Sys.getenv("LIBERARY_CATALOG", ""))
  if (!nzchar(root)) root <- file.path(library_home(), "catalog")
  root <- path.expand(as.character(root)[[1L]])
  if (isTRUE(create)) .library_initialize_catalog(root)
  if (!dir.exists(root)) stop("LibeRary catalogue not found: ", root, call. = FALSE)
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

.library_initialize_catalog <- function(root) {
  root <- path.expand(root)
  entries <- file.path(root, "entries")
  if (!dir.exists(entries) && !dir.create(entries, recursive = TRUE, showWarnings = FALSE)) {
    stop("Unable to create LibeRary catalogue: ", root, call. = FALSE)
  }
  packaged <- .library_packaged_root()
  seed_packaged <- !file.exists(file.path(root, ".skip-packaged-seed"))
  changed <- FALSE
  if (seed_packaged && nzchar(packaged)) {
    sources <- list.dirs(file.path(packaged, "entries"), recursive = FALSE, full.names = TRUE)
    for (source in sources) {
      target <- file.path(entries, basename(source))
      if (!dir.exists(target)) changed <- isTRUE(file.copy(source, entries, recursive = TRUE, copy.mode = TRUE)) || changed
    }
  }
  if (changed || !file.exists(file.path(root, "index.json"))) .library_rebuild_index(root)
  invisible(root)
}

.library_valid_id <- function(library_id) {
  id <- as.character(library_id)
  if (length(id) != 1L || is.na(id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]{0,127}$", id) || id %in% c(".", "..")) {
    stop("Invalid LibeRary entry id.", call. = FALSE)
  }
  id
}

.library_index_path <- function(root = library_catalog_root()) file.path(root, "index.json")

.library_entry_dir <- function(library_id, root = library_catalog_root()) {
  id <- .library_valid_id(library_id)
  base <- normalizePath(file.path(root, "entries"), winslash = "/", mustWork = TRUE)
  path <- file.path(base, id)
  if (!startsWith(tolower(normalizePath(dirname(path), winslash = "/", mustWork = TRUE)), tolower(base))) {
    stop("Catalogue path escaped its root.", call. = FALSE)
  }
  path
}

.library_read_index <- function(root = library_catalog_root()) {
  path <- .library_index_path(root)
  if (!file.exists(path)) return(list(schema_version = LIBRARY_SCHEMA_VERSION, entries = list()))
  value <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(value) || !is.list(value$entries)) stop("Invalid LibeRary index: ", path)
  value
}

.library_read_manifest <- function(library_id, root = library_catalog_root()) {
  path <- file.path(.library_entry_dir(library_id, root), "manifest.json")
  if (!file.exists(path)) stop("Catalogue entry not found: ", library_id, call. = FALSE)
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  issues <- library_validate(manifest = manifest, root = root, check_artifact = FALSE)
  if (length(issues$errors)) stop("Invalid manifest for ", library_id, ": ", paste(issues$errors, collapse = "; "))
  manifest
}

.library_entry_paths <- function(library_id, root = library_catalog_root()) {
  edir <- .library_entry_dir(library_id, root)
  manifest <- .library_read_manifest(library_id, root)
  ctl_rel <- as.character(manifest$model$artifact %||% "model.ctl")[[1L]]
  if (basename(ctl_rel) != ctl_rel) stop("Model artifact must be stored directly inside its entry.")
  list(entry_dir = edir, manifest = file.path(edir, "manifest.json"),
       ctl = file.path(edir, ctl_rel), references = file.path(edir, "references.bib"),
       extraction = file.path(edir, "extraction"),
       reproduction = file.path(edir, "reproduction"))
}

.library_atomic_write <- function(value, path, auto_unbox = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("liberary-", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = auto_unbox, pretty = TRUE,
                       null = "null", digits = NA)
  previous <- paste0(path, ".previous")
  if (file.exists(path)) {
    unlink(previous, force = TRUE)
    if (!file.rename(path, previous)) stop("Unable to rotate ", path)
  }
  if (!file.rename(temporary, path)) {
    if (file.exists(previous)) file.rename(previous, path)
    stop("Unable to publish ", path)
  }
  unlink(previous, force = TRUE)
  if (.Platform$OS.type != "windows") Sys.chmod(path, "0600")
  invisible(path)
}

.library_atomic_write_lines <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("liberary-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(enc2utf8(as.character(value)), temporary, useBytes = TRUE)
  previous <- paste0(path, ".previous")
  if (file.exists(path)) {
    unlink(previous, force = TRUE)
    if (!file.rename(path, previous)) stop("Unable to rotate ", path)
  }
  if (!file.rename(temporary, path)) {
    if (file.exists(previous)) file.rename(previous, path)
    stop("Unable to publish ", path)
  }
  unlink(previous, force = TRUE)
  invisible(path)
}

.library_with_lock <- function(root, code, timeout = 10) {
  lock <- file.path(root, ".catalog.lock")
  deadline <- Sys.time() + timeout
  acquired <- FALSE
  while (!acquired && Sys.time() < deadline) {
    acquired <- dir.create(lock, showWarnings = FALSE)
    if (!acquired) Sys.sleep(0.05)
  }
  if (!acquired) stop("The LibeRary catalogue is busy; try again shortly.")
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  force(code)
}

.library_rebuild_index <- function(root = library_catalog_root()) {
  directories <- list.dirs(file.path(root, "entries"), recursive = FALSE, full.names = TRUE)
  # Transactional staging and backup directories are deliberately hidden and
  # must never become visible catalogue records during an atomic swap.
  directories <- directories[!startsWith(basename(directories), ".")]
  records <- lapply(directories, function(directory) {
    path <- file.path(directory, "manifest.json")
    if (!file.exists(path)) return(NULL)
    value <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(value)) return(NULL)
    list(library_id = value$library_id %||% basename(directory), title = value$title %||% "",
         status = value$status %||% "draft", updated_at = value$updated_at %||% value$created_at %||% "")
  })
  records <- Filter(Negate(is.null), records)
  records <- records[order(vapply(records, `[[`, character(1), "library_id"))]
  .library_atomic_write(list(schema_version = LIBRARY_SCHEMA_VERSION,
                             updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                             entries = records), file.path(root, "index.json"))
}

#' Validate a LibeRary manifest or catalogue entry
#' @param library_id Optional catalogue id.
#' @param manifest Optional parsed manifest.
#' @param root Catalogue root.
#' @param check_artifact Verify that the model artifact exists.
#' @return A list of `valid`, `errors`, and `warnings`.
#' @export
library_validate <- function(library_id = NULL, manifest = NULL,
                             root = library_catalog_root(), check_artifact = TRUE) {
  if (is.null(manifest)) {
    if (is.null(library_id)) stop("Supply `library_id` or `manifest`.")
    path <- file.path(.library_entry_dir(library_id, root), "manifest.json")
    if (!file.exists(path)) return(list(valid = FALSE, errors = "manifest.json is missing", warnings = character()))
    manifest <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = identity)
    if (inherits(manifest, "error")) return(list(valid = FALSE, errors = conditionMessage(manifest), warnings = character()))
  }
  errors <- character(); warnings <- character()
  required <- c("schema_version", "library_id", "version", "status", "title", "model", "study", "provenance")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) errors <- c(errors, paste("Missing fields:", paste(missing, collapse = ", ")))
  id <- tryCatch(.library_valid_id(manifest$library_id), error = identity)
  if (inherits(id, "error")) errors <- c(errors, conditionMessage(id))
  if (!as.character(manifest$status %||% "") %in% LIBRARY_STATUS_LEVELS) errors <- c(errors, "Unknown status")
  confidence <- suppressWarnings(as.numeric(manifest$confidence$overall %||% NA_real_))
  if (is.finite(confidence) && (confidence < 0 || confidence > 1)) errors <- c(errors, "Confidence must be between 0 and 1")
  if (isTRUE(check_artifact) && !inherits(id, "error")) {
    artifact <- basename(as.character(manifest$model$artifact %||% "model.ctl"))
    if (!file.exists(file.path(.library_entry_dir(id, root), artifact))) errors <- c(errors, "Model artifact is missing")
  }
  if (identical(manifest$model$generated_suggestion %||% FALSE, TRUE)) warnings <- c(warnings, "Model code contains generated suggestions requiring review")
  list(valid = !length(errors), errors = unique(errors), warnings = unique(warnings))
}
