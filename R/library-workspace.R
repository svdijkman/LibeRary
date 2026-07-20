#' Import a catalogue model into a LibeRation workspace
#'
#' The control stream is parsed and compiled by LibeRation before it is stored.
#' Catalogue identity, version, evidence provenance, and qualification state are
#' retained in the immutable model-version provenance record.
#' @param library_id Catalogue entry id.
#' @param project Existing project id or a name for a new project. By default a
#'   stable project id is derived from `library_id`.
#' @param workspace An `nm_workspace`, path, or `NULL` for LibeRation's default.
#' @param version_label Optional model-version label.
#' @param root Catalogue root.
#' @return Project id, model-version id, workspace, and compatibility report.
#' @export
library_use_in_workspace <- function(library_id, project = NULL, workspace = NULL,
                                     version_label = NULL, root = library_catalog_root()) {
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    stop("Install LibeRation >= 0.6.0 to import catalogue models.", call. = FALSE)
  }
  entry <- library_get(library_id, root)
  checks <- library_validate(library_id, root = root)
  if (!checks$valid) stop(paste(checks$errors, collapse = "; "))
  control <- LibeRation::nm_control_read(library_model(library_id, root), strict = TRUE)
  ws <- if (is.null(workspace)) LibeRation::nm_workspace() else
    if (inherits(workspace, "nm_workspace")) workspace else LibeRation::nm_workspace(workspace)
  projects <- LibeRation::nm_project_list(ws)
  default_id <- if (startsWith(library_id, "lib_")) library_id else paste0("lib_", library_id)
  requested <- trimws(as.character(project %||% default_id)[[1L]])
  if (!nzchar(requested)) requested <- default_id
  if (requested %in% projects$id) {
    project_id <- requested
  } else {
    valid_id <- tryCatch(.library_valid_id(requested), error = function(e) NULL)
    created <- LibeRation::nm_project_create(
      ws,
      name = if (is.null(project)) entry$manifest$title %||% requested else requested,
      id = valid_id,
      description = paste("Imported from LibeRary", library_id)
    )
    project_id <- created$id
  }
  provenance <- list(
    source = "LibeRary", library_id = library_id,
    library_version = entry$manifest$version %||% "",
    schema_version = entry$manifest$schema_version %||% "",
    status_at_import = entry$manifest$status %||% "",
    imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    evidence = entry$manifest$provenance %||% list(),
    qualification = entry$manifest$qualification %||% list()
  )
  attr(control$model, "library_provenance") <- provenance
  save_args <- list(
    workspace = ws, project = project_id, model = control$model, data = NULL,
    label = version_label %||% entry$manifest$title
  )
  # Early 0.6.0 builds did not yet expose the dedicated provenance argument.
  # The model attribute above preserves the same information when importing
  # into one of those installations.
  if ("provenance" %in% names(formals(LibeRation::nm_project_save))) {
    save_args$provenance <- list(LibeRary = provenance)
  }
  id <- do.call(LibeRation::nm_project_save, save_args)
  list(project = project_id, version_id = id, workspace = ws,
       compatibility = control$compatibility, provenance = provenance)
}
