.library_repository_directories <- function(data_dir) {
  file.path(data_dir, c(
    "inbox", "cache", "catalog", "manifests", "logs", "documents", "triage"
  ))
}

.library_repository_root <- function(data_dir) {
  data_dir <- as.character(data_dir %||% "")
  if (length(data_dir) != 1L || is.na(data_dir) || !nzchar(data_dir)) {
    stop("The LibeRary data directory must be one non-empty path.", call. = FALSE)
  }
  data_dir <- path.expand(data_dir)
  if (file.exists(data_dir) && !dir.exists(data_dir)) {
    stop("The LibeRary data path is not a directory: ", data_dir, call. = FALSE)
  }
  root <- normalizePath(data_dir, winslash = "/", mustWork = FALSE)
  home <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  protected <- unique(c(
    home,
    normalizePath(file.path(home, "Documents"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(home, "LibeR"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(home, "Documents", "LibeR"), winslash = "/", mustWork = FALSE)
  ))
  if (identical(dirname(root), root) || tolower(root) %in% tolower(protected)) {
    stop("Refusing to wipe an unsafe repository root: ", root, call. = FALSE)
  }
  root
}

#' Permanently empty the mutable LibeRary repository
#'
#' Removes all LibeRary-managed catalogue entries, acquired publications,
#' parsed documents, manifests, triage records, caches, and job logs beneath
#' `data_dir`. The upgrade-safe configuration YAML and any unrelated files in
#' the data-directory root are retained. Package-bundled example entries are
#' not copied back into a repository explicitly emptied by this function.
#'
#' @param confirmation Must be exactly `"YES"` (case-sensitive).
#' @param data_dir Persistent LibeRary data directory.
#' @return A summary containing the normalized root and number of removed
#'   filesystem entries, invisibly.
#' @export
library_repository_wipe <- function(confirmation, data_dir = library_home()) {
  if (!identical(as.character(confirmation %||% ""), "YES")) {
    stop('Type "YES" exactly to wipe the LibeRary repository.', call. = FALSE)
  }
  root <- .library_repository_root(data_dir)
  if (!dir.exists(root) && !dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
    stop("Unable to create the LibeRary data directory: ", root, call. = FALSE)
  }

  targets <- .library_repository_directories(root)
  root_prefix <- paste0(tolower(root), "/")
  removed <- 0L
  for (target in targets) {
    resolved <- normalizePath(target, winslash = "/", mustWork = FALSE)
    if (!startsWith(tolower(resolved), root_prefix)) {
      stop("A repository path escaped the configured data directory.", call. = FALSE)
    }
    if (!file.exists(target) && !dir.exists(target)) next
    removed <- removed + length(list.files(
      target, all.files = TRUE, recursive = TRUE, full.names = TRUE,
      include.dirs = TRUE, no.. = TRUE
    )) + 1L
    status <- unlink(target, recursive = TRUE, force = TRUE)
    if (!identical(status, 0L) || file.exists(target) || dir.exists(target)) {
      stop("Unable to completely remove repository path: ", target, call. = FALSE)
    }
  }

  for (directory in targets[basename(targets) != "catalog"]) {
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
      stop("Unable to recreate repository directory: ", directory, call. = FALSE)
    }
  }
  catalog <- file.path(root, "catalog")
  entries <- file.path(catalog, "entries")
  if (!dir.create(entries, recursive = TRUE, showWarnings = FALSE) && !dir.exists(entries)) {
    stop("Unable to recreate the empty LibeRary catalogue.", call. = FALSE)
  }
  marker <- file.path(catalog, ".skip-packaged-seed")
  if (!file.create(marker)) stop("Unable to mark the repository as explicitly emptied.", call. = FALSE)
  jsonlite::write_json(
    list(schema_version = LIBRARY_SCHEMA_VERSION, entries = list()),
    file.path(catalog, "index.json"), auto_unbox = TRUE, pretty = TRUE
  )
  invisible(list(root = root, removed = removed))
}
