args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args) >= 1L) args[[1L]] else "AED_PKPD"
output_dir <- if (length(args) >= 2L) args[[2L]] else
  file.path("validation", "liberary", "aed-pkpd-reference", "0.1.2")
version <- if (length(args) >= 3L) args[[3L]] else basename(output_dir)

if (dir.exists("LibeRary") && file.exists(file.path("LibeRary", "DESCRIPTION"))) {
  package_root <- normalizePath("LibeRary", winslash = "/", mustWork = TRUE)
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Install devtools before running this source-tree helper.")
  }
  devtools::load_all(package_root, quiet = TRUE)
} else if (!requireNamespace("LibeRary", quietly = TRUE)) {
  stop("Install LibeRary or run this helper from the LibeR source-tree root.")
}

LibeRary::library_reference_build(source_dir, output_dir, version = version)
print(LibeRary::library_reference_validate(output_dir, check_hashes = TRUE,
                                           source_dir = source_dir))
