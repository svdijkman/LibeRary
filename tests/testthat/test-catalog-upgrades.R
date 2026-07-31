test_that("newer packaged revisions safely upgrade persistent catalogue entries", {
  packaged <- tempfile("liberary-packaged-")
  root <- tempfile("liberary-persistent-")
  dir.create(file.path(packaged, "entries"), recursive = TRUE)
  dir.create(file.path(root, "entries"), recursive = TRUE)
  on.exit(unlink(c(packaged, root), recursive = TRUE, force = TRUE), add = TRUE)

  id <- "aedapt_lamotrigine_he"
  source <- system.file("catalog", "entries", id, package = "LibeRary")
  if (!nzchar(source)) {
    source <- testthat::test_path(
      "..", "..", "inst", "catalog", "entries", id
    )
  }
  packaged_entry <- file.path(packaged, "entries", id)
  persistent_entry <- file.path(root, "entries", id)
  .library_copy_tree(source, packaged_entry)
  .library_copy_tree(source, persistent_entry)

  manifest_path <- file.path(persistent_entry, "manifest.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$version <- "1.0.0"
  local_record <- manifest$qualification$clinical_use[[1L]]
  local_record$qualification_id <- "cq-local-prior-review"
  local_record$model$version <- "1.0.0"
  manifest$qualification$clinical_use <- c(
    manifest$qualification$clinical_use, list(local_record)
  )
  jsonlite::write_json(
    manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  control_path <- file.path(persistent_entry, "model.ctl")
  control <- readLines(control_path, warn = FALSE)
  control <- gsub("(WT/27.87)", "WT", control, fixed = TRUE)
  writeLines(control, control_path, useBytes = TRUE)
  writeLines("retain me", file.path(persistent_entry, "local-note.txt"))

  testthat::local_mocked_bindings(
    .library_packaged_root = function() packaged,
    .package = "LibeRary"
  )
  .library_initialize_catalog(root)

  active <- library_get(id, root = root)
  expect_identical(active$manifest$version, "1.0.1")
  expect_true(any(grepl(
    "V=THETA(6)*(WT/27.87)",
    gsub("[[:space:]]", "", library_model(id, root = root)),
    fixed = TRUE
  )))
  expect_true(file.exists(file.path(persistent_entry, "local-note.txt")))
  archived <- readLines(
    file.path(persistent_entry, "versions", "1.0.0", "model.ctl"),
    warn = FALSE
  )
  expect_true(any(grepl(
    "V=THETA(6)*WT", gsub("[[:space:]]", "", archived), fixed = TRUE
  )))
  expect_true(any(vapply(
    active$manifest$qualification$clinical_use,
    function(record) identical(
      record$qualification_id, "cq-local-prior-review"
    ),
    logical(1)
  )))
  current <- library_clinical_qualifications(
    library_id = id, root = root
  )
  expect_false(any(vapply(
    current,
    function(record) identical(
      record$qualification_id, "cq-local-prior-review"
    ),
    logical(1)
  )))

  .library_initialize_catalog(root)
  expect_identical(library_get(id, root = root)$manifest$version, "1.0.1")
})
