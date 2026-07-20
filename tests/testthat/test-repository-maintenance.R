test_that("repository wipe requires exact typed confirmation", {
  root <- tempfile("liberary-wipe-confirm-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(library_repository_wipe("yes", root), 'Type "YES" exactly')
  expect_error(library_repository_wipe(" YES ", root), 'Type "YES" exactly')
})

test_that("repository wipe removes managed data but retains settings", {
  root <- tempfile("liberary-wipe-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines("email: retained@example.org", file.path(root, "config.yml"))
  writeLines("unrelated", file.path(root, "user-note.txt"))
  managed <- c("inbox", "cache", "catalog", "manifests", "logs", "documents", "triage")
  for (directory in managed) {
    path <- file.path(root, directory, "nested")
    dir.create(path, recursive = TRUE)
    writeLines(directory, file.path(path, "artifact.txt"))
  }

  result <- library_repository_wipe("YES", root)

  expect_gt(result$removed, 0L)
  expect_true(file.exists(file.path(root, "config.yml")))
  expect_true(file.exists(file.path(root, "user-note.txt")))
  expect_true(all(dir.exists(file.path(root, managed))))
  expect_true(file.exists(file.path(root, "catalog", ".skip-packaged-seed")))
  expect_true(file.exists(file.path(root, "catalog", "index.json")))
  expect_false(any(file.exists(file.path(root, managed, "nested", "artifact.txt"))))

  LibeRary:::.library_initialize_catalog(file.path(root, "catalog"))
  expect_equal(nrow(library_list(root = file.path(root, "catalog"))), 0L)
})

test_that("ingest GUI exposes a guarded repository wipe dialog", {
  source <- paste(readLines(
    system.file("shiny-ingest", "app.R", package = "LibeRary"),
    warn = FALSE
  ), collapse = "\n")
  expect_match(source, 'Wipe repository...', fixed = TRUE)
  expect_match(source, 'Type "YES" to confirm', fixed = TRUE)
  expect_match(source, 'disabled = !confirmed', fixed = TRUE)
  expect_match(source, 'job_is_running()', fixed = TRUE)
  expect_match(source, 'library_repository_wipe(confirmation, cfg$data_dir)', fixed = TRUE)
})
