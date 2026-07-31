.test_packaged_catalog <- function(...) {
  installed <- system.file("catalog", ..., package = "LibeRary")
  if (nzchar(installed)) return(installed)
  testthat::test_path("..", "..", "inst", "catalog", ...)
}

test_that("packaged AEDapt models are valid research candidates", {
  root <- .test_packaged_catalog()
  ids <- grep(
    "^aedapt_",
    list.dirs(file.path(root, "entries"), recursive = FALSE, full.names = FALSE),
    value = TRUE
  )
  expect_length(ids, 19L)
  checks <- lapply(ids, library_validate, root = root)
  expect_true(all(vapply(checks, function(value) isTRUE(value$valid), logical(1))))

  qualifications <- library_clinical_qualifications(
    status = "candidate", root = root
  )
  aedapt <- Filter(function(value) startsWith(value$library_id, "aedapt_"),
                   qualifications)
  expect_length(aedapt, 19L)
  expect_true(all(vapply(aedapt, function(value) {
    identical(value$status, "candidate") &&
      isFALSE(identical(value$status, "qualified"))
  }, logical(1))))
})

test_that("packaged AEDapt control streams translate in LibeRation", {
  skip_if_not_installed("LibeRation")
  root <- .test_packaged_catalog("entries")
  controls <- Sys.glob(file.path(root, "aedapt_*", "model.ctl"))
  expect_length(controls, 19L)
  translated <- lapply(controls, LibeRation::nm_control_read, strict = TRUE)
  expect_true(all(vapply(translated, function(value) {
    inherits(value$model, "nm_model")
  }, logical(1))))
  expect_equal(
    sort(vapply(translated, function(value) value$model$ADVAN, integer(1))),
    sort(c(rep(2L, 14L), rep(4L, 2L), rep(13L, 3L)))
  )
})

test_that("He lamotrigine translation uses the published weight-normalised volume", {
  root <- .test_packaged_catalog()
  control <- library_model("aedapt_lamotrigine_he", root = root)
  compact <- gsub("[[:space:]]", "", control)
  expect_true(any(grepl(
    "V=THETA(6)*(WT/27.87)", compact, fixed = TRUE
  )))
  expect_false(any(grepl("V=THETA(6)*WT", compact, fixed = TRUE)))
  entry <- library_get("aedapt_lamotrigine_he", root = root)
  expect_identical(entry$manifest$version, "1.0.1")
  expect_match(
    paste(entry$manifest$provenance$corrections, collapse = " "),
    "published V/F = 16.7 * (WT / 27.87)",
    fixed = TRUE
  )
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_control_read(control, strict = TRUE)$model
  data <- data.frame(
    ID = 1L, TIME = c(0, 12), EVID = c(1L, 0L),
    AMT = c(100, 0), RATE = 0, II = c(12, 0),
    SS = c(1L, 0L), CMT = c(1L, 2L),
    DV = c(NA, 5), MDV = c(1L, 0L),
    WT = 27.87, COMED_VPA = 0, COMED_CBZ = 0, COMED_PHB = 0
  )
  fit <- LibeRation::nm_individual_fit(model, data)
  observed <- fit$predictions$MDV == 0L
  expect_lt(abs(fit$eta[[1L]]), 1)
  expect_equal(fit$predictions$IPRED[observed], 5, tolerance = 0.02)
})
