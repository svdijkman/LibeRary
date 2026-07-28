test_that("clinical-use qualifications are scoped, append-only, and queryable", {
  root <- tempfile("liberary-clinical-")
  old <- options(LibeRary.catalog = root)
  on.exit({
    options(old)
    unlink(root, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  catalog <- library_catalog_root()
  expect_true("lib_theo_synthetic" %in% library_list(root = catalog)$library_id)

  qualification <- library_clinical_qualification(
    status = "qualified",
    scope = list(
      drugs = "theophylline",
      indications = "asthma",
      routes = "oral",
      endpoint_kinds = "therapeutic_range",
      population = list(
        AGE = list(min = 18, max = 80, unit = "years", required = TRUE)
      ),
      covariates = list(required = c("AGE", "WT")),
      assays = list(required = TRUE, matrices = "plasma", units = "mg/L")
    ),
    evidence = list(
      external_validation_score = 0.9,
      implementation_verification_score = 1
    ),
    governance = list(
      issuer = "Example university hospital",
      reviewer = "Clinical pharmacology committee",
      review_due = "2099-12-31T00:00:00Z"
    ),
    limitations = "Not qualified during pregnancy"
  )
  expect_s3_class(qualification, "library_clinical_qualification")
  manifest <- library_clinical_qualify(
    "lib_theo_synthetic", qualification, root = catalog
  )
  expect_length(manifest$qualification$clinical_use, 1L)

  records <- library_clinical_qualifications(
    "lib_theo_synthetic", status = "qualified", root = catalog
  )
  expect_length(records, 1L)
  expect_equal(records[[1L]]$model_version, "1.0.1")
  expect_true(library_list(root = catalog)$clinically_qualified[
    library_list(root = catalog)$library_id == "lib_theo_synthetic"
  ])

  suspension <- library_clinical_qualification(
    status = "suspended",
    scope = qualification$scope,
    governance = list(
      issuer = "Example university hospital",
      reviewer = "Clinical pharmacology committee"
    ),
    supersedes = qualification$qualification_id,
    notes = "Temporarily suspended pending review"
  )
  library_clinical_qualify("lib_theo_synthetic", suspension, root = catalog)
  current <- library_clinical_qualifications(
    "lib_theo_synthetic", current = TRUE, root = catalog
  )
  expect_length(current, 1L)
  expect_equal(current[[1L]]$status, "suspended")
  expect_false(library_list(root = catalog)$clinically_qualified[
    library_list(root = catalog)$library_id == "lib_theo_synthetic"
  ])
})

test_that("qualified records require governance and an explicit endpoint scope", {
  expect_error(
    library_clinical_qualification(
      status = "qualified",
      scope = list(drugs = "Drug A"),
      governance = list(issuer = "Hospital", reviewer = "Reviewer")
    ),
    "endpoint"
  )
  expect_error(
    library_clinical_qualification(
      status = "qualified",
      scope = list(drugs = "Drug A", endpoint_kinds = "therapeutic_range")
    ),
    "issuer and reviewer"
  )
})
