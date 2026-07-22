test_that("publication is validated, versioned, and explicit about generated defaults", {
  root <- tempfile("liberary-catalog-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  cfg <- ingest_load_config()
  cfg$data_dir <- root
  cfg$inbox_dir <- file.path(root, "inbox")
  cfg$cache_dir <- file.path(root, "cache")
  cfg$catalog_dir <- file.path(root, "catalog")
  source_pdf <- file.path(root, "source.pdf")
  writeBin(charToRaw("%PDF-1.4\nfixture"), source_pdf)
  bundle_path <- file.path(root, "bundle.json")
  bundle <- list(
    source = list(path = source_pdf, sha256 = "fixture-sha256"),
    provenance = list(acquisition_path = source_pdf),
    parser = list(name = "fixture"), manifest_path = bundle_path
  )
  jsonlite::write_json(bundle, bundle_path, auto_unbox = TRUE)
  metadata <- list(pmid = "12345", title = "Test oral population PK", abstract = "Modelled with NONMEM.",
                   doi = "10.1/test", journal = "Test", year = "2026", authors = "A Researcher")
  extraction <- list(
    title = metadata$title, compound = "Testdrug", population = "Adults", route = "oral",
    n_subjects = 20, software = "NONMEM", estimation_method = "FOCEI", model_type = "pk",
    structural_model = list(
      advan = 2, trans = 2, compartments = 1, description = "One compartment",
      implementations = list(list(
        engine = "ADVAN", advan = 2, trans = 2, status = "reported",
        confidence = 1, rationale = "Reported in the publication.",
        evidence = list(status = "reported", source_locator = "fixture",
                        evidence = "ADVAN2 TRANS2", confidence = 1),
        alternatives = list(), review_required = FALSE
      ))
    ),
    parameters = list(
      theta = list(list(name = "KA", typical = 1, se = NULL, unit = "1/h"),
                   list(name = "CL", typical = 5, se = NULL, unit = "L/h"),
                   list(name = "V", typical = 50, se = NULL, unit = "L")),
      omega = list(list(description = "IIV KA", value = 0.1,
                        reported_metric = "variance", eta_distribution = "log_normal"),
                   list(description = "IIV CL", value = 0.1,
                        reported_metric = "variance", eta_distribution = "log_normal"),
                   list(description = "IIV V", value = 0.1,
                        reported_metric = "variance", eta_distribution = "log_normal")),
      sigma = list(list(description = "proportional", value = 0.05,
                        reported_metric = "variance"))),
    covariates = character(), residual_error = "Y = F * (1 + ERR(1))",
    confidence = list(overall = 0.8, fields = list(structure = 0.9, parameters = 0.8,
                                                   population = 0.8, software = 1)),
    evidence_quotes = "Modelled with NONMEM.", notes = "Test fixture"
  )
  first_audit <- list(provider = "fixture", model = "index-v1", content_hash = "first")
  first <- ingest_publish_catalog_entry(metadata, extraction, cfg, status = "draft",
                                        raw_llm = first_audit, document_bundle = bundle)
  expect_equal(first$version, "1.0.0")
  expect_true(library_validate(first$library_id, root = cfg$catalog_dir)$valid)
  expect_true(any(grepl("^\\$SUBROUTINES ADVAN2 TRANS2", library_model(first$library_id, cfg$catalog_dir))))
  expect_equal(library_source_pdf(first$library_id, cfg$catalog_dir),
               normalizePath(source_pdf, winslash = "/", mustWork = TRUE))
  if (requireNamespace("LibeRation", quietly = TRUE)) {
    expect_true(LibeRation::nm_control_read(library_model(first$library_id, cfg$catalog_dir), strict = TRUE)$compatibility$translated)
  }
  second_audit <- list(provider = "fixture", model = "index-v2", content_hash = "second")
  second <- ingest_publish_catalog_entry(metadata, extraction, cfg, status = "review",
                                         raw_llm = second_audit, document_bundle = bundle)
  expect_equal(second$version, "1.0.1")
  expect_true(file.exists(file.path(second$entry_dir, "versions", "1.0.0", "manifest.json")))
  archived_audit <- jsonlite::fromJSON(file.path(
    second$entry_dir, "versions", "1.0.0", "extraction", "raw_llm.json"
  ))
  current_audit <- jsonlite::fromJSON(file.path(second$entry_dir, "extraction", "raw_llm.json"))
  expect_equal(archived_audit$content_hash, "first")
  expect_equal(current_audit$content_hash, "second")
  expect_equal(sum(library_list(root = cfg$catalog_dir)$library_id == second$library_id), 1L)
  expect_error(library_review(second$library_id, "validated", "Reviewer", root = cfg$catalog_dir),
               "confirm_generated")
  gate <- library_qualification_check(second$library_id, root = cfg$catalog_dir)
  expect_true(gate$ready)
  expect_true(gate$compile$passed)
  expect_true(gate$simulation$passed)
  reviewed <- library_review(second$library_id, "validated", "Reviewer", "Checked",
                             confirm_generated = TRUE, root = cfg$catalog_dir)
  expect_equal(reviewed$status, "validated")
  if (requireNamespace("LibeRation", quietly = TRUE)) {
    imported <- library_use_in_workspace(
      second$library_id, project = "Human project name", workspace = tempfile("workspace-"),
      root = cfg$catalog_dir
    )
    expect_equal(imported$project, "human-project-name")
    expect_equal(imported$provenance$library_id, second$library_id)
  }
})

test_that("machine publication cannot bypass catalogue quarantine", {
  root <- tempfile("liberary-quarantine-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  cfg <- ingest_load_config()
  cfg$data_dir <- root
  cfg$inbox_dir <- file.path(root, "inbox")
  cfg$cache_dir <- file.path(root, "cache")
  cfg$catalog_dir <- file.path(root, "catalog")
  metadata <- list(pmid = "112233", title = "Quarantine fixture",
                   abstract = "A NONMEM model", doi = "", journal = "",
                   year = "2026", authors = character())
  extraction <- ingest_stub_extraction(metadata)
  expect_error(
    ingest_publish_catalog_entry(metadata, extraction, cfg, status = "validated"),
    "cannot create"
  )
})

test_that("remote LLM content is blocked unless explicitly enabled", {
  cfg <- ingest_load_config()
  cfg$llm$indexing$provider <- "openai"
  cfg$llm$indexing$model <- "test-model"
  cfg$llm$allow_remote_content <- FALSE
  expect_error(library_llm_chat(list(list(role = "user", content = "test")), cfg, "indexing"),
               "Remote LLM content transfer is disabled")
})

test_that("catalogue publication restores the prior entry when index activation fails", {
  root <- tempfile("liberary-transaction-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  cfg <- ingest_load_config()
  cfg$data_dir <- root
  cfg$inbox_dir <- file.path(root, "inbox")
  cfg$cache_dir <- file.path(root, "cache")
  cfg$catalog_dir <- file.path(root, "catalog")
  cfg$reproduction$enabled <- FALSE
  metadata <- list(pmid = "998877", title = "Transactional fixture", abstract = "NONMEM model",
                   doi = "", journal = "", year = "2026", authors = character())
  extraction <- ingest_stub_extraction(metadata)
  first <- ingest_publish_catalog_entry(metadata, extraction, cfg, status = "stub",
                                        raw_llm = list(marker = "original"))
  manifest_before <- readBin(first$manifest_path, "raw", n = file.info(first$manifest_path)$size)
  audit_path <- file.path(first$entry_dir, "extraction", "raw_llm.json")
  audit_before <- readBin(audit_path, "raw", n = file.info(audit_path)$size)

  testthat::local_mocked_bindings(
    .library_rebuild_index = function(...) stop("forced index failure"),
    .package = "LibeRary"
  )
  expect_error(
    ingest_publish_catalog_entry(metadata, extraction, cfg, status = "stub",
                                 raw_llm = list(marker = "replacement")),
    "prior entry was restored"
  )
  expect_identical(readBin(first$manifest_path, "raw", n = file.info(first$manifest_path)$size),
                   manifest_before)
  expect_identical(readBin(audit_path, "raw", n = file.info(audit_path)$size), audit_before)
  expect_equal(jsonlite::read_json(first$manifest_path)$version, "1.0.0")
})
