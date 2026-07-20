schema_fixture_value <- function(schema) {
  type <- as.character(schema$type %||% "object")
  type <- type[[1L]]
  if (!is.null(schema$enum)) return(as.character(schema$enum[[1L]]))
  switch(type,
    object = {
      required <- as.character(unlist(schema$required %||% character()))
      stats::setNames(lapply(required, function(name) {
        schema_fixture_value(schema$properties[[name]])
      }), required)
    },
    array = list(),
    string = "",
    number = max(as.numeric(schema$minimum %||% 0), 0.5),
    integer = as.integer(max(as.numeric(schema$minimum %||% 0), 1)),
    boolean = TRUE,
    null = NULL,
    stop("Unsupported fixture schema type: ", type)
  )
}

test_that("deliberative retrieval prioritizes topic evidence", {
  text <- paste(
    strrep("Background without model detail. ", 220),
    "## Interindividual variability\nOMEGA block ETA clearance variance covariance final model.",
    strrep("Discussion without model detail. ", 220),
    sep = "\n"
  )
  chunks <- LibeRary:::.library_document_chunks(text, chunk_chars = 1500L, overlap = 100L)
  selected <- LibeRary:::.library_retrieve_chunks(
    chunks, c("omega", "eta", "covariance"), limit = 2L
  )
  expect_true(any(grepl("OMEGA block", vapply(selected, `[[`, character(1), "text"))))
  expect_true(all(nzchar(vapply(selected, `[[`, character(1), "locator"))))
})

test_that("article chunks retain UTF-8 through Windows substring operations", {
  text <- paste(
    strrep("Background. ", 500),
    "| Source | Röshammar et al., 2017 |",
    "| Model | population pharmacokinetic final model |",
    sep = "\n"
  )
  Encoding(text) <- "UTF-8"
  chunks <- LibeRary:::.library_document_chunks(
    text, chunk_chars = 1500L, overlap = 100L
  )
  expect_silent(selected <- LibeRary:::.library_retrieve_chunks(
    chunks, c("model", "Röshammar"), limit = 3L
  ))
  expect_true(any(grepl(
    "Röshammar", vapply(selected, `[[`, character(1), "text"), fixed = TRUE
  )))
  expect_true(all(vapply(chunks, function(chunk) {
    !is.na(iconv(chunk$text, from = "UTF-8", to = "UTF-8", sub = NA))
  }, logical(1))))
})

test_that("plain-text readers normalize UTF-8 and legacy Windows text", {
  utf8_path <- tempfile(fileext = ".md")
  writeBin(charToRaw(enc2utf8("Röshammar – final model")), utf8_path)
  expect_identical(
    LibeRary:::.library_read_text_utf8(utf8_path),
    enc2utf8("Röshammar – final model")
  )

  legacy_path <- tempfile(fileext = ".txt")
  legacy <- iconv("Röshammar", from = "UTF-8", to = "windows-1252", toRaw = TRUE)[[1L]]
  writeBin(legacy, legacy_path)
  expect_identical(
    LibeRary:::.library_read_text_utf8(legacy_path), enc2utf8("Röshammar")
  )
})

test_that("investigation stages are content-addressed and resumable", {
  root <- tempfile("liberary-stage-cache-")
  dir.create(root)
  bundle <- list(bundle_path = root, source = list(sha256 = "fixture-source"))
  cfg <- ingest_load_config()
  cfg$llm$indexing$provider <- "ollama"
  cfg$llm$indexing$model <- "fixture"
  calls <- 0L
  schema <- list(
    type = "object", additionalProperties = FALSE,
    properties = list(answer = list(type = "string")), required = "answer"
  )
  fake_chat <- function(messages, cfg, role, format, sensitive) {
    calls <<- calls + 1L
    list(provider = "fixture", model = "fixture", content = '{"answer":"supported"}',
         done_reason = "stop", usage = list(input_tokens = 4L, output_tokens = 3L))
  }
  first <- LibeRary:::.library_investigation_stage(
    "fixture", list(list(role = "user", content = "question")), schema,
    cfg, "indexing", bundle, cache = TRUE, chat = fake_chat
  )
  second <- LibeRary:::.library_investigation_stage(
    "fixture", list(list(role = "user", content = "question")), schema,
    cfg, "indexing", bundle, cache = TRUE, chat = fake_chat
  )
  expect_equal(calls, 1L)
  expect_false(first$cached)
  expect_true(second$cached)
  expect_equal(second$value$answer, "supported")
  expect_true(file.exists(second$path))
})

test_that("consistency gates reject unresolved or unsupported model records", {
  ledger <- list(
    claims = list(
      list(id = "c1", field = "structural.compartments", value = "1",
           domain = "structure", status = "reported", model_stage = "final", source_locator = "Table 2",
           evidence = "one compartment"),
      list(id = "c2", field = "structural.compartments", value = "2",
           domain = "structure", status = "reported", model_stage = "final", source_locator = "Results",
           evidence = "two compartments"),
      list(id = "c3", field = "theta.CL", value = "5", status = "reported",
           domain = "theta", model_stage = "final", source_locator = "", evidence = "")
    ),
    model_inventory = list(list(role = "final")), stage_summaries = list()
  )
  checks <- LibeRary:::.library_ledger_consistency_checks(ledger)
  expect_false(checks$ready)
  expect_gt(length(checks$conflicting_claims), 0L)
  expect_gt(length(checks$reported_claims_without_evidence), 0L)
})

test_that("claim ids are namespaced by specialist stage", {
  claim <- list(
    id = "c1", domain = "theta", field = "parameters.theta.CL", value = "5",
    unit = "L/h", status = "reported", model_stage = "final",
    source_locator = "Table 2", evidence = "CL 5 L/h", confidence = 0.9,
    dependencies = list(), alternatives = list()
  )
  ledger <- list(claims = list())
  ledger <- LibeRary:::.library_ledger_add_claims(ledger, list(claim), "theta")
  claim$domain <- "omega"
  claim$field <- "parameters.omega.CL"
  claim$value <- "0.1"
  ledger <- LibeRary:::.library_ledger_add_claims(ledger, list(claim), "omega")
  expect_equal(length(ledger$claims), 2L)
  expect_equal(vapply(ledger$claims, `[[`, character(1), "id"),
               c("theta::c1", "omega::c1"))
})

test_that("deliberative extraction completes staged investigation before synthesis", {
  root <- tempfile("liberary-deliberative-")
  dir.create(root)
  markdown <- file.path(root, "article.md")
  writeLines(c(
    "# Methods", "The final model was a one-compartment oral model.",
    "Table 2 reports clearance, volume, interindividual variability and residual error.",
    "# Results", "Patients received 100 mg every 12 hours."
  ), markdown)
  bundle <- list(
    bundle_path = root, source = list(sha256 = "fixture-source"),
    parser = list(markdown_path = markdown, name = "fixture"),
    vision = list(image_paths = character(), page_numbers = integer())
  )
  metadata <- list(pmid = "9002", title = "Fixture model", abstract = "", doi = "",
                   journal = "Journal", year = "2026", pmcid = "", authors = "Researcher")
  cfg <- ingest_load_config()
  cfg$data_dir <- root
  cfg$deliberative$enabled <- TRUE
  cfg$deliberative$cache_stages <- FALSE
  cfg$deliberative$visual_verification <- FALSE
  cfg$deliberative$max_gap_rounds <- 0L
  stages <- character()
  fake_chat <- function(messages, cfg, role, format, sensitive) {
    value <- schema_fixture_value(format)
    required <- as.character(unlist(format$required %||% character()))
    if ("model_inventory" %in% required) {
      value$model_inventory <- list(list(
        id = "m1", label = "final PK model", role = "final", compound = "Testdrug",
        endpoint = "concentration", source_locator = "Methods", evidence = "final model",
        confidence = 0.9
      ))
      value$model_present <- TRUE
      value$model_probability <- 0.95
    }
    if ("topic" %in% required) value$coverage <- 0.9
    if ("overall_ready" %in% required) value$overall_ready <- FALSE
    if ("extraction" %in% required) {
      value$model_present <- TRUE
      value$model_probability <- 0.9
    }
    list(provider = "fixture", model = paste0("fixture-", role),
         content = jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA),
         done_reason = "stop", usage = list(input_tokens = 10L, output_tokens = 10L),
         runtime = list(processor = "fixture"))
  }
  result <- ingest_deliberative_extract(
    metadata, bundle, cfg,
    progress = function(value, message, stage) stages <<- c(stages, stage),
    chat = fake_chat
  )
  expect_true(result$available)
  expect_true(isTRUE(result$result$model_present))
  expect_true(all(c(
    "document_map", "reconnaissance", "investigate_structure",
    "falsification_1", "consistency_checks", "synthesis", "deliberative_complete"
  ) %in% stages))
  expect_equal(result$audit$pipeline, "evidence_led_deliberative")
  expect_true(file.exists(result$audit$evidence_ledger_path))
  expect_gte(length(result$audit$stages), 9L)
})

test_that("catalogue versions preserve their evidence ledger", {
  root <- tempfile("liberary-ledger-catalog-")
  dir.create(root)
  ledger_path <- file.path(root, "source-ledger.json")
  jsonlite::write_json(
    list(claims = list(list(id = "structure::c1")), questions = list(),
         deterministic_checks = list(ready = TRUE, coverage_fraction = 1)),
    ledger_path, auto_unbox = TRUE
  )
  cfg <- ingest_load_config()
  cfg$data_dir <- root
  cfg$inbox_dir <- file.path(root, "inbox")
  cfg$cache_dir <- file.path(root, "cache")
  cfg$catalog_dir <- file.path(root, "catalog")
  cfg$reproduction$enabled <- FALSE
  metadata <- list(pmid = "9003", title = "Ledger fixture", abstract = "NONMEM model",
                   doi = "", journal = "", year = "2026", authors = character())
  published <- ingest_publish_catalog_entry(
    metadata, ingest_stub_extraction(metadata), cfg, status = "stub",
    raw_llm = list(text = list(
      pipeline = "evidence_led_deliberative", pipeline_version = "3.1.0",
      evidence_ledger_path = ledger_path
    ))
  )
  copied <- file.path(published$entry_dir, "extraction", "evidence-ledger.json")
  expect_true(file.exists(copied))
  provenance <- library_provenance(published$library_id, cfg$catalog_dir)
  expect_equal(provenance$evidence_ledger$claims[[1L]]$id, "structure::c1")
  expect_equal(provenance$provenance$evidence_ledger, "extraction/evidence-ledger.json")
})
