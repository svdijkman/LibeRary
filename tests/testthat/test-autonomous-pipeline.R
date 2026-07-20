pipeline_metadata <- function(abstract = "Population pharmacokinetic model with NONMEM and FOCEI.") {
  list(pmid = "9001", title = "Population PK analysis", abstract = abstract,
       doi = "10.1000/test", journal = "Journal", year = "2026", pmcid = "",
       authors = "Researcher")
}

pipeline_extraction <- function(theta = 5) {
  value <- ingest_stub_extraction(pipeline_metadata())
  value$compound <- "Testdrug"
  value$route <- "oral"
  value$software <- "NONMEM"
  value$model_type <- "pk"
  value$structural_model <- list(advan = 2, trans = 2, compartments = 1,
                                 description = "One-compartment oral model")
  value$parameters$theta <- list(list(name = "CL", typical = theta, se = NULL, unit = "L/h"))
  value$confidence$overall <- 0.9
  value
}

test_that("abstract triage retains low probability articles for later", {
  cfg <- ingest_load_config()
  cfg$llm$triage$provider <- "none"
  high <- ingest_triage_abstract(pipeline_metadata(), cfg)
  low_metadata <- pipeline_metadata("A randomized clinical report without modelling.")
  low_metadata$title <- "Randomized clinical study"
  low <- ingest_triage_abstract(low_metadata, cfg)
  expect_true(high$tier %in% c("high", "intermediate"))
  expect_equal(low$tier, "low")
  expect_equal(low$action, "defer_low")

  local_mocked_bindings(
    ingest_unpaywall_lookup = function(...) NULL,
    ingest_europe_pmc_lookup = function(...) list(pmcid = "", is_open_access = FALSE, pdf_url = ""),
    ingest_pmc_oa_lookup = function(...) list(is_oa = FALSE, pdf_url = "")
  )
  row <- ingest_classify_entry(low_metadata, cfg, low)
  expect_equal(row$acquisition_class, "deferred_low")
  expect_equal(row$status, "deferred")
  expect_true(nzchar(row$suggested_url))
})

test_that("triage model categories cannot expand a manifest row", {
  cfg <- ingest_load_config()
  triage <- list(relevant_probability = 0.9, recoverable_probability = 0.8,
                 tier = "high", action = "first_pass", method = "llm",
                 provider = "ollama", model = "qwen-test",
                 model_categories = c("pk", "pd", "pkpd"), evidence = character(),
                 uncertainty = character())
  local_mocked_bindings(
    ingest_unpaywall_lookup = function(...) NULL,
    ingest_europe_pmc_lookup = function(...) list(pmcid = "", is_open_access = FALSE, pdf_url = ""),
    ingest_pmc_oa_lookup = function(...) list(is_oa = FALSE, pdf_url = "")
  )
  row <- ingest_classify_entry(pipeline_metadata(), cfg, triage)
  expect_length(row$triage_model, 1L)
  expect_equal(row$triage_model, "qwen-test")
  expect_equal(nrow(LibeRary:::ingest_entries_to_df(list(row))), 1L)
})

test_that("Docling adapter uses its configured standard-pipeline runner", {
  pdf <- tempfile(fileext = ".pdf"); writeBin(charToRaw("%PDF-1.4\nfixture"), pdf)
  output <- tempfile("docling-")
  old <- options(LibeRary.docling_runner = function(executable, args, stdout, stderr) {
    destination <- args[[which(args == "--output") + 1L]]
    writeLines("# Parsed model", file.path(destination, "fixture.md"))
    jsonlite::write_json(list(text = "Parsed model"), file.path(destination, "fixture.json"), auto_unbox = TRUE)
    writeLines("fixture docling", stdout)
    0L
  })
  on.exit(options(old), add = TRUE)
  cfg <- ingest_load_config()
  cfg$docling$pipeline <- "standard"
  parsed <- ingest_docling_parse(pdf, output, cfg)
  expect_true(parsed$success)
  expect_match(parsed$parser, "docling_standard")
  expect_true(file.exists(parsed$markdown_path))
  expect_true(file.exists(parsed$json_path))
})

test_that("document bundles are content-addressed and resumable", {
  root <- tempfile("library-documents-")
  pdf <- tempfile(fileext = ".pdf"); writeBin(charToRaw("%PDF-1.4\nfixture"), pdf)
  cfg <- ingest_load_config(); cfg$data_dir <- root; cfg$inbox_dir <- file.path(root, "inbox")
  cfg$cache_dir <- file.path(root, "cache"); cfg$catalog_dir <- file.path(root, "catalog")
  local_mocked_bindings(
    ingest_docling_parse = function(pdf_path, output_dir, cfg, force = FALSE) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      md <- file.path(output_dir, "article.md"); writeLines("# Model", md)
      js <- file.path(output_dir, "article.json"); jsonlite::write_json(list(text = "Model"), js)
      list(success = TRUE, parser = "docling_standard", version = "fixture",
           markdown_path = md, json_path = js, html_path = "", error = "")
    },
    ingest_render_pdf_pages = function(...) character()
  )
  first <- ingest_document_bundle(pipeline_metadata(), pdf, cfg)
  second <- ingest_document_bundle(pipeline_metadata(), pdf, cfg)
  expect_equal(first$source$sha256, second$source$sha256)
  expect_true(second$reused)
  expect_true(file.exists(first$manifest_path))
})

test_that("dual-lane comparison finds material parameter discrepancies", {
  text <- list(available = TRUE, result = list(model_present = TRUE,
    recoverability = list(overall = 0.9), extraction = pipeline_extraction(5)))
  vision_same <- list(available = TRUE, result = list(model_present = TRUE,
    recoverability = list(overall = 0.85), extraction = pipeline_extraction(5)))
  vision_diff <- vision_same; vision_diff$result$extraction <- pipeline_extraction(8)
  same <- ingest_compare_extractions(text, vision_same)
  different <- ingest_compare_extractions(text, vision_diff)
  expect_true(same$consistent)
  expect_false(different$consistent)
  expect_true(any(vapply(different$differences, function(x) x$field == "theta.CL" && x$impact == "major", logical(1))))
})

test_that("Ollama vision messages retain a one-image JSON array", {
  message <- list(role = "user", content = list(
    list(type = "text", text = "read this"),
    list(type = "image_url", image_url = list(url = "data:image/png;base64,YWJj"))
  ))
  adapted <- LibeRary:::.library_ollama_messages(list(message))
  expect_type(adapted[[1]]$images, "list")
  expect_length(adapted[[1]]$images, 1L)
  encoded <- jsonlite::toJSON(adapted, auto_unbox = TRUE)
  expect_match(encoded, '"images":\\["YWJj"\\]')
})

test_that("vision roles exclude Ollama text-only models", {
  models <- data.frame(
    id = c("text-only", "multimodal"),
    name = c("text-only", "multimodal"),
    provider = "ollama",
    text_usable = TRUE,
    vision_capable = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  text_models <- LibeRary:::.library_models_for_role(models, "indexing")
  vision_models <- LibeRary:::.library_models_for_role(models, "vision")
  adjudication_models <- LibeRary:::.library_models_for_role(models, "adjudication")
  expect_true(all(text_models$usable))
  expect_equal(vision_models$id[vision_models$usable], "multimodal")
  expect_equal(adjudication_models$id[adjudication_models$usable], "multimodal")
})

test_that("process batch reports article stages independently from batch progress", {
  root <- tempfile("liberary-nested-progress-")
  dir.create(root)
  pdf <- file.path(root, "article.pdf")
  writeBin(charToRaw("%PDF-1.4\nfixture"), pdf)
  bundle_dir <- file.path(root, "bundle")
  dir.create(bundle_dir)
  cfg <- ingest_load_config()
  cfg$data_dir <- root
  cfg$inbox_dir <- file.path(root, "inbox")
  cfg$cache_dir <- file.path(root, "cache")
  cfg$catalog_dir <- file.path(root, "catalog")
  manifest <- data.frame(
    pmid = "9001", title = "Fixture", doi = "", publisher = "", year = "2026", pmcid = "",
    local_path = pdf, triage_tier = "high", acquisition_class = "oa_auto",
    stringsAsFactors = FALSE
  )
  batch_events <- list()
  article_events <- list()
  local_mocked_bindings(
    ingest_entrez_fetch_metadata = function(...) list(`9001` = pipeline_metadata()),
    ingest_document_bundle = function(...) list(
      bundle_path = bundle_dir, source = list(sha256 = "fixture"),
      parser = list(name = "fixture"), manifest_path = file.path(bundle_dir, "bundle.json")
    ),
    ingest_dual_extract = function(metadata, bundle, cfg, adjudicate, progress) {
      progress(0.03, "Parsed-text extraction (LLM inference)", "text_extraction")
      progress(0.64, "PDF vision extraction complete — 84% GPU / 16% CPU", "vision_extraction")
      progress(1, "Extraction complete", "complete")
      list(
        status = "machine_consistent", model_present = FALSE, extraction = NULL,
        comparison = list(comparable = TRUE, consistent = TRUE), adjudication = NULL,
        warning = "", audit = list(ok = TRUE),
        text = list(available = TRUE), vision = list(available = TRUE)
      )
    },
    .package = "LibeRary"
  )
  result <- ingest_process_batch(
    manifest, cfg, resume = FALSE,
    progress = function(...) batch_events[[length(batch_events) + 1L]] <<- list(...),
    article_progress = function(...) article_events[[length(article_events) + 1L]] <<- list(...)
  )
  expect_equal(result$summary$selected, 1L)
  expect_true(any(vapply(article_events, function(x) identical(x[[6L]], "vision_extraction"), logical(1))))
  expect_true(any(vapply(article_events, function(x) grepl("84% GPU", x[[2L]]), logical(1))))
  expect_equal(tail(batch_events, 1L)[[1L]][[1L]], 1)
})
