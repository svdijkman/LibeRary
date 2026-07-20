reference_test_corpus <- function(partition = "test", training_eligible = FALSE) {
  root <- tempfile("reference-corpus-")
  for (directory in c("models", "articles", "screening", "partitions")) {
    dir.create(file.path(root, directory), recursive = TRUE)
  }
  manifest <- list(schema_version = "1.0.0", corpus_id = "aed-pkpd-reference",
                   version = "test", counts = list(), safeguards = list())
  article <- list(schema_version = "1.0.0", article_id = "pmid_12345", pmid = "12345",
                  title = "Test PK model", abstract = "A population PK model was developed.",
                  compounds = "PHT", model_ids = "aed_pht_12345_m01", partition = partition)
  target <- list(
    title = "Test PK model", compound = "PHT", population = "N: 20 adults",
    route = "oral", n_subjects = 20, software = NULL, estimation_method = NULL,
    model_type = "PK", structural_model = list(advan = NULL, trans = NULL,
      compartments = 1L, description = "1 compartment with first order absorption"),
    parameters = list(
      theta = list(list(name = "CL", typical = 5, se = NULL, unit = "L/h")),
      omega = list(list(description = "IIV CL", parameter = "CL", eta_index = 1L,
        eta_distribution = "log_normal", eta_expression = "CL=THETA(1)*exp(ETA(1))",
        variability_level = "iiv", reported_value = 0.1, reported_metric = "variance",
        value = 0.1, conversion = "none")),
      omega_covariance = list(), sigma = list()),
    covariates = character(), residual_error = NULL,
    confidence = list(overall = 1, fields = list(structure = 1, parameters = 1,
      population = 1, software = 0)), evidence_quotes = character(), notes = "test"
  )
  model <- list(
    schema_version = "1.0.0", corpus_id = "aed-pkpd-reference",
    reference_id = "aed_pht_12345_m01", article_id = "pmid_12345", pmid = "12345",
    model_index = 1L, compound_group = "PHT", family_id = "pmid_12345", partition = partition,
    source = list(appendix = list(path = "", sha256 = ""),
                  publication_pdf = list(path = "", sha256 = "")),
    raw = list(equations = "CL=theta1*exp(eta1)"),
    reference = list(extraction_target = target, study = list(), covariates = list(), validation = list()),
    provenance = list(field_tiers = list(structure = "C")),
    quality = list(tier = "C", review_status = "unreviewed", strict_score_eligible = FALSE,
                   training_eligible = training_eligible, issues = character())
  )
  getFromNamespace(".library_atomic_write", "LibeRary")(manifest, file.path(root, "manifest.json"))
  getFromNamespace(".library_atomic_write", "LibeRary")(article, file.path(root, "articles", "pmid_12345.json"))
  getFromNamespace(".library_atomic_write", "LibeRary")(model, file.path(root, "models", "aed_pht_12345_m01.json"))
  for (name in c("train", "validation", "test")) {
    ids <- if (identical(name, partition)) "aed_pht_12345_m01" else character()
    getFromNamespace(".library_atomic_write", "LibeRary")(
      list(partition = name, model_ids = ids, article_ids = if (length(ids)) "pmid_12345" else character(),
           screening_ids = character()), file.path(root, "partitions", paste0(name, ".json")))
  }
  root
}

test_that("reference parser retains historical six-digit PMIDs", {
  key <- getFromNamespace(".library_reference_key", "LibeRary")("Heimann 1977 590316")
  expect_equal(key$pmid, "590316")
  expect_equal(key$year, "1977")
})

test_that("reference parameter parser preserves NONMEM-scale variability", {
  parse_parameters <- getFromNamespace(".library_reference_parameters", "LibeRary")
  value <- parse_parameters(
    "theta1=5 omega1=0.17 omega1,2=0.02 omega2=0.0065 sigma1=0.04",
    "CL=theta1*exp(eta1) V=theta2*exp(eta2) Cobs=Cipred*(1+eps1)"
  )
  expect_equal(value$theta[[1]]$name, "CL")
  expect_equal(value$omega[[1]]$value, 0.17)
  expect_equal(value$omega[[1]]$reported_metric, "variance")
  expect_equal(value$omega[[1]]$eta_distribution, "log_normal")
  expect_equal(value$omega_covariance[[1]]$value, 0.02)
  expect_equal(value$sigma[[1]]$error_model, "proportional")
})

test_that("article partitions are deterministic and PMID grouped", {
  partition <- getFromNamespace(".library_reference_partition", "LibeRary")
  expect_identical(partition("23018530", 20260716, 0.15, 0.15),
                   partition("23018530", 20260716, 0.15, 0.15))
})

test_that("validation rejects training leakage from the test partition", {
  root <- reference_test_corpus(partition = "test", training_eligible = TRUE)
  checked <- library_reference_validate(root)
  expect_false(checked$valid)
  expect_true(any(grepl("training_eligible", checked$errors)))
})

test_that("extraction benchmark reports silver metrics separately", {
  root <- reference_test_corpus(partition = "test")
  predictions <- tempfile("reference-predictions-")
  dir.create(predictions)
  target <- library_reference_get("aed_pht_12345_m01", root)$reference$extraction_target
  getFromNamespace(".library_atomic_write", "LibeRary")(
    list(reference_id = "aed_pht_12345_m01", prediction = target),
    file.path(predictions, "aed_pht_12345_m01.json")
  )
  result <- library_reference_benchmark(root, predictions, task = "extraction", partition = "test")
  expect_equal(result$summary$strict$n, 0)
  expect_equal(result$summary$silver$n, 1)
  expect_equal(result$summary$silver$numeric_accuracy, 1)
  expect_equal(result$summary$silver$semantic_accuracy, 1)
})

test_that("training export refuses the locked test partition", {
  root <- reference_test_corpus(partition = "test")
  expect_error(
    library_reference_training_export(root, root, tempfile("training-export-"),
                                      tasks = "extraction", partitions = "test"),
    "never be exported"
  )
})

test_that("curation creates a successor corpus without changing its parent", {
  root <- reference_test_corpus(partition = "train")
  successor <- tempfile("reference-successor-")
  decisions <- data.frame(reference_id = "aed_pht_12345_m01", tier = "B",
                          training_eligible = TRUE, notes = "Checked", stringsAsFactors = FALSE)
  library_reference_revise(root, successor, decisions, version = "0.2.0", curator = "tester")
  expect_equal(library_reference_get("aed_pht_12345_m01", root)$quality$tier, "C")
  revised <- library_reference_get("aed_pht_12345_m01", successor)
  expect_equal(revised$quality$tier, "B")
  expect_true(revised$quality$training_eligible)
  expect_true(library_reference_validate(successor)$valid)
})

test_that("reference comparison aligns JSON leaves and exposes prediction variants", {
  root <- reference_test_corpus(partition = "test")
  predictions <- tempfile("reference-comparison-")
  dir.create(predictions)
  target <- library_reference_get("aed_pht_12345_m01", root)$reference$extraction_target
  prediction <- target
  prediction$parameters$theta[[1L]]$typical <- 7.5
  prediction$software <- "NONMEM"
  text_variant <- prediction
  text_variant$parameters$theta[[1L]]$typical <- 5
  getFromNamespace(".library_atomic_write", "LibeRary")(
    list(reference_id = "aed_pht_12345_m01", prediction = prediction,
         variants = list(text = text_variant)),
    file.path(predictions, "aed_pht_12345_m01.json")
  )

  compared <- library_reference_compare("aed_pht_12345_m01", root, predictions)
  theta <- compared$comparison[compared$comparison$pointer == "/parameters/theta/0/typical", ]
  expect_equal(theta$status, "Different")
  expect_equal(theta$delta_percent, 50)
  expect_setequal(compared$variants, c("prediction", "text"))

  text <- library_reference_compare("aed_pht_12345_m01", root, predictions, variant = "text")
  theta <- text$comparison[text$comparison$pointer == "/parameters/theta/0/typical", ]
  expect_equal(theta$status, "Match")
})

test_that("reference comparison remains useful when predictions are unavailable", {
  root <- reference_test_corpus(partition = "test")
  compared <- library_reference_compare(
    "aed_pht_12345_m01", root, file.path(tempdir(), "missing-predictions")
  )
  expect_null(compared$prediction)
  expect_true(all(compared$comparison$status == "Review only"))
})

test_that("field corrections update only a successor normalized target", {
  root <- reference_test_corpus(partition = "train")
  successor <- tempfile("reference-corrected-")
  decisions <- data.frame(
    reference_id = "aed_pht_12345_m01", tier = "B", training_eligible = TRUE,
    review_status = "reviewed", notes = "Compared with source", stringsAsFactors = FALSE
  )
  corrections <- data.frame(
    reference_id = "aed_pht_12345_m01",
    pointer = "/parameters/theta/0/typical",
    source = "liberary", value_json = "7.5", stringsAsFactors = FALSE
  )
  original <- library_reference_get("aed_pht_12345_m01", root)
  library_reference_revise(
    root, successor, decisions, version = "0.2.0", curator = "tester",
    corrections = corrections
  )
  revised <- library_reference_get("aed_pht_12345_m01", successor)
  expect_equal(original$reference$extraction_target$parameters$theta[[1L]]$typical, 5)
  expect_equal(revised$reference$extraction_target$parameters$theta[[1L]]$typical, 7.5)
  expect_identical(revised$raw, original$raw)
  expect_equal(revised$provenance$curation[[1L]]$field_corrections[[1L]]$source,
               "liberary")
  expect_true(library_reference_validate(successor)$valid)
})

test_that("packaged adapter-training tools are discoverable", {
  files <- library_reference_training_files()
  expect_true(file.exists(files$trainer))
  expect_true(file.exists(files$requirements))
})
