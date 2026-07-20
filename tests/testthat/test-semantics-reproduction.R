test_that("population normalization retains multiple cohorts and statistics", {
  population <- library_population_normalize(
    "Elderly N: 37 Age: 62.9+/-6.18 (51-75) WT: 77+/-11.64; Young N: 35 Age: 34.1+/-7.8 WT: 69.01+/-12.3"
  )
  expect_equal(population$n_total, 72)
  expect_length(population$cohorts, 2L)
  expect_equal(vapply(population$cohorts, `[[`, character(1), "label"), c("Elderly", "Young"))
  expect_equal(population$cohorts[[1L]]$n$analysed, 37L)
  age <- population$cohorts[[1L]]$descriptors[[1L]]
  expect_equal(age$name, "age")
  expect_setequal(vapply(age$statistics, `[[`, character(1), "type"), c("mean_sd", "range"))
})

test_that("reproduction planning converts explicit units and rejects unsafe regimens", {
  regimen <- list(amount = 250, amount_unit = "mcg/kg")
  converted <- LibeRary:::.library_reproduction_amount_mg(regimen, list(WT = 80))
  expect_equal(converted$value, 20)

  unknown <- LibeRary:::.library_reproduction_amount_mg(
    list(amount = 2, amount_unit = "tablet"), list()
  )
  expect_equal(unknown$blocker, "dose_unit_not_convertible_to_mg")

  extraction <- list(
    title = "Steady state", population = "N: 10", route = "oral", n_subjects = 10,
    dosing = list(list(
      cohort_id = "overall", route = "oral", administration = "unspecified",
      amount = 100, amount_unit = "mg", interval = NULL, interval_unit = NULL,
      duration = NULL, duration_unit = NULL, repetitions = NULL, steady_state = TRUE,
      raw = "100 mg at steady state",
      source = list(status = "reported", source_locator = "Methods", evidence = "100 mg", confidence = 1)
    )),
    structural_model = list(advan = 2L, trans = 2L, compartments = 1L,
                            description = "One compartment oral model"),
    parameters = list(theta = list(list(name = "KA", typical = 1),
                                   list(name = "CL", typical = 5),
                                   list(name = "V", typical = 50)),
                      omega = list(), omega_covariance = list(), sigma = list()),
    reproduction_targets = list(), confidence = list(overall = 1, fields = list()),
    covariates = character(), residual_error = "Y=F"
  )
  plan <- library_reproduction_plan(extraction, allow_generated_defaults = TRUE)
  expect_false(plan$eligible)
  expect_true("steady_state_interval_missing" %in% plan$blockers)
})

test_that("common ADVAN and TRANS mappings are inferred with provenance", {
  implementation <- library_infer_implementation(
    list(advan = NULL, trans = NULL, compartments = 1L,
         description = "One compartment with first-order absorption and linear elimination"),
    route = "oral",
    parameters = list(theta = list(
      list(name = "KA"), list(name = "CL"), list(name = "V")
    ))
  )
  expect_equal(implementation$advan, 2L)
  expect_equal(implementation$trans, 2L)
  expect_equal(implementation$status, "inferred")
  expect_true(implementation$review_required)

  nonlinear <- library_infer_implementation(
    list(advan = NULL, trans = NULL, compartments = 1L,
         description = "One compartment with Michaelis-Menten elimination"),
    route = "oral", parameters = list(theta = list(list(name = "VMAX"), list(name = "KM")))
  )
  expect_null(nonlinear$advan)
  expect_setequal(vapply(nonlinear$alternatives, `[[`, integer(1), "advan"), c(6L, 13L))
})

test_that("model enrichment remains backward compatible", {
  extraction <- list(
    title = "Example", compound = "X", population = "N: 20 Age: 40+/-10 WT: 70+/-12",
    route = "intravenous", n_subjects = 20L, dose = "100 mg intravenous bolus",
    structural_model = list(advan = NULL, trans = NULL, compartments = 1L,
                            description = "One compartment linear elimination"),
    parameters = list(theta = list(list(name = "CL", typical = 5),
                                   list(name = "V", typical = 50)),
                      omega = list(), omega_covariance = list(), sigma = list())
  )
  enriched <- library_model_enrich(extraction)
  expect_equal(enriched$population, extraction$population)
  expect_equal(enriched$n_subjects, 20)
  expect_equal(enriched$structural_model$advan, 1L)
  expect_equal(enriched$structural_model$trans, 2L)
  expect_equal(enriched$dosing[[1L]]$amount, 100)
})

test_that("structured dosing permits an explicitly unknown route", {
  regimen <- list(
    cohort_id = "cohort_1", route = NULL, administration = "intravenous infusion",
    amount = 600, amount_unit = "mg", interval = 12, interval_unit = "h",
    duration = NULL, duration_unit = NULL, repetitions = NULL,
    steady_state = FALSE, raw = NULL,
    source = list(status = "reported", source_locator = "Methods",
                  evidence = "600 mg q12h", confidence = 0.9)
  )
  normalized <- library_dosing_normalize(list(regimen), route = "intravenous")
  expect_identical(normalized[[1L]]$route, NULL)
  expect_equal(normalized[[1L]]$amount, 600)
  expect_equal(LibeRary:::.library_semantic_scalar(c("first", "second")), "first")
})

test_that("reproduction plans expose blockers instead of inventing evidence", {
  extraction <- ingest_stub_extraction(list(
    pmid = "1", title = "Incomplete", abstract = "NONMEM oral PK",
    doi = "", journal = "", year = "", pmcid = "", authors = character()
  ))
  plan <- library_reproduction_plan(extraction)
  expect_false(plan$eligible)
  expect_true("dose_amount_missing" %in% plan$blockers)
  expect_true("generated_parameter_defaults_present" %in% plan$blockers)
})

test_that("reproduction planning accepts previously unseen demographic descriptors", {
  descriptor <- function(name, value) list(
    name = name, unit = NULL,
    statistics = list(list(type = "reported_center", value = value)),
    categories = list(), source = list()
  )
  details <- list(cohorts = list(list(descriptors = list(
    descriptor("CrCL (mL/min)", 83), descriptor("Baseline characteristics", 1)
  ))))
  covariates <- LibeRary:::.library_reproduction_covariates(details)
  expect_equal(covariates$CRCL_ML_MIN, 83)
  expect_equal(covariates$BASELINE_CHARACTERISTICS, 1)
})

test_that("complete reproduction plans execute with LibeRation", {
  skip_if_not_installed("LibeRation")
  extraction <- list(
    title = "IV example", compound = "X", population = "N: 3 WT: 70+/-10",
    population_details = NULL, route = "intravenous", n_subjects = 3L,
    dosing = list(list(
      cohort_id = "overall", route = "intravenous", administration = "bolus",
      amount = 100, amount_unit = "mg", interval = NULL, interval_unit = NULL,
      duration = NULL, duration_unit = NULL, repetitions = 1L, steady_state = FALSE,
      raw = "100 mg IV bolus",
      source = list(status = "reported", source_locator = "Table 1", evidence = "100 mg IV bolus", confidence = 1)
    )),
    structural_model = list(advan = 1L, trans = 2L, compartments = 1L,
                            description = "One compartment IV bolus"),
    parameters = list(
      theta = list(list(name = "CL", typical = 5, se = NULL, unit = "L/h"),
                   list(name = "V", typical = 50, se = NULL, unit = "L")),
      omega = list(), omega_covariance = list(), sigma = list()
    ),
    covariates = character(), residual_error = "Y = F",
    reproduction_targets = list(list(
      id = "figure_1", kind = "concentration_time", source_type = "figure",
      source_locator = "Figure 1", cohort_id = "overall", analyte = "X",
      statistic = "median", x_unit = "h", y_unit = "mg/L", scale = "linear",
      points = list(list(time = 1, value = 1.8, lower = NULL, upper = NULL),
                    list(time = 4, value = 1.3, lower = NULL, upper = NULL)),
      evidence = "Digitized points", confidence = 0.8
    )), confidence = list(overall = 1, fields = list()), evidence_quotes = character(), notes = ""
  )
  plan <- library_reproduction_plan(extraction, n_subjects = 3L, allow_generated_defaults = TRUE)
  expect_true(plan$eligible)
  expect_true(plan$scorable)
  output <- tempfile("reproduction-")
  result <- library_reproduction_run(plan, output_dir = output, nsim = 2L, seed = 12L,
                                     allow_generated_defaults = TRUE)
  expect_s3_class(result, "library_reproduction_result")
  expect_true(file.exists(file.path(output, "reproduction.json")))
  expect_true(file.exists(file.path(output, "reproduction.png")))
  expect_true(result$score$scorable)
})
