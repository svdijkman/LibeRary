test_that("ingest_normalize_doi strips URL prefix", {
  expect_equal(
    ingest_normalize_doi("https://doi.org/10.1000/xyz"),
    "10.1000/xyz"
  )
})

test_that("ingest_score_model_relevance detects NONMEM", {
  s <- ingest_score_model_relevance("We used NONMEM for population PK.")
  expect_gte(s$score, 1L)
  expect_true(any(grepl("nonmem", s$keywords)))
})

test_that("ingest_throttle enforces minimum interval", {
  t0 <- Sys.time()
  ingest_throttle("test_throttle_unit", 0.2)
  ingest_throttle("test_throttle_unit", 0.2)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_gte(elapsed, 0.15)
})

test_that("ingest_is_probably_pdf checks magic bytes", {
  tmp <- tempfile(fileext = ".pdf")
  writeBin(charToRaw("%PDF-1.4"), tmp)
  expect_true(ingest_is_probably_pdf(tmp))
  writeLines("not a pdf", tmp)
  expect_false(ingest_is_probably_pdf(tmp))
})

test_that("ingest_stub_extraction creates low confidence stub", {
  meta <- list(
    pmid = "123",
    title = "Population PK with NONMEM",
    abstract = "We used NONMEM for population pharmacokinetics.",
    doi = "", journal = "", year = "", pmcid = "", authors = character()
  )
  stub <- ingest_stub_extraction(meta)
  expect_equal(stub$software, "NONMEM")
  expect_lt(stub$confidence$overall, 0.5)
})

test_that("LLM percentage confidences are normalized without clamping invalid values", {
  parsed <- ingest_parse_llm_json(paste0(
    '{"confidence":{"overall":83,"fields":{"structure":82.6,',
    '"parameters":"79%","population":1,"software":100}}}'
  ))
  expect_equal(parsed$confidence$overall, 0.83)
  expect_equal(parsed$confidence$fields$structure, 0.826)
  expect_equal(parsed$confidence$fields$parameters, 0.79)
  expect_equal(parsed$confidence$fields$population, 1)
  expect_equal(parsed$confidence$fields$software, 1)
  expect_equal(ingest_parse_llm_json('{"coverage":85}')$coverage, 0.85)
  expect_error(
    ingest_parse_llm_json('{"confidence":101}'),
    "expected 0-1 or 0-100%"
  )
})

test_that("structured JSON parsing repairs only safe formatting damage", {
  schema <- list(
    type = "object", additionalProperties = FALSE,
    properties = list(values = list(type = "array", items = list(type = "number"))),
    required = "values"
  )
  parsed <- ingest_parse_llm_json(
    "Result follows:\n```json\n{\"values\":[1,2,]\n```", schema
  )
  expect_equal(unlist(parsed$values), c(1, 2))
  expect_error(
    ingest_parse_llm_json('{"values":[1,"tru', schema),
    "value was truncated"
  )
  expect_error(
    ingest_parse_llm_json('{"values":1}', schema),
    "response.values: expected array"
  )
})

test_that("structured chat retries invalid output and retains every attempt", {
  cfg <- ingest_load_config()
  cfg$llm$structured_retries <- 1L
  calls <- 0L
  requests <- list()
  fake_chat <- function(messages, cfg, role, format, sensitive) {
    calls <<- calls + 1L
    requests[[calls]] <<- list(messages = messages, cfg = cfg)
    list(provider = "fixture", model = "fixture-model", content = if (calls == 1L) {
      '{"answer":"tru'
    } else '{"answer":true}', done_reason = if (calls == 1L) "length" else "stop")
  }
  schema <- list(type = "object", additionalProperties = FALSE,
                 properties = list(answer = list(type = "boolean")), required = "answer")
  result <- LibeRary:::.library_structured_chat(
    list(list(role = "user", content = "respond")), cfg, "triage", schema,
    sensitive = FALSE, chat = fake_chat
  )
  expect_true(result$ok)
  expect_true(result$value$answer)
  expect_equal(result$retry_count, 1L)
  expect_length(result$attempts, 2L)
  expect_match(result$attempts[[1L]]$response, '"tru$')
  expect_match(result$attempts[[1L]]$error, "output-length limit")
  expect_false(any(vapply(requests[[2L]]$messages, function(x) {
    identical(x$role, "assistant")
  }, logical(1))))
  expect_length(requests[[2L]]$messages, 2L)
  expect_match(requests[[2L]]$messages[[2L]]$content, "never duplicate", ignore.case = TRUE)
  expect_gt(requests[[2L]]$cfg$llm$triage$num_ctx, cfg$ollama$num_ctx)
  expect_equal(result$attempts[[1L]]$request_num_ctx, cfg$ollama$num_ctx)
  expect_equal(result$attempts[[2L]]$request_num_ctx,
               requests[[2L]]$cfg$llm$triage$num_ctx)
})

test_that("best-lane selection is deterministic with malformed confidence values", {
  lane <- function(confidence) list(
    available = TRUE,
    result = list(recoverability = list(overall = confidence), extraction = list(confidence = list(overall = 0)))
  )
  text <- lane(c(NA_real_, NaN))
  vision <- lane(character())
  expect_identical(LibeRary:::.library_best_lane(text, vision), text)
})

test_that("ingest_map_to_ctl produces ctl lines", {
  ext <- ingest_stub_extraction(list(
    pmid = "1", title = "Test", abstract = "NONMEM oral PK",
    doi = "", journal = "", year = "", pmcid = "", authors = character()
  ))
  ctl <- ingest_map_to_ctl(ext)
  expect_true(any(grepl("^\\$PROBLEM", ctl)))
  expect_true(any(grepl("^\\$THETA", ctl)))
})

test_that("control mapping converts CV percentages to NONMEM variances", {
  ext <- ingest_stub_extraction(list(
    pmid = "2", title = "Reported variability", abstract = "NONMEM PK",
    doi = "", journal = "", year = "", pmcid = "", authors = character()
  ))
  ext$route <- "intravenous"
  ext$structural_model <- list(advan = 1, trans = 2, compartments = 1, description = "One compartment")
  ext$parameters$theta <- list(
    list(name = "CL", typical = 5, se = NULL, unit = "L/h"),
    list(name = "V", typical = 50, se = NULL, unit = "L"),
    list(name = "extra", typical = 3, se = NULL, unit = NULL)
  )
  ext$parameters$omega <- list(list(description = "CV% CL", value = 37.9))
  ext$parameters$sigma <- list(list(description = "CV% residual", value = 14.1))
  ctl <- ingest_map_to_ctl(ext)
  mapping <- attr(ctl, "mapping")
  expect_false(any(grepl("^ 37[.]9$|^ 14[.]1$", ctl)))
  omega <- as.numeric(trimws(ctl[[match("$OMEGA", ctl) + 1L]]))
  sigma <- as.numeric(trimws(ctl[[match("$SIGMA", ctl) + 1L]]))
  expect_equal(omega, log1p((37.9 / 100)^2), tolerance = 1e-10)
  expect_equal(sigma, (14.1 / 100)^2, tolerance = 1e-10)
  expect_false("OMEGA(1)" %in% mapping$generated_defaults)
  expect_false("SIGMA(1)" %in% mapping$generated_defaults)
  expect_true("OMEGA(2)" %in% mapping$generated_defaults)
  expect_equal(mapping$unmapped_theta, "extra")
  expect_true(mapping$review_required)
})

test_that("normal ETA CV is converted on the absolute parameter scale", {
  ext <- ingest_stub_extraction(list(
    pmid = "3", title = "Normal ETA model", abstract = "NONMEM PK",
    doi = "", journal = "", year = "", pmcid = "", authors = character()
  ))
  ext$route <- "intravenous"
  ext$structural_model <- list(advan = 1, trans = 2, compartments = 1, description = "One compartment")
  ext$parameters$theta <- list(
    list(name = "CL", typical = 5, se = NULL, unit = "L/h"),
    list(name = "V", typical = 50, se = NULL, unit = "L")
  )
  ext$parameters$omega <- list(list(
    description = "IIV CL", parameter = "CL", eta_index = 1L,
    eta_distribution = "normal", eta_expression = "CL = THETA(1) + ETA(1)",
    variability_level = "iiv", reported_value = 20,
    reported_metric = "cv_percent", value = 1, conversion = "(5*0.2)^2"
  ))
  ctl <- ingest_map_to_ctl(ext)
  omega <- as.numeric(trimws(ctl[[match("$OMEGA", ctl) + 1L]]))
  expect_equal(omega, 1)
  expect_true(any(grepl("CL = THETA[(]1[)] [+] ETA[(]1[)]", ctl)))
})

test_that("OMEGA correlations render as a NONMEM block covariance", {
  ext <- ingest_stub_extraction(list(
    pmid = "4", title = "Correlated ETAs", abstract = "NONMEM PK",
    doi = "", journal = "", year = "", pmcid = "", authors = character()
  ))
  ext$route <- "intravenous"
  ext$structural_model <- list(advan = 1, trans = 2, compartments = 1, description = "One compartment")
  ext$parameters$theta <- list(
    list(name = "CL", typical = 5, se = NULL, unit = "L/h"),
    list(name = "V", typical = 50, se = NULL, unit = "L")
  )
  ext$parameters$omega <- list(
    list(description = "IIV CL", eta_index = 1L, eta_distribution = "log_normal",
         reported_value = 0.1, reported_metric = "variance", value = 0.1),
    list(description = "IIV V", eta_index = 2L, eta_distribution = "log_normal",
         reported_value = 0.2, reported_metric = "variance", value = 0.2)
  )
  ext$parameters$omega_covariance <- list(list(
    row_eta = 2L, col_eta = 1L, reported_value = 0.5,
    reported_metric = "correlation", value = sqrt(0.1 * 0.2) * 0.5,
    conversion = "rho*sqrt(var1*var2)"
  ))
  ctl <- ingest_map_to_ctl(ext)
  record <- match("$OMEGA BLOCK(2)", ctl)
  expect_false(is.na(record))
  expect_equal(as.numeric(strsplit(trimws(ctl[[record + 2L]]), " +")[[1L]][[1L]]),
               sqrt(0.1 * 0.2) * 0.5, tolerance = 1e-10)
})

test_that("null coalescing accepts callbacks without warnings", {
  callback <- function(...) TRUE
  expect_warning(value <- callback %||% identity, NA)
  expect_identical(value, callback)
})

test_that("GUI settings persist email but not the NCBI API key", {
  skip_if_not_installed("yaml")
  root <- tempfile("liberary-config-")
  dir.create(root)
  path <- file.path(root, "config.yml")
  cfg <- getFromNamespace("DEFAULT_CONFIG", "LibeRary")
  cfg$data_dir <- file.path(root, "data")
  cfg$entrez$email <- "researcher@example.org"
  cfg$entrez$api_key <- "session-secret"
  cfg$entrez$requests_per_second <- 2.5
  cfg$llm$indexing$instruction <- "Custom pharmacometric extraction instruction"

  library_save_config(cfg, path)
  restored <- ingest_load_config(path)

  expect_equal(restored$entrez$email, "researcher@example.org")
  expect_equal(restored$entrez$requests_per_second, 2.5)
  expect_equal(restored$llm$indexing$instruction, "Custom pharmacometric extraction instruction")
  expect_identical(yaml::read_yaml(path)$entrez$api_key, "")
})

test_that("installed GUI background bootstrap reports worker errors", {
  skip_if_not_installed("yaml")
  root <- tempfile("liberary-gui-worker-")
  dir.create(root)
  config <- file.path(root, "config.yml")
  cfg <- getFromNamespace("DEFAULT_CONFIG", "LibeRary")
  cfg$data_dir <- file.path(root, "data")
  cfg$entrez$email <- "researcher@example.org"
  library_save_config(cfg, config)

  params <- list(
    job = "intentional-test-error",
    source_root = FALSE,
    lib_path = tempdir(),
    config_path = config,
    entrez_email = cfg$entrez$email,
    data_dir = cfg$data_dir,
    log_path = file.path(root, "gui.log"),
    progress_path = file.path(root, "progress.json")
  )
  result <- getFromNamespace("ingest_gui_background_worker", "LibeRary")(params)
  progress <- ingest_read_progress(params$progress_path)

  expect_false(result$ok)
  expect_identical(progress$status, "error")
  expect_match(progress$message, "Unknown job")
})

test_that("GUI cancellation terminates the background process tree", {
  skip_if_not_installed("callr")
  directory <- tempfile("liberary-cancel-")
  dir.create(directory)
  progress <- file.path(directory, "progress.json")
  log <- file.path(directory, "worker.log")
  ingest_write_progress(progress, 0.42, "Processing article", 3L, 7L, "running")
  worker <- callr::r_bg(function() Sys.sleep(60), supervise = TRUE)
  on.exit(if (worker$is_alive()) worker$kill_tree(), add = TRUE)
  result <- LibeRary:::.ingest_gui_cancel_worker(worker, progress, log)
  expect_true(result$cancelled)
  expect_false(worker$is_alive())
  state <- ingest_read_progress(progress)
  expect_equal(state$status, "cancelled")
  expect_equal(state$value, 0.42)
  expect_equal(state$step, 3L)
  expect_match(paste(readLines(log, warn = FALSE), collapse = "\n"), "Cancelled by user")
})

test_that("GUI cancellation is harmless without an active worker", {
  result <- LibeRary:::.ingest_gui_cancel_worker(NULL, tempfile(), tempfile())
  expect_false(result$cancelled)
  expect_match(result$message, "No background job")
})

test_that("GUI progress retains separate batch and current-PMID channels", {
  directory <- tempfile("liberary-progress-")
  dir.create(directory)
  path <- file.path(directory, "progress.json")
  ingest_write_progress(path, 0.25, "Completed 2/8 articles", 2L, 8L, "running")
  LibeRary:::.ingest_write_current_progress(
    path, 0.4, "PMID 123 — PDF vision extraction (LLM inference)",
    4L, 8L, "123", "vision_extraction", "running"
  )
  ingest_write_progress(path, 0.375, "Completed 3/8 articles", 3L, 8L, "running")
  state <- ingest_read_progress(path)
  expect_equal(state$value, 0.375)
  expect_equal(state$batch$step, 3L)
  expect_equal(state$current$value, 0.4)
  expect_equal(state$current$pmid, "123")
  expect_equal(state$current$stage, "vision_extraction")
  expect_match(state$current$message, "vision extraction")
})
