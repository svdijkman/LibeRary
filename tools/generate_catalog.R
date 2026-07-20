# Generate inst/catalog entries from LibeRation synthetic models.
# Run from LibeRary root: Rscript tools/generate_catalog.R

pkg_root <- if (file.exists("DESCRIPTION")) "." else dirname(dirname(normalizePath(sys.frame(1)$ofile)))
lib_root <- normalizePath(pkg_root, mustWork = TRUE)

if (!requireNamespace("LibeRation", quietly = TRUE)) {
  stop("LibeRation must be installed to generate catalog entries.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite required.")
}

catalog_dir <- file.path(lib_root, "inst", "catalog")
entries_dir <- file.path(catalog_dir, "entries")
dir.create(entries_dir, recursive = TRUE, showWarnings = FALSE)

specs <- list(
  list(
    library_id = "lib_theo_synthetic",
    synthetic_id = "theo",
    status = "validated",
    compound = "theophylline",
    population = "general (synthetic teaching dataset)",
    route = "oral",
    confidence = 1.0
  ),
  list(
    library_id = "lib_iv1_synthetic",
    synthetic_id = "iv1",
    status = "validated",
    compound = "generic drug (synthetic)",
    population = "general (synthetic IV bolus)",
    route = "iv_bolus",
    confidence = 1.0
  ),
  list(
    library_id = "lib_iv2_synthetic",
    synthetic_id = "iv2",
    status = "validated",
    compound = "generic drug (synthetic)",
    population = "general (synthetic IV bolus 2-compartment)",
    route = "iv_bolus",
    confidence = 1.0
  )
)

index_entries <- list()

for (sp in specs) {
  sim <- LibeRation::nm_synthetic_dataset(sp$synthetic_id, n_sub = 2L, seed = 1L)
  cat_info <- LibeRation::nm_synthetic_catalog()[[sp$synthetic_id]]

  ctl <- LibeRation::nm_ctl_compose(list(
    problem = cat_info$label,
    advan = sim$model$ADVAN,
    trans = sim$model$TRANS,
    use_ode = isTRUE(sim$model$USE_ODE),
    subroutine = "",
    data_file = cat_info$csv,
    input_cols = sim$model$INPUT,
    thetas = sim$model$THETAS,
    omegas = sim$model$OMEGAS,
    sigmas = sim$model$SIGMAS,
    pk = sim$model$PRED,
    error = sim$model$ERROR
  ))

  edir <- file.path(entries_dir, sp$library_id)
  dir.create(edir, recursive = TRUE, showWarnings = FALSE)
  writeLines(ctl, file.path(edir, "model.ctl"))

  manifest <- list(
    schema_version = "1.1.0",
    library_id = sp$library_id,
    version = "1.0.1",
    status = sp$status,
    title = cat_info$label,
    model = list(
      artifact = "model.ctl",
      advan = sim$model$ADVAN,
      trans = sim$model$TRANS,
      type = "pk",
      use_ode = isTRUE(sim$model$USE_ODE)
    ),
    study = list(
      compound = sp$compound,
      population = sp$population,
      route = sp$route,
      n_subjects = NULL,
      keywords = c("population pk", "synthetic", sp$synthetic_id),
      dataset_fingerprint = paste0("synthetic:", sp$synthetic_id)
    ),
    confidence = list(
      overall = sp$confidence,
      fields = list(
        structure = sp$confidence,
        parameters = sp$confidence,
        population = sp$confidence
      )
    ),
    provenance = list(
      source_type = "synthetic",
      origin = "LibeRation",
      synthetic_id = sp$synthetic_id,
      description = cat_info$description
    ),
    qualification = list(
      author_validated = TRUE,
      curator = "LibeRation synthetic gold standard"
    ),
    relations = list(
      same_compound = character(),
      prior_version = character(),
      mbma_pool = FALSE
    )
  )

  jsonlite::write_json(
    manifest,
    file.path(edir, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  index_entries[[length(index_entries) + 1L]] <- list(
    library_id = sp$library_id,
    title = cat_info$label,
    status = sp$status
  )
  message("Wrote ", sp$library_id)
}

index <- list(
  schema_version = "1.1.0",
  updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
  entries = index_entries
)
jsonlite::write_json(
  index,
  file.path(catalog_dir, "index.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
message("Catalog index written to ", file.path(catalog_dir, "index.json"))
