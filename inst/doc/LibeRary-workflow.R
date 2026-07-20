## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ----eval=FALSE---------------------------------------------------------------
# library(LibeRary)
# cfg <- ingest_load_config()
# cfg$entrez$email <- "you@example.org"
# library_save_config(cfg)


## ----eval=FALSE---------------------------------------------------------------
# library_llm_models("ollama", cfg, role = "indexing")
# 
# cfg$llm$triage$provider <- "same"
# cfg$llm$indexing$model <- "text-model"
# cfg$llm$vision$model <- "vision-model"
# cfg$llm$adjudication$model <- "adjudication-model"
# cfg$llm$require_independent_extraction_models <- FALSE


## ----eval=FALSE---------------------------------------------------------------
# found <- ingest_discover(limit = 100, cfg = cfg)
# found$summary
# found$low_backlog_path


## ----eval=FALSE---------------------------------------------------------------
# ingest_fetch_institutional(
#   found$manifest_path, cfg = cfg,
#   classes = "deferred_low", tiers = "low"
# )
# ingest_process_batch(found$manifest_path, cfg = cfg, tiers = "low")


## ----eval=FALSE---------------------------------------------------------------
# ingest_docling_available(cfg)
# bundle <- ingest_document_bundle(metadata, "article.pdf", cfg)


## ----eval=FALSE---------------------------------------------------------------
# dual <- ingest_dual_extract(metadata, bundle, cfg, adjudicate = TRUE)
# dual$comparison$differences
# dual$status


## ----eval=FALSE---------------------------------------------------------------
# enriched <- library_model_enrich(extraction)
# enriched$structural_model$canonical
# enriched$structural_model$implementations


## ----eval=FALSE---------------------------------------------------------------
# plan <- library_reproduction_plan(enriched)
# if (plan$eligible) {
#   result <- library_reproduction_run(
#     plan, "reproduction/pmid-123", nsim = 200, n_cores = 4
#   )
# }


## ----eval=FALSE---------------------------------------------------------------
# result <- ingest_process_batch(
#   found$manifest_path,
#   cfg = cfg,
#   tiers = c("high", "intermediate"),
#   resume = TRUE,
#   adjudicate = TRUE
# )
# library_list(root = cfg$catalog_dir)


## ----eval=FALSE---------------------------------------------------------------
# imported <- library_use_in_workspace(
#   "pmid_12345678",
#   workspace = LibeRation::nm_workspace()
# )


## ----eval=FALSE---------------------------------------------------------------
# job <- library_job(
#   "dual_extract", metadata,
#   pdf_path = "article.pdf",
#   confirm_transfer = TRUE,
#   cfg = cfg
# )
# queue$submit(job)

