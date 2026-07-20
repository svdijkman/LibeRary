## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ----eval=FALSE---------------------------------------------------------------
# library(LibeRary)
# 
# root <- "validation/liberary/aed-pkpd-reference/0.2.2"
# library_reference_build("AED_PKPD", root, version = "0.2.2")
# library_reference_validate(root, check_hashes = TRUE,
#                            source_dir = "AED_PKPD")
# library_reference_list(root)


## ----eval=FALSE---------------------------------------------------------------
# cfg <- ingest_load_config()
# 
# library_reference_run(
#   root = root,
#   source_dir = "AED_PKPD",
#   output_dir = "benchmark/aed-current",
#   cfg = cfg,
#   task = "extraction",
#   partition = "test",
#   resume = TRUE,
#   assess = FALSE
# )
# 
# score <- library_reference_benchmark(
#   root,
#   "benchmark/aed-current/predictions",
#   task = "extraction",
#   partition = "test",
#   output_dir = "benchmark/aed-current/report"
# )
# score$summary


## ----eval=FALSE---------------------------------------------------------------
# library_reference_shiny(
#   corpus = "validation/liberary/aed-pkpd-reference/0.2.2",
#   predictions = "validation/liberary/aed-pkpd-benchmark/text-current/predictions",
#   source_dir = "AED_PKPD"
# )
# 
# # The same application is available through the general launcher.
# library_shiny(mode = "reference", reference_root = root,
#               predictions = "benchmark/current/predictions",
#               source_dir = "AED_PKPD")


## ----eval=FALSE---------------------------------------------------------------
# library_reference_revise(
#   root,
#   "validation/liberary/aed-pkpd-reference/0.2.0",
#   decisions = "review-decisions.csv",
#   version = "0.2.0",
#   curator = "reviewer-id"
# )


## ----eval=FALSE---------------------------------------------------------------
# library_reference_training_export(
#   "validation/liberary/aed-pkpd-reference/0.2.0",
#   source_dir = "AED_PKPD",
#   output_dir = "training/aed-pkpd",
#   tasks = c("extraction", "triage"),
#   partitions = c("train", "validation")
# )

