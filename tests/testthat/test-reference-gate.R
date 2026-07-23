test_that("reference release gate is strict-tier only and fail closed", {
  qualifying <- list(strict = list(
    n = 20L, scored = 20L,
    scalar_coverage = 0.95, scalar_score = 0.9,
    numeric_coverage = 0.92, numeric_accuracy = 0.94,
    semantic_coverage = 0.9, semantic_accuracy = 0.88
  ), silver = list(n = 500L, scalar_score = 1))
  gate <- library_reference_release_gate(qualifying)
  expect_s3_class(gate, "library_reference_gate")
  expect_true(gate$passed)

  silver_only <- qualifying
  silver_only$strict <- list(n = 0L, scored = 0L)
  failed <- library_reference_release_gate(silver_only)
  expect_false(failed$passed)
  expect_error(
    library_reference_release_gate(silver_only, error = TRUE),
    "strict reference gate failed"
  )
})
