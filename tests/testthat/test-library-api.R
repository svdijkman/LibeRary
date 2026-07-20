test_that("library_list returns gold entries", {
  skip_if_not(dir.exists(system.file("catalog", package = "LibeRary")))
  df <- library_list()
  expect_gte(nrow(df), 3L)
  expect_true("lib_theo_synthetic" %in% df$library_id)
})

test_that("library_get reads manifest", {
  skip_if_not(dir.exists(system.file("catalog", package = "LibeRary")))
  e <- library_get("lib_theo_synthetic")
  expect_equal(e$manifest$model$advan, 4L)
  expect_true(file.exists(e$paths$ctl))
})

test_that("library_search finds theo", {
  skip_if_not(dir.exists(system.file("catalog", package = "LibeRary")))
  hits <- library_search("theophylline")
  expect_true(any(grepl("theo", hits$library_id, ignore.case = TRUE)))
})
