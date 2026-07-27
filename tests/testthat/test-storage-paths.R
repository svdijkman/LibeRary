test_that("LibeRary home supports an explicit isolated repository", {
  root <- file.path(tempdir(), paste0("liberary-home-", Sys.getpid()))
  old <- Sys.getenv("LIBERARY_HOME", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("LIBERARY_HOME")
    else Sys.setenv(LIBERARY_HOME = old)
    unlink(root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  Sys.setenv(LIBERARY_HOME = root)
  expect_false(dir.exists(root))
  expect_equal(
    library_home(create = FALSE),
    normalizePath(root, winslash = "/", mustWork = FALSE)
  )
  expect_equal(
    library_home(create = TRUE),
    normalizePath(root, winslash = "/", mustWork = TRUE)
  )
  expect_true(dir.exists(root))
})

test_that("shared root names cannot escape their parent directory", {
  expect_error(
    .liber_shared_user_root(root_name = "../outside"),
    "single directory name",
    fixed = TRUE
  )
})
