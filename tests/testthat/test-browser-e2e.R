test_that("LibeRary catalogue renders in a real browser", {
  skip_if_not_installed("shinytest2")
  skip_if(Sys.getenv("LIBER_RUN_BROWSER_TESTS") != "true")
  root <- tempfile("liberary-browser-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  app <- LibeRary::library_shiny(catalog = root, launch.browser = NULL)
  driver <- shinytest2::AppDriver$new(
    app, name = "liberary-browser", width = 1366, height = 768,
    load_timeout = 120000, seed = 20260723
  )
  on.exit(driver$stop(), add = TRUE)
  driver$wait_for_idle()
  expect_identical(driver$get_js("document.title"), "LibeRary")
  expect_match(driver$get_js("document.body.innerText"), "models in catalog")
  expect_false(driver$get_js(
    "document.documentElement.scrollWidth > document.documentElement.clientWidth + 2"
  ))
})
