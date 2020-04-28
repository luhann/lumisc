context("r startup")

test_that("correct startup options are set", {
  defaults = options()
  set_startup_options()
  startup = options()
  expect_equal(names(startup), names(defaults))
  expect_silent(set_startup_options())
})