test_that("correct startup options are set", {
  set_startup_options()
  startup = options()
  expect_true(startup[["warnPartialMatchArgs"]])
  expect_true(startup[["warnPartialMatchAttr"]])
  expect_true(startup[["warnPartialMatchDollar"]])
  expect_false(startup[["show.signif.stars"]])
  expect_silent(set_startup_options())
})