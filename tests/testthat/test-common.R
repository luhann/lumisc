context("common functions")


test_that("input length is equal to output length", {
  expect_equal(length(1 %nin% 1:10), 1L)
  expect_equal(length(11 %nin% 1:10), 1L)
  expect_equal(length(-1:3 %nin% 1:10), 5L)
})

test_that("output is of type logical", {
  expect_equal(typeof(1 %nin% 1:10), "logical")
  expect_equal(typeof(11 %nin% 1:10), "logical")
  expect_equal(typeof(NA %nin% 1:10), "logical")
  expect_equal(typeof(NULL %nin% 1:10), "logical")
})