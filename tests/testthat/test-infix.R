context("infix")

test_that("output is of correct type", {
  data = data.frame(col1 = 1:10, col2 = 11:20)

  expect_type(1 %!in% 1:10, "logical")
  expect_type(NA %!in% 1:10, "logical")
  expect_type(NULL %!in% 1:10, "logical")

  expect_type(1 %cat% 2, "character")
  expect_type(NULL %cat% 2, "character")
})

test_that("input length is equal to output length", {
  expect_equal(length(1 %!in% 1:10), 1L)
  expect_equal(length(-1:3 %!in% 1:10), 5L)
  expect_equal(length(NA %!in% 1:10), 1L)
})

test_that("nil", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_equal(NA %||% 2, NA)
})