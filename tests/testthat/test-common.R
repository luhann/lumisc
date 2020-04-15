context("common functions")



test_that("input length is equal to output length", {
  expect_equal(length(1 %!in% 1:10), 1L)
  expect_equal(length(-1:3 %!in% 1:10), 5L)
  expect_equal(length(NA %!in% 1:10), 1L)
})

test_that("output is of type logical", {
  expect_equal(typeof(1 %!in% 1:10), "logical")
  expect_equal(typeof(NA %!in% 1:10), "logical")
  expect_equal(typeof(NULL %!in% 1:10), "logical")
})

test_that("output is same type as input", {
  expect_type(getmode(1:10), "integer")
  expect_type(getmode(c("a", "b", "c", "a")), "character")
  expect_type(getmode(c("a", "b", "c", "a")), "character")
})

test_that("function doesn't work with unexpected type", {
  data = data.frame(col1 = 1:10, col2 = 11:20)

  expect_error(getmode(c(TRUE, TRUE, FALSE)), class = "invalid_type_error")
  expect_error(get_colref(data, TRUE), class = "invalid_type_error")
})


test_that("returns correct type", {
  data = data.frame(col1 = 1:10, col2 = 11:20)

  expect_type(get_colref(data, 1L), "character")
  expect_type(get_colref(data, 1.5), "character")
  expect_type(get_colref(data, "col1"), "integer")
})

test_that("stops on invalid input", {
  data = data.frame(col1 = 1:10, col2 = 11:20)

  expect_error(get_colref(data, 3L), class = "invalid_index_error")
  expect_error(get_colref(data, "col7"), class = "invalid_index_error")
})

test_that("nil", {  
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_equal(NA %||% 2, NA)
})