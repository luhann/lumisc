test_that("output is of correct type", {
  data = data.frame(col1 = 1:10, col2 = 11:20)

  expect_type(1 %!in% 1:10, "logical")
  expect_type(NA %!in% 1:10, "logical")
  expect_type(NULL %!in% 1:10, "logical")

  expect_type(get_colref(data, 1L), "character")
  expect_type(get_colref(data, 1.5), "character")
  expect_type(get_colref(data, "col1"), "integer")

  expect_type(lumiscversion(), "character")

expect_type(create_list(c("test", "names")), "list")
expect_type(create_list(c(1, 2)), "list")
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

test_that("stops on invalid input", {
  data = data.frame(col1 = 1:10, col2 = 11:20)

  expect_error(get_colref(data, 3L), class = "invalid_index_error")
  expect_error(get_colref(data, "col7"), class = "invalid_index_error")
})