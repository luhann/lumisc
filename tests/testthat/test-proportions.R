context("proportions functions")



test_that("fails with correct error", {
  data = data.frame(id = rep(1:10, times = 2L), count = rep(1:5, times = 4L))

  expect_error(return_proportions(data, NULL), "length zero$")
  expect_error(return_proportions(data, "no_in"), class = "invalid_index_error")
  expect_error(return_proportions(data, 3L), class = "invalid_index_error")
})

test_that("returns correct result", {
  data = data.frame(id = rep(1:10, times = 2L), count = rep(1:5, times = 4L))

  expect_equal(nrow(return_proportions(data, "count")), 6L)
  expect_equal(nrow(return_proportions(data, NA)), 1L)
})