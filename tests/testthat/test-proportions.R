test_that("fails with correct error", {
  data = data.frame(id = rep(1:10, times = 2L), count = rep(1:5, times = 4L))
  char = matrix(rep(c("a", "b", "c"), times = 3), nrow = 3)

  expect_error(return_proportions(data, NULL), "length zero$")
  expect_error(return_proportions(data, "no_in"), class = "invalid_index_error")
  expect_error(return_proportions(data, 3L), class = "invalid_index_error")

  expect_error(diversity(char, index = "shannon"), class = "invalid_type_error")
})

test_that("displays correct warnings", {
  expect_message(diversity(1:9, index = "shannon"), regexp = "coerce to matrix")
})

test_that("returns correct result", {
  data = data.frame(id = rep(1:10, times = 2L), count = rep(1:5, times = 4L))
  num_matrix = matrix(1, nrow = 1, ncol = 3)

  expect_equal(nrow(return_proportions(data, "count")), 6L)
  expect_equal(nrow(return_proportions(data, NA)), 1L)
  expect_equal(diversity(num_matrix, index = "shannon"), -sum(num_matrix / rowSums(num_matrix) * log(num_matrix / rowSums(num_matrix))))
})