context("proportions")



test_that("fails with correct error", {
  data = data.frame(id = rep(1:10, times = 2L), count = rep(1:5, times = 4L))

  expect_error(return_proportions(data, NULL), "length zero$")
  expect_error(return_proportions(data, "no_in"), "not in dataframe$")
  expect_error(return_proportions(data, 3L), "number not in dataframe$")

})

test_that("returns correct result", {
  data = data.frame(id = rep(1:10, times = 2L), count = rep(1:5, times = 4L))

  expect_equal(nrow(return_proportions(data, "count")), 6L)
  expect_equal(nrow(return_proportions(data, NA)), 1L)
})
