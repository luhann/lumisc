
data = data.frame(id = rep(1:10, times = 2L), count = rep(1:5, times = 4L))
char = matrix(rep(c("a", "b", "c"), times = 3), nrow = 3)
num_matrix = matrix(1, nrow = 1, ncol = 3)

# test that things error correctly
expect_error(return_proportions(data, NULL), "length zero$")
expect_error(return_proportions(data, "no_in"), class = "invalid_index_error")
expect_error(return_proportions(data, 3L), class = "invalid_index_error")

# returns correct result
expect_equal(nrow(return_proportions(data, "count")), 6L)
expect_equal(nrow(return_proportions(data, NA)), 1L)
