# test output
expect_true(is.double(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2))))
expect_equal(class(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2))), class(matrix()))

# test for correct error message
expect_error(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4, 1, 1), ncol = 2)), class = "invalid_dim_error")

# test output dimensions
result = rmvn(50, c(1, 2, 3), matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3))
expect_equal(dim(result), c(50L, 3L))

# test with default arguments
result_default = rmvn(100, c(0))
expect_equal(dim(result_default), c(100L, 1L))

# test that empirical mean is roughly correct for large n
set.seed(42)
result_mean = rmvn(10000, c(5, -3), matrix(c(1, 0, 0, 1), nrow = 2))
expect_equal(mean(result_mean[, 1]), 5, tolerance = 0.1)
expect_equal(mean(result_mean[, 2]), -3, tolerance = 0.1)
expect_error(rmvn(10, c(0, 0), matrix(c(1, 0.9, 0, 1), 2)), class = "asymmetric_cov_error")
expect_equal(dim(rmvn(10, c(a = 0, b = 0), matrix(c(1, 0.5, 0.5, 1), 2, dimnames = list(c("a", "b"), NULL)))), c(10L, 2L))
expect_equal(dim(rmvn(10, 0, 1)), c(10L, 1L))
