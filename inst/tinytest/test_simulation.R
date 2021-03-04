# test output
expect_true(is.double(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2))))
expect_equal(class(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2))), class(matrix()))


# test for correct error message
expect_error(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4, 1, 1), ncol = 2)), class = "invalid_dim_error")
