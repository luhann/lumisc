# declare matrix
mat = matrix(1:100, nrow = 10, ncol = 10)
ns_mat = matrix(1:100, nrow = 50, ncol = 2)

# make sure the function produces the correct output
expect_true(is_orthogonal(diag(5)), info = "identity matrix is identified as orthogonal")
expect_false(is_orthogonal(diag(5) + 1), info = "identity matrix + 1 is not orthogonal")
expect_error(is_orthogonal("test", coerce = FALSE), class = "invalid_type_error")
expect_error(is_orthogonal(1:9), class = "invalid_type_error")
expect_warning(is_orthogonal(ns_mat), class = "non_square_matrix")
# test coerce
expect_error(is_orthogonal(1:10, coerce = TRUE), class = "failed_coercion")
expect_false(is_orthogonal(1:25, coerce = TRUE), info = "matrix is coerced but not orthogonal")
expect_true(is_orthogonal(as.numeric(diag(5)), coerce = TRUE), info = "matrix is coerced and orthogonal")

# test colMax
expect_equal(colMaxs(mat), c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
expect_error(colMaxs(as.character(mat)), class = "invalid_type_error")

# test colVars
# for this matrix variances should all be the same
expect_equal(colVars(mat), rep(stats::var(mat[, 1]), times = ncol(mat)))
expect_error(colVars(as.character(mat)), class = "invalid_type_error")

# test splitn
# with the defaults
expect_equal(splitn(mat)[[1]], mat[1, , drop = FALSE])
# with custom options
expect_equal(splitn(mat, r = 2)[[1]], mat[1:2, , drop = FALSE])
expect_equal(splitn(mat, c = 5)[[1]], mat[1, 1:5, drop = FALSE])
expect_error(splitn(as.character(mat)), class = "invalid_type_error")
