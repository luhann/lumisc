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

# test numerical tolerance: a rotation matrix is orthogonal but has floating-point error
theta = pi / 4
rot = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
expect_true(is_orthogonal(rot), info = "rotation matrix is orthogonal with tolerance")

# test near-orthogonal matrix that is not quite orthogonal
near = diag(5) + 1e-4
expect_false(is_orthogonal(near), info = "near-identity is not orthogonal")

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

# test splitn dimensions
splits = splitn(mat, r = 5, c = 5)
expect_equal(length(splits), 4L)
expect_equal(dim(splits[[1]]), c(5L, 5L))

# test colMaxs / colVars with NA
mat_na = matrix(c(1, NA, 3, 4, 5, 6), nrow = 2)
expect_true(is.na(colMaxs(mat_na)[1]))
expect_true(is.na(colVars(mat_na)[1]))
expect_equal(colVars(mat_na, na.rm = TRUE), apply(mat_na, 2, stats::var, na.rm = TRUE))

set.seed(1)
big = matrix(rnorm(200, mean = 1e8), 20)
expect_equal(colVars(big), apply(big, 2, stats::var))
