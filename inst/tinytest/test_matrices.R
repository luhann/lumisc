expect_true(is_orthogonal(diag(5)), info = "identity matrix is identified as orthogonal")



expect_error(is_orthogonal(1:9, coerce = FALSE), class = "invalid_type_error")
expect_error(is_orthogonal(1:10, coerce = TRUE), class = "failed_coercion")

mat = matrix(1:100, nrow = 10, ncol = 10)
expect_equal(colMaxs(mat), c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
expect_error(colMaxs(as.character(mat)), class = "invalid_type_error")
