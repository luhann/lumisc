expect_true(is_orthogonal(diag(5)), info = "identity matrix is identified as orthogonal")



expect_error(is_orthogonal(1:9, coerce = FALSE), class = "invalid_type_error")
expect_error(is_orthogonal(1:10, coerce = TRUE), class = "failed_coercion")
