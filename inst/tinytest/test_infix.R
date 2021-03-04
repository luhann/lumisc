data = data.frame(col1 = 1:10, col2 = 11:20)

expect_true(is.logical(1 %!in% 1:10))
expect_true(is.logical(NA %!in% 1:10))
expect_true(is.logical(NULL %!in% 1:10))

expect_true(is.character(1 %cat% 2))
expect_true(is.character(NULL %cat% 2))


expect_equal(length(1 %!in% 1:10), 1L)
expect_equal(length(-1:3 %!in% 1:10), 5L)
expect_equal(length(NA %!in% 1:10), 1L)


expect_equal(1 %||% 2, 1)
expect_equal(NULL %||% 2, 2)
expect_equal(NA %||% 2, NA)
