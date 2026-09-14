# proportions tests
df = data.frame(successes = c(10, 10, 10), failures = c(10, 10, 10))

expect_equivalent(proportions(df), df / 60)
expect_equivalent(proportions(df, margin = 1), df / 20)
result_margin2 = proportions(df, margin = 2)
expect_equivalent(as.data.frame(result_margin2), data.frame(successes = rep(1 / 3, 3), failures = rep(1 / 3, 3)))

single = data.frame(x = c(3, 7))
expect_equivalent(proportions(single), single / 10)

result = proportions(df)
expect_true(inherits(result, "data.table"))

# col_search tests
expect_equal(character(0), col_search(iris, "test"))
expect_equal("Sepal.Width", col_search(iris, "Sepal.Width"))
expect_equal("Sepal.Width", col_search(iris, c("Sepal.Width", "test")))
expect_equal(character(0), col_search(iris, c("test", "test2")))
expect_true(length(col_search(iris, "sepal", ignore_case = TRUE)) > 0)
expect_equal(character(0), col_search(iris, "sepal", ignore_case = FALSE))

# get_colref tests
data = data.frame(col1 = 1:10, col2 = 11:20)

expect_true(is.character(get_colref(data, 1L)))
expect_true(is.character(get_colref(data, 1.5)))
expect_true(is.integer(get_colref(data, "col1")))
expect_error(get_colref(data, TRUE), class = "invalid_type_error")
expect_error(get_colref(data, 3L), class = "invalid_index_error")
expect_error(get_colref(data, "col7"), class = "invalid_index_error")
expect_equal(get_colref(data, 2L), "col2")
expect_equal(get_colref(data, "col2"), 2L)

# write_df tests
tmp = tempfile(fileext = ".csv")
on.exit(unlink(tmp), add = TRUE)

df1 = data.frame(a = 1:3, b = c("x", "y", "z"))
df2 = data.frame(a = 4:6, b = c("p", "q", "r"))

write_df(list(df1, df2), tmp, overwrite = TRUE)
expect_true(file.exists(tmp))

lines = readLines(tmp)
expect_true(length(lines) > 0)

write_df(list(df1), tmp, overwrite = TRUE)
expect_true(file.exists(tmp))

# proportions with ...
expect_equivalent(proportions(df, margin = NULL), proportions(df))
expect_equal(proportions(table(c(1, 1, 2))), base::proportions(table(c(1, 1, 2))))
expect_equal(proportions(matrix(1:4, 2), 1), base::proportions(matrix(1:4, 2), 1))
