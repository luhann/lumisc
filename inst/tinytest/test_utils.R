# Create mock data
data = data.frame(col1 = 1:10, col2 = 11:20)

# %!in% tests
expect_true(is.logical(1 %!in% 1:10))
expect_true(is.logical(NA %!in% 1:10))
expect_true(is.logical(NULL %!in% 1:10))

# get_colref tests
expect_true(is.character(get_colref(data, 1L)))
expect_true(is.character(get_colref(data, 1.5)))
expect_true(is.integer(get_colref(data, "col1")))
expect_error(get_colref(data, TRUE), class = "invalid_type_error")
expect_error(get_colref(data, 3L), class = "invalid_index_error")
expect_error(get_colref(data, "col7"), class = "invalid_index_error")

# create list tests
expect_true(is.list(create_list(c("test", "names"))))
expect_true(is.list(create_list(c(1, 2))))

# mode tests
expect_true(is.integer(mode(1:10)))
expect_true(is.character(mode(c("a", "b", "c", "a"))))
expect_true(is.character(mode(c("a", "b", "c", "a"))))
expect_error(mode(c(TRUE, TRUE, FALSE)), class = "invalid_type_error")

# is_date tests
expect_true(is_date(as.Date("2021-10-11")))
expect_false(is_date("2021-10-11"))
expect_error(is_date(as.Date("not a date")))