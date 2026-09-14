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
lst = create_list(c("a", "b", "c"))
expect_equal(names(lst), c("a", "b", "c"))
expect_equal(length(lst), 3L)
expect_equal(length(create_list(character(0))), 0L)

# mode tests
expect_true(is.integer(mode(1:10)))
expect_true(is.character(mode(c("a", "b", "c", "a"))))
expect_error(mode(c(TRUE, TRUE, FALSE)), class = "invalid_type_error")
expect_equal(mode(c(1, 1, 2, 3)), 1)
expect_equal(sort(mode(c(1, 1, 2, 2, 3))), c(1, 2))
expect_equal(mode(c("a", "b", "c", "a")), "a")
expect_equal(mode(factor(c("a", "b", "a"))), factor("a", levels = c("a", "b")))
expect_equal(mode(c(3L, 1L, 3L, 1L, 2L)), c(1L, 3L))
expect_equal(mode(c(NA, NA, 5L)), NA_integer_)
expect_equal(mode(c(NA, 5L, 5L, -2L)), 5L)
expect_equal(mode(c(1L, 1L, 1e9L)), 1L)
expect_equal(mode(c(NA_integer_, NA_integer_)), NA_integer_)
expect_equal(mode(c(-.Machine$integer.max, .Machine$integer.max, 7L, 7L)), 7L)
expect_silent(mode(rep(-.Machine$integer.max, 3L)))
expect_equal(mode(rep(-.Machine$integer.max, 3L)), -.Machine$integer.max)
expect_equal(mode(factor(c("a", NA, NA, "b"))), factor(NA, levels = c("a", "b")))
expect_equal(mode(factor(c("b", "a", "b", "a"), levels = c("b", "a", "c"))), factor(c("b", "a"), levels = c("b", "a", "c")))
expect_inherits(mode(factor(c("lo", "hi", "hi"), ordered = TRUE)), "ordered")
expect_equal(length(mode(factor(character(0)))), 0L)
set.seed(1)
ints = sample(1000L, 1e4, replace = TRUE)
expect_equal(mode(ints), sort(mode(as.numeric(ints))))

# is_date tests
expect_true(is_date(as.Date("2021-10-11")))
expect_true(is_date(as.POSIXct("2021-10-11")))
expect_true(is_date(as.POSIXlt("2021-10-11")))
expect_false(is_date("2021-10-11"))
expect_false(is_date(42))

# switchv tests
expect_equal(
  switchv(c("horse", "fish", "cat", "bug"), horse = "fast", cat = "cute", "what?"),
  c("fast", "what?", "cute", "what?")
)
expect_equal(switchv(character(0), a = 1), character(0))
expect_equal(switchv(c("a", "z"), a = "x"), c("x", NA))
expect_equal(switchv(c("a", "b"), a = 1, b = 2.5), c(1, 2.5))
expect_equal(switchv(c("a", "b", "c"), a = , b = "ab", "other"), c("ab", "ab", "other"))
expect_equal(switchv(c("b", "a", "b", NA), a = 1, b = 2), c(2, 1, 2, NA))

# quote_vec tests
expect_equal(1L, length(quote_vec(letters)))
expect_equal(quote_vec(c("a", "b")), '"a", "b"')
expect_equal(quote_vec(c("a", "b"), collapse = "; "), '"a"; "b"')
expect_equal(quote_vec(c('a"b', NA)), '"a\\"b", NA')

# rgb2hex tests
expect_equal("#FFFFFF", rgb2hex(255, 255, 255))
expect_equal("#000000", rgb2hex(0, 0, 0))
expect_equal("#FF0000", rgb2hex(255, 0, 0))

# col_search tests
expect_equal(character(0), col_search(iris, "test"))
expect_equal("Sepal.Width", col_search(iris, "Sepal.Width"))
expect_equal("Sepal.Width", col_search(iris, c("Sepal.Width", "test")))
expect_equal(character(0), col_search(iris, c("test", "test2")))

# check_package tests
expect_error(lumisc:::check_package("pkg_doesnt_exist"), class = "package_not_installed")

# write files tests
tmp = tempdir()
on.exit(unlink(tmp))

create_lintr(file.path(tmp))
create_rproj(file.path(tmp, "tmp"))
expect_true(file.exists(file.path(tmp, ".lintr")))
expect_true(file.exists(file.path(tmp, "tmp.Rproj")))

tmp_file = file.path(tmp, "test_data.rds")
on.exit(unlink(tmp_file))

# cache_file tests
result = cache_file(tmp_file, mtcars[1:5, ])

expect_true(file.exists(tmp_file))
expect_equal(nrow(result), 5)
expect_inherits(result, "data.frame")

cached_result = cache_file(tmp_file, stop("Cache failed: Expression was re-evaluated!"))
expect_equal(cached_result, result)

new_data = mtcars[1:2, ]
forced_result = cache_file(tmp_file, new_data, force = TRUE)

expect_equal(nrow(forced_result), 2)
expect_equal(nrow(readRDS(tmp_file)), 2)
if (lumisc:::zstd_available()) {
  expect_equal(readBin(tmp_file, "raw", 4), as.raw(c(0x28, 0xb5, 0x2f, 0xfd)))
}
gz_file = file.path(tmp, "test_gzip.rds")
cache_file(gz_file, 1:3, compress = TRUE)
expect_equal(readBin(gz_file, "raw", 2), as.raw(c(0x1f, 0x8b)))
expect_equal(readRDS(gz_file), 1:3)
unlink(gz_file)

# cache_file evaluates in calling environment
local_val = 42
cache_env_result = cache_file(
  file.path(tmp, "test_env.rds"),
  local_val * 2
)
expect_equal(cache_env_result, 84)

cache_dir = tempfile()
dir.create(cache_dir)
cache_file(file.path(cache_dir, "x.rds"), 1)
expect_equal(list.files(cache_dir), "x.rds")
unlink(cache_dir, recursive = TRUE)
on.exit(unlink(file.path(tmp, "test_env.rds")), add = TRUE)
