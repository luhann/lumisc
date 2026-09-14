tmp = tempfile(fileext = ".txt")
on.exit(unlink(tmp))

writeLines(c("a", "b", "c"), tmp)
expect_equal(count_lines(tmp, batch = 1), 3L)
expect_equal(count_lines(tmp, batch = 10), 3L)
expect_equal(count_lines(tmp, batch = 2), 3L)

writeLines(character(0), tmp)
expect_equal(count_lines(tmp, batch = 10), 0L)

writeLines(rep("x", 100), tmp)
expect_equal(count_lines(tmp, batch = 1), 100L)
expect_equal(count_lines(tmp, batch = 50), 100L)

expect_error(count_lines(tempfile(), batch = 10), "cannot open the connection")