tmp = tempfile(fileext = ".txt")

writeLines(c("a", "b", "c"), tmp)
expect_equal(count_lines(tmp, chunk = 1), 3)
expect_equal(count_lines(tmp, chunk = 10), 3)
expect_equal(count_lines(tmp), 3)

writeLines(character(0), tmp)
expect_equal(count_lines(tmp), 0)

writeLines(rep("x", 100), tmp)
expect_equal(count_lines(tmp, chunk = 7), 100)

writeBin(charToRaw("a\nb\nc"), tmp)
expect_silent(count_lines(tmp))
expect_equal(count_lines(tmp), 3)

gz = gzfile(tmp, "w")
writeLines(rep("x", 50), gz)
close(gz)
expect_equal(count_lines(tmp), 50)

expect_error(count_lines(tempfile()), "cannot open the connection")

unlink(tmp)
