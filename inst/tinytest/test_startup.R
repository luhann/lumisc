# For tinytest
report_side_effects()

# Save original options to restore after testing
orig = options()

set_startup_options()
startup = options()
expect_true(startup[["warnPartialMatchArgs"]])
expect_true(startup[["warnPartialMatchAttr"]])
expect_true(startup[["warnPartialMatchDollar"]])
expect_false(startup[["show.signif.stars"]])
expect_silent(set_startup_options())

# Test that custom arguments override defaults
set_startup_options(digits = 7L, width = 120L)
opts = options()
expect_equal(opts[["digits"]], 7L)
expect_equal(opts[["width"]], 120L)

set_startup_options(bitmapType = "Xlib", error = utils::recover)
expect_equal(getOption("bitmapType"), "Xlib")
expect_identical(getOption("error")[[1]], utils::recover)
set_startup_options(error = NULL)
expect_null(getOption("error"))

# Restore original options
options(orig)
