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

# Restore original options
options(orig)