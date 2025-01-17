# proportions tests
df = data.frame(successes = c(10, 10, 10), failures = c(10, 10, 10))

expect_equivalent(proportions(df), df / 60)
expect_equivalent(proportions(df, margin = 1), df / 20)
