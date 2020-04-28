context("simulations")

test_that("output is of expected type", {
  expect_type(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2)), "double")
  expect_that(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2)), is_a("matrix"))
})

test_that("function generates correct error", {
  expect_error(rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4, 1, 1), ncol = 2)), class = "invalid_dim_error")
})