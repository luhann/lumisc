test_that("plot is created without error", {
  x = lm(wt ~ mpg, data = mtcars)
  y = lm(wt ~ disp, data = mtcars)

  plot = estimate_plot(list(x, y), c("x", "y"))

  expect_s3_class(plot, "ggplot")
})
