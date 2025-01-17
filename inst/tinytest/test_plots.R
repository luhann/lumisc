# plot is created without error
x = lm(wt ~ mpg, data = mtcars)
y = lm(wt ~ disp, data = mtcars)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot = estimate_plot(list(x, y), c("x", "y"))
  expect_true("ggplot" %in% class(plot), info = "Expect ggplot class")

    plot = estimate_plot(list(x, y), c("x", "y"), coefficient = "mpg")
    expect_true("ggplot" %in% class(plot), info = "Expect ggplot class")
}
