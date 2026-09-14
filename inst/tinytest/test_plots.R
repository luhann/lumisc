# plot is created without error
x = lm(wt ~ mpg, data = mtcars)
y = lm(wt ~ disp, data = mtcars)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot = estimate_plot(list(x, y), c("x", "y"))
  expect_true("ggplot" %in% class(plot), info = "Expect ggplot class")

  plot = estimate_plot(list(x, y), c("x", "y"), coefficient = "mpg")
  expect_true("ggplot" %in% class(plot), info = "Expect ggplot class")

  # single model (not a list)
  single_plot = estimate_plot(x)
  expect_true("ggplot" %in% class(single_plot), info = "Expect ggplot class for single model")

  # theme_patroclus
  thm = theme_patroclus()
  expect_inherits(thm, "theme")

  thm_no_ticks = theme_patroclus(ticks = FALSE)
  expect_inherits(thm_no_ticks, "theme")

  named_plot = estimate_plot(list(x = x, y = y))
  expect_equal(sort(unique(named_plot$data$model)), c("x", "y"))
  expect_equal(single_plot$labels$x, "Term")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  expect_error(estimate_plot(list(x, y), c("x", "y")), class = "package_not_installed")
}
