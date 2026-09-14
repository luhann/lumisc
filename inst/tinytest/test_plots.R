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
  expect_inherits(thm_no_ticks$axis.ticks, "ggplot2::element_blank")
  expect_inherits(thm$axis.ticks, "ggplot2::element_line")

  dark = theme_patroclus(mode = "dark")
  expect_equal(dark$plot.background$fill, "#000000")
  expect_equal(dark$palette.colour.discrete[1], "#cf7d55")

  named_plot = estimate_plot(list(x = x, y = y))
  expect_equal(sort(unique(named_plot$data$model)), c("x", "y"))
  expect_equal(single_plot$labels$x, "Term")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  expect_error(estimate_plot(list(x, y), c("x", "y")), class = "package_not_installed")
}
