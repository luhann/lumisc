#' Return a plot of a list of models estimates and their intervals
#'
#' Creates a scatter plot with one point for each estimate and lineranges for their confidence intervals.
#'
#' @name estimate_plot
#' @importFrom rlang .data
#' @param model_list A list of statistical model objects.
#' @param model_names A character vector containing the list of model names.
#' @param coefficient The model term to be compared across all models.
#' @return An invisible plot containing all model estimates as a ggplot
#' @export
estimate_plot = function(model_list, model_names, coefficient = NULL) {
  # bind term to null to pass R CMD CHECK for data.table
  term = NULL

  check_package("ggplot2")
  check_package("broom")

  model_list = lapply(model_list, broom::tidy, conf.int = TRUE)

  data.table::setattr(model_list, "names", model_names)
  model_table = data.table::rbindlist(model_list, use.names = TRUE, idcol = "model")

  if (!is.null(coefficient)) {
    model_table = subset(model_table, term == coefficient)
  }

  # Now to make a nice plot
  p = ggplot2::ggplot(
    ggplot2::aes(x = as.factor(.data$model), y = .data$estimate, colour = .data$term),
    data = model_table
  ) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high), alpha = 0.8) +
    ggplot2::labs(x = "Model", y = "Estimate", colour = "Term") +
    ggplot2::theme_minimal()

  invisible(p)
}


#' Tufte Maximal Data, Minimal Ink Theme With a Twist
#'
#' Theme based on Chapter 6 'Data-Ink Maximization and Graphical
#' Design' of Edward Tufte *The Visual Display of Quantitative
#' Information*. No border, no axis lines, no grids. This theme works
#' best in combination with \code{geom_rug} or
#' \code{geom_rangeframe}.
#'
#' @note
#' The default font family is set to 'serif' as he uses serif fonts
#' for labels in 'The Visual Display of Quantitative Information'.
#' The serif font used by Tufte in his books is a variant of Bembo,
#' while the sans serif font is Gill Sans. If these fonts are
#' installed on your system, then you can use them with the package
#' \bold{extrafont}.
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ticks \code{logical} Show axis ticks?
#'
#' @references Tufte, Edward R. (2001) The Visual Display of
#' Quantitative Information, Chapter 6.
#'
#' @export
theme_patroclus = function(base_size = 11, base_family = "serif", ticks = TRUE) {
  check_package("ggplot2")

  ret = ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.line = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(linewidth = 0.1, lineend = "butt")
    )
  if (!ticks) {
    ret = ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
  }
  ret
}
