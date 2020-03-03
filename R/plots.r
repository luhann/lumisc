#' Return a grid of plots that share a legend
#'
#' Creates a n x n grid of plots that all share the same legend using grid.arrange.
#'
#' @param ... The ggplot2 objects to plot
#' @param ncol The number of columns to arrange the plots into.
#' @param nrow The number of rows to arrange the plots into.
#' @param position Where to place the shared legend in the grid of plots, either at the bottom or right.
#' @param plot_legend Which plot to pull the legend element from, defaults to the first provided plot.
#' @return An invisible plot containing all supplied ggplots, as well as a shared legend located at `position`.
#' @export
grid_arrange_shared_legend = function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right"), plot_legend = 1) {

  plots = list(...)
  position = match.arg(position)
  g = ggplot2::ggplotGrob(plots[[plot_legend]] + ggplot2::theme(legend.position = position))$grobs
  legend = g[[which(sapply(g, function(x){ x$name}) == "guide-box")]]
  lheight = sum(legend$height)
  lwidth = sum(legend$width)
  gl = lapply(plots, function(x){ x + ggplot2::theme(legend.position = "none")})
  gl = c(gl, ncol = ncol, nrow = nrow)

  combined = switch(position,
                    "bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                                        legend,
                                                        ncol = 1,
                                                        heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)),
                    "right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                                      legend,
                                                      ncol = 2,
                                                      widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)))

  grid::grid.newpage()
  grid::grid.draw(combined)

  # return gtable invisibly
  return(combined)

}



#' Return a plot of a list of models estimates and their intervals
#'
#' Creates a scatter plot with one point for each estimate and lineranges for their confidence intervals. `model_names` is implicitly converted to numeric to provide a sensible order for the provided models on the x-axis.
#'
#' @importFrom rlang .data
#' @param model_list A list of statistical model objects.
#' @param model_names A character vector containing the list of model names.
#' @param coefficient The model term to be compared across all models.
#' @return An invisible plot containing all model estimates as a ggplot.
#' @export
estimate_plot = function(model_list, model_names, coefficient = NULL) {
  model_list = lapply(model_list, broom::tidy, conf.int = TRUE)

  data.table::setattr(model_list, 'names', model_names)
  model_table = data.table::rbindlist(model_list, use.names = TRUE, idcol = "model")

  data.table::set(model_table, j = "model", value = as.numeric(model_table[["model"]]))

  if (!is.null(coefficient)) {
      model_table = model_table[.data$term == coefficient, ]
  }

  # Now to make a nice plot
  p = ggplot2::ggplot(ggplot2::aes(x = .data$model, y = .data$estimate), data = model_table) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high), alpha = 0.8) +
    ggplot2::labs(x = "Model", y = "Estimate", colour = "Term") + 
    ggplot2::theme_minimal()

  return(p)
}
