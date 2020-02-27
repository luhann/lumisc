#' Return a grid of plots that share a legend
#'
#' Creates a n x n grid of plots that all share the same legend using grid.arrange.
#'
#' @param ... The ggplot2 objects to plot
#' @param ncol The number of columns to arrange the plots into.
#' @param nrow The number of rows to arrange the plots into.
#' @param position Where to place the shared legend in the grid of plots, either at the bottom or right.
#' @param plot_legend Which plot to pull the legend element from, defaults to the first provided plot.
#' @return An invisible plot containing all supplied ggplots, as well as a shared legend located at [position].
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
  invisible(combined)

}



#' Return a plot of a list of models estimates and their intervals
#'
#' Creates a scatter plot with one point for each estimate and lineranges for their confidence intervals
#'
#' @param model_list A list of statistical model objects.
#' @param model_names A character vector containing the list of model names.
#' @param term The model term to be compared across all models.
#' @return An invisible plot containing all model estimates as a ggplot.
#' @export
estimate_plot = function(model_list, model_names, term = NULL) {
  if (!("broom" %in% rownames(installed.packages()))) {
    stop("This function only works with broom! Please install the broom package.")
  }

  if (!("data.table" %in% rownames(installed.packages()))) {
    stop("This function only works with data.table! Please install the data.table package.")
  }

  if (!("ggplot2" %in% rownames(installed.packages()))) {
    stop("This function uses ggplot2 for plots! Please install the ggplot2 package.")
  }

  model_list = lapply(model_list, broom::tidy)

  data.table::setattr(model_list, 'names', model_names)
  model_table = data.table::rbindlist(model_list, use.names = TRUE, idcol = "model")

  model_table[, model := as.numeric(model)]
  if (!is.null(term)) {
      model_table[term = term]
  }

  # Now to make a nice plot
  p = ggplot2::ggplot(aes(x = model, y = estimate), data = model_table) +
    ggplot2::geom_pointrange(aes(ymin = conf.low, ymax = conf.high), col = "#2789f2", alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 1, linetype= 4, color = "black", size = 0.5, alpha = 0.8) +
    ggplot2::scale_y_continuous(breaks = 0:10) +
    ggplot2::labs(x = "Timeperiod", y = "Conditional Odds Ratio") +
    ggplot2::theme_minimal()

  return(p)
}
