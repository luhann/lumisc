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