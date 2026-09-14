#' Return a plot of a list of models estimates and their intervals
#'
#' Creates a scatter plot with one point for each estimate and lineranges for their confidence intervals.
#'
#' @name estimate_plot
#' @importFrom rlang .data
#' @param model A list of statistical model objects, or a single model.
#' @param model_names A character vector of model names. Defaults to the names of `model`.
#' @param coefficient The model term to be compared across all models.
#' @param ... Additional arguments passed to broom::tidy.
#' @return A ggplot of all model estimates
#' @export
estimate_plot = function(model, model_names = names(model), coefficient = NULL, ...) {
  # bind term to null to pass R CMD CHECK for data.table
  term = NULL

  check_package("ggplot2")
  check_package("broom")

  single = !inherits(model, "list")

  model_table = if (single) {
    broom::tidy(model, conf.int = TRUE, ...)
  } else {
    lapply(model, broom::tidy, conf.int = TRUE, ...) |>
      stats::setNames(model_names) |>
      data.table::rbindlist(use.names = TRUE, idcol = "model")
  }

  if (!is.null(coefficient)) {
    model_table = subset(model_table, term %in% coefficient)
  }

  mapping = if (single) {
    ggplot2::aes(x = as.factor(.data$term), y = .data$estimate)
  } else {
    ggplot2::aes(x = as.factor(.data$model), y = .data$estimate, colour = .data$term)
  }

  ggplot2::ggplot(model_table, mapping) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high), alpha = 0.8) +
    ggplot2::labs(x = if (single) "Term" else "Model", y = "Estimate", colour = "Term") +
    theme_patroclus()
}


#' Patroclus Theme
#'
#' A minimal, Tufte-esque ggplot2 theme using the screen palettes and typefaces of the Patroclus design system: EB
#' Garamond for titles, IBM Plex Sans for axis titles, legends and strips, and IBM Plex Mono for tick labels. Discrete
#' colour and fill scales default to copper, steel blue and alizarin.
#'
#' @note
#' The fonts must be installed and visible to the graphics device (e.g. via systemfonts with ragg); otherwise the device
#' falls back to its default family.
#'
#' @param base_size Base font size, given in pts.
#' @param mode Either "light" or "dark" palette.
#' @param ticks \code{logical} Show axis ticks?
#'
#' @export
theme_patroclus = function(base_size = 11, mode = c("light", "dark"), ticks = TRUE) {
  check_package("ggplot2")

  pal = patroclus[[match.arg(mode)]]
  fonts = patroclus$fonts
  label = ggplot2::element_text(family = fonts[["sans"]], colour = pal[["muted"]])
  accents = unname(pal[c("accent", "accent_cool", "accent_meta", "muted")])

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = fonts[["serif"]],
    header_family = fonts[["serif"]],
    ink = pal[["text"]],
    paper = pal[["bg"]],
    accent = pal[["accent"]]
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(colour = pal[["headline"]]),
      axis.title = label,
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.text = ggplot2::element_text(family = fonts[["mono"]], colour = pal[["muted"]]),
      axis.line.x = ggplot2::element_line(colour = pal[["line"]], linewidth = 0.1, lineend = "butt"),
      axis.ticks = if (ticks) ggplot2::element_line(colour = pal[["line"]]) else ggplot2::element_blank(),
      legend.title = label,
      legend.text = label,
      legend.position = "bottom",
      legend.box = "vertical",
      strip.text = label,
      panel.grid = ggplot2::element_blank(),
      palette.colour.discrete = accents,
      palette.fill.discrete = accents
    )
}
