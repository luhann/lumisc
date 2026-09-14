#' cache_file
#'
#' @param file Path to the cache file.
#' @param expr The expression to evaluate if the cache doesn't exist.
#' @param force Logical; if TRUE, re-run the expression even if the file exists.
#' @param ... Additional arguments passed to saveRDS.
#'
#' @export
cache_file = function(file, expr, force = FALSE, ...) {
  if (file.exists(file) && !force) {
    return(readRDS(file))
  }

  tmp = tempfile(tmpdir = dirname(file))
  on.exit(unlink(tmp))
  saveRDS(expr, tmp, ...)
  file.rename(tmp, file)
  expr
}

#' create_list
#'
#' Create an empty list that is length \code{names} and with each element named by the vector of names provided. This is
#' shorthand for creating an empty list and then assigning it names.
#'
#' @param names A character or integer vector of names to be assigned to list.
#' @return An empty named list of length \code{names}.
#'
#' @export
create_list = function(names) {
  stats::setNames(vector(mode = "list", length = length(names)), names)
}

#' is_date
#'
#' Checks to see if the given vector is one of the two base R date objects (Date, POSIXt)
#'
#' @param x Vector you wish to check is a date.
#' @name is_date
#' @export
is_date = function(x) {
  inherits(x, c("Date", "POSIXt"))
}

#' Mode
#'
#' Returns the mode (number that occurs the most) of a given vector. All tied values are returned, and `NA` counts as
#' a value.
#'
#' @param x Vector you wish to evaluate.
#' @name mode
#' @export
mode = function(x) {
  UseMethod("mode")
}

#' @export
mode.default = function(x) {
  rlang::abort(
    paste("Mode not defined for type:", typeof(x)),
    class = "invalid_type_error",
    val_type = typeof(x)
  )
}

#' @export
mode.numeric = function(x) {
  ux = unique(x)
  tab = tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#' @export
mode.integer = function(x) {
  n_na = if (anyNA(x)) sum(is.na(x)) else 0L
  if (n_na == length(x)) {
    return(mode.numeric(x))
  }
  lo = min(x, na.rm = TRUE)
  hi = max(x, na.rm = TRUE)
  if (hi - as.numeric(lo) >= length(x)) {
    return(mode.numeric(x))
  }
  tab = c(tabulate(x - lo + 1L, hi - lo + 1L), n_na)
  c(lo:hi, NA)[tab == max(tab) & tab > 0L]
}

#' @export
mode.factor = function(x) {
  tab = c(tabulate(x, nlevels(x)), if (anyNA(x)) sum(is.na(x)) else 0L)
  factor(c(levels(x), NA)[tab == max(tab) & tab > 0L], levels = levels(x), ordered = is.ordered(x))
}

#' @export
mode.character = mode.numeric


#' quit
#'
#' Quits R without saving the workspace to an image
#'
#' @name q
#' @param save Default \code{no}. Choose whether or not to save the workspace.
#' @param ... Additional arguments passed to the \code{quit()} function.
#' @aliases quit
#' @export
q = function(save = "no", ...) {
  quit(save = save, ...)
}

#' Return vector as quoted character vector
#'
#' Take an existing R vector and add quotes to it for easy manipulation and copy/pasting.
#'
#' @param vec A character or integer vector to be quoted
#' @param collapse An optional character string to separate the results. Not NA_character_.
#' @return A single string of the escaped, quoted elements of \code{vec}; \code{NA} is left unquoted.
#'
#' @export
quote_vec = function(vec, collapse = ", ") {
  paste(encodeString(vec, quote = '"'), collapse = collapse)
}


#' Return hex value of specified rgb colour
#'
#' Take an RGB colour value and return a hex colour code
#'
#' @param r R channel
#' @param g G channel
#' @param b B channel
#' @return A hex colour code
#'
#' @export
rgb2hex = function(r, g, b) grDevices::rgb(r, g, b, maxColorValue = 255)

#' switchv
#'
#' Vectorized version of switch
#'
#' Vectorized version of [base::switch()]: calls [base::switch()] once per unique
#' value of the input and maps the results back.
#'
#' @param EXPR An expression evaluating to a vector of numbers of strings
#' @param ... List of alternatives
#'
#' @return Vector of returned values, simplified to their common type. Elements with no matching alternative are `NA`.
#'
#' @examples
#' switchv(c("horse", "fish", "cat", "bug"),
#'   horse = "fast",
#'   cat = "cute",
#'   "what?"
#' )
#' @export
#' @author Karl Broman
switchv = function(EXPR, ...) {
  keys = unique(EXPR)
  unlist(lapply(keys, \(x) switch(x, ...) %||% NA)[match(EXPR, keys)]) %||% EXPR[0]
}

#' view
#'
#' Invoke a spreadsheet-style data viewer on a matrix-like R object.
#'
#' @name view
#' @param x An R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @param title Title for viewer window. Defaults to name of 'x' prefixed by 'Data:'.
#' @export
view = function(x, title) {
  if (interactive()) {
    utils::View(x, title)
  }
}

#' view_last
#'
#' Invoke a spreadsheet-style data viewer on the last R object interacted with.
#'
#' @name view_last
#' @param title Title for viewer window. Defaults to name of 'x' prefixed by 'Data:'.
#' @export
view_last = function(title) {
  if (interactive()) {
    utils::View(.Last.value, title)
  }
}
