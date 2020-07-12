#' Mode
#'
#' Returns the mode (number that occurs the most) of a given vector.
#'
#' @param x Vector you wish to evaluate.
#' @name getmode
#' @export
getmode = function(x) {
  UseMethod("getmode")
}

#' @export
getmode.default = function(x) {
  rlang::abort(
    glue::glue("Mode not defined for type: ({typeof(x)})."),
    class = "invalid_type_error",
    val_type = typeof(x)
  )
}

#' @export
getmode.numeric = function(x) {
  ux = unique(x)
  tab = tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#' @export
getmode.factor = getmode.numeric

#' @export
getmode.character = getmode.numeric


#' get_colref
#'
#' Returns the column number that corresponds to a given column name or vice-versa if a number is supplied
#'
#' @param df The dataframe to evaluate
#' @param x The column name (in string format) or column number (integer format) that you wish to evaluate
#' @name get_colref
#' @export
get_colref = function(df, x) {
  UseMethod("get_colref", x)
}

#' @export
get_colref.default = function(df, x) {
  rlang::abort(glue::glue("Can't access column using given type ({typeof(x)})."), class = "invalid_type_error")
}

#' @export
get_colref.numeric = function(df, x) {
  # Coerce all numerics to integer so double inputs still extract a column
  # Following the regular coercion rules
  x = as.integer(x)

  if (x > length(colnames(df)) | x < 1) {
    rlang::abort("Value supplied is outside column range.", class = "invalid_index_error")
  } else {
    return(colnames(df)[x])
  }
}

#' @export
get_colref.character = function(df, x) {
  if (x %in% colnames(df) != TRUE) {
    rlang::abort(glue::glue("Column \"{x}\" not found in supplied dataframe."), class = "invalid_index_error")
  } else {
    return(which(colnames(df) == x))
  }
}

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


#' view
#'
#' Invoke a spreadsheet-style data viewer on a matrix-like R object.
#'
#' @name view
#' @param x An R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @param title Title for viewer window.  Defaults to name of 'x' prefixed by 'Data:'.
#' @export
view = function(x, title) {
  utils::View(x, title)
}

# switchv
#' Vectorized version of switch
#'
#' Vectorized version of [base::switch()]: just loops over
#' input and calls [base::switch()].
#'
#' @param EXPR An expression evaluating to a vector of numbers of strings
#' @param ... List of alternatives
#'
#' @return Vector of returned values.
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
  result = EXPR

  for (i in seq(along = result)) {
    result[i] = switch(EXPR[i], ...)
  }

  result
}


#'
#' Installed version of R/lumisc
#'
#' Print the version number of the currently installed version of R/lumisc.
#'
#' @export
#' @return
#' A character string with the version number of the currently installed
#'   version of R/lumisc.
#'
#' @examples
#' lumiscversion()
#' @keywords
#' print
lumiscversion = function() {
  as.character(utils::packageVersion("lumisc"))
}