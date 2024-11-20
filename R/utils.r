#' Mode
#'
#' Returns the mode (number that occurs the most) of a given vector.
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
mode.factor = mode.numeric

#' @export
mode.character = mode.numeric


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
  rlang::abort(paste("Can't access column using given type:", typeof(x)), class = "invalid_type_error")
}

#' @export
get_colref.numeric = function(df, x) {
  # Coerce all numerics to integer so double inputs still extract a column
  # Following the regular coercion rules
  x = as.integer(x)

  if (x > length(colnames(df)) || x < 1) {
    rlang::abort("Value supplied is outside column range.", class = "invalid_index_error")
  } else {
    colnames(df)[x]
  }
}

#' @export
get_colref.character = function(df, x) {
  if (x %in% colnames(df) != TRUE) {
    rlang::abort(paste("Column", x, "not found in supplied dataframe."), class = "invalid_index_error")
  } else {
    which(colnames(df) == x)
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
#' @param title Title for viewer window. Defaults to name of 'x' prefixed by 'Data:'.
#' @export
view = function(x, title) {
  utils::View(x, title)
}

#' view_last
#'
#' Invoke a spreadsheet-style data viewer on the last R object interacted with.
#'
#' @name view_last
#' @param title Title for viewer window. Defaults to name of 'x' prefixed by 'Data:'.
#' @export
view_last = function(title) {
  utils::View(.Last.value, title)
}

#' switchv
#'
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

#' Return vector as quoted character vector
#'
#' Take an existing R vector and add quotes to it for easy manipulation and copy/pasting.
#'
#' @param vec A character or integer vector to be quoted
#' @param collapse An optional character string to separate the results. Not NA_character_.
#' @return An quoted vector of length \code{vec}.
#'
#' @export
quote_vec = function(vec, collapse = ", ") {
  paste('"', vec, '"', collapse = collapse, sep = "")
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

#' Search for column names that match given string
#'
#' @param df Dataframe to search in
#' @param search_string String to search column names for
#' @param ignore_case Should case be ignored in column search. Defaults to TRUE
#' @return A character vector of column names that match search
#'
#' @export
col_search = function(df, search_string, ignore_case = TRUE) {
  grep(search_string, colnames(df), value = TRUE, perl = TRUE, ignore.case = ignore_case)
}

#' Create dir if it doesn't exist
#'
#' If supplied a filepath with multiple directories will create all parent directories
#'
#' @param name Name of directory
#' @export
create_dir = function(name) {
  if (!dir.exists(name)) {
    dir.create(name, recursive = TRUE)
  }
}

#' Make an .rproj file in specified directory
#'
#' @param path The name of the .Rproj file to create
#' @return TRUE if file was successfully created
#' @export
create_rproj = function(path) {
  name = basename(path)

  # if the provided name does not include .Rproj extension add it on
  if (!grepl(r"{\.rproj$}", name, ignore.case = TRUE)) {
    name = paste0(name, ".Rproj")
    if (dirname(path) == ".") {
      path = name
    } else {
      path = paste0(dirname(path), "/", name)
      create_dir(dirname(path))
    }
  }

  if (file.exists(path)) {
    rlang::inform("Rproj file already exists")
    invisible(FALSE)
  } else {
    rproj = c(
      "Version: 1.0",
      NA,
      "RestoreWorkspace: No",
      "SaveWorkspace: No",
      "AlwaysSaveHistory: Default",
      NA,
      "EnableCodeIndexing: Yes"
    )

    data.table::fwrite(list(rproj), file = path)
    invisible(TRUE)
  }
}

#' Return error message and abort if package not found
#'
#' @param package The name of the package that to be installed
check_package = function(package) {
  if (requireNamespace(package, quietly = TRUE)) {
    TRUE
  } else {
    rlang::abort(
      paste(package, "is not installed, please install it to use these functions"),
      class = "package_not_installed"
    )
  }
}