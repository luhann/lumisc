#' Inverse Value Matching
#'
#'
#' Complement of \code{\%in\%}. Returns the elements of \code{x} that are
#' not in \code{y}.
#'
#' @usage x %in% c(x, y, z)
#' @examples
#'  1 %nin% 1:10
#'  1 %nin% 2:10
#' 11 %nin% 1:10
#' @export
#' @rdname nin
`%nin%` = Negate(`%in%`)


#' Mode
#'
#' Returns the mode (number that occurs the most) of a given vector.
#'
#' @param x Vector you wish to evaluate.
#' @rdname getmode
#' @export 
getmode = function(x) {
  UseMethod("getmode")
}

#' @export 
getmode.default = function(x) {
  stop("I'm not sure what to do with type ", typeof(x))
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
#' @rdname get_colref
#' @export 
get_colref = function(df, x) {
  UseMethod("get_colref", x)
}

#' @export 
get_colref.default = function(df, x) {
  stop("Invalid column type")
}

#' @export 
get_colref.numeric = function(df, x) {
  if (x > length(colnames(df)) | x < 1) {
      stop("Value supplied is outside column range")
    } else {
      return(colnames(df)[x])
    }

}

#' @export 
get_colref.character = function(df, x) {
if (x %in% colnames(df) != TRUE) {
      stop("Column name not in supplied dataframe")
    } else {
      return(which(colnames(df)==x))
    }
}
