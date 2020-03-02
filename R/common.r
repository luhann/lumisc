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
#'@rdname getmode
getmode = function(x) {
  uniqx = unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#' get_colref
#'
#' Returns the column number that corresponds to a given column name or vice-versa if a number is supplied
#'
#' @param df The dataframe to evaluate
#' @param x The column name (in string format) or column number (integer format) that you wish to evaluate
#' @rdname get_colref
get_colref = function(df, x) {
  if (is.integer(x) == TRUE) {
    if (x > length(colnames(df)) | x < 1) {
      stop("Value supplied is outside column range.")
    } else {
      return(colnames(df)[x])
    }

  } else if (is.character(x) == TRUE) {
    if (x %in% colnames(df) != TRUE) {
      stop("Column name not in supplied dataframe")
    } else {
      return(which(colnames(df)==x))
    }
  } else stop("Invalid Type")
}
