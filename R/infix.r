#' Inverse Value Matching
#'
#' Complement of \code{\%in\%}. Returns the elements of \code{x} that are
#' not in \code{y}.
#'
#' @name nin
#' @param x The values to be matched. Long vectors are supported.
#' @param table Vector or ‘NULL’: the values to be matched against. Long vectors are not supported.
#' @aliases %!in%
#' @export
#' @examples
#' 1 %!in% 1:10
#' 1 %!in% 2:10
#' 11 %!in% 1:10
`%!in%` = function(x, table) !`%in%`(x, table)

#' Default value for NULL
#'
#' Return \code{lhs} if \code{lhs} is not \code{NULL} otherwise returns
#' \code{rhs}
#' @param lhs An object to set if it's null
#' @param rhs The value to provide if x is null
#' @name nil
#' @aliases %||%
#' @export
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` = function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}

#' quick concat
#'
#' quickly concatenate two variables
#' @param x Variable to concatenate first
#' @param y Variable to concatenate second
#' @name concat
#' @aliases %cat%
#' @export
#' @examples
#' 1 %cat% 2
`%cat%` = function(x, y) {
  paste0(x, y)
}