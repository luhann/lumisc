#' is_orthogonal
#'
#' Returns TRUE/FALSE is a matrix is orthogonal.
#'
#' @param x Square/rectangular matrix you wish to evaluate.
#' @param coerce Boolean indicating whether to attempt to coerce the input into a square matrix
#' @name is_orthogonal
#' @export
is_orthogonal = function(x, coerce = FALSE) {
  UseMethod("is_orthogonal")
}

#' @export
is_orthogonal.default = function(x, coerce = FALSE) {
  rlang::abort(
    glue::glue("Invalid type: ({typeof(x)})."),
    class = "invalid_type_error",
    val_type = typeof(x)
  )
}

#' @export
is_orthogonal.matrix = function(x, coerce = FALSE) {
  if (nrow(x) != ncol(x)) {
    rlang::warn(
      glue::glue("Input matrix is not square and cannot be orthogonal."),
      class = "non_square_matrix"
    )

    return(FALSE)
  }

  if (sum(x %*% t(x) == diag(nrow(x))) == length(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @export
is_orthogonal.numeric = function(x, coerce = FALSE) {
  if (coerce == FALSE) {
    rlang::abort(
      glue::glue("Input is not a matrix.\n Use coerce = TRUE to attempt to coerce input into a square matrix."),
      class = "invalid_type_error"
    )

    return(FALSE)
  }

  if (coerce == TRUE) {
    if (sqrt(length(x)) %% 1 != 0) {
      rlang::abort(
        glue::glue("Vector length cannot be coerced to square matrix"),
        class = "failed_coercion"
      )
    }
  }

  x_matrix = matrix(x, nrow = sqrt(length(x)))

  if (sum(x_matrix %*% t(x_matrix) == diag(nrow(x_matrix))) == length(x_matrix)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' colVars
#'
#' Computes the column wise variances on a given matrix.
#'
#' @param mat Matrix containing numeric values
#' @param coerce Boolean indicating whether to attempt to coerce the input into a matrix
#' @param ... Additional parameters to be passed to variance function
#' @name colVars
#' @export
colVars = function(mat, coerce = FALSE, ...) {
  UseMethod("colVars")
}

#' @export
colVars.default = function(mat, coerce = FALSE, ...) {
  rlang::abort(
    glue::glue("Invalid type: ({typeof(x)})."),
    class = "invalid_type_error",
    val_type = typeof(mat)
  )
}

#' @export
colVars.matrix = function(mat, coerce = FALSE, ...) {
  apply(mat, MARGIN = 2, stats::var, ...)
}

#' splitn
#'
#' Splits a given matrix into a list of matrices (byrow) into a list of submatrices
#'
#' @param mat Matrix containing numeric values
#' @param r Rows in each submatrix
#' @param c Cols in each submatrix
#' @name splitn
#' @export
splitn = function(mat, r = 1, c = ncol(mat)) {
  UseMethod("splitn")
}

#' @export
splitn.default = function(mat, r = 1, c = ncol(mat)) {
  rlang::abort(
    glue::glue("Splitn not defined for type: ({typeof(x)})."),
    class = "invalid_type_error",
    val_type = typeof(mat)
  )
}

#' @export
splitn.matrix = function(mat, r = 1, c = ncol(mat)) {
  mat = lapply(
    split(mat, interaction((row(mat) - 1) %/% r + 1, (col(mat) - 1) %/% c + 1)),
    function(x) {
      dim(x) = c(r, c)
      x
    }
  )

  names(mat) = NULL

  return(mat)
}
