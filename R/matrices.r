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
    glue::glue("Mode not defined for type: ({typeof(x)})."),
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