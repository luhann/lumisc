#' Return group proportions
#'
#' Give proportions and counts for each category of a given group in a long form dataset.
#'
#' @param dataframe The dataframe containing all longitudinal measures.
#' @param variable The variable you want to return the category proportions for.
#' @param precision The number of decimal points to round to.
#' @return A datafram with one entry per category of the variable and its proportion.
#' @export
return_proportions = function(dataframe, variable, precision = 2) {
  if (!variable %in% colnames(dataframe) & typeof(variable) == "character") {
    rlang::abort(glue::glue("Column \"{variable}\" not found in dataframe."), class = "invalid_index_error")
  }

  if (!variable %in% seq_len(ncol(dataframe)) & typeof(variable) == "integer") {
    rlang::abort(glue::glue("Column number {variable} not found in dataframe."), class = "invalid_index_error")
  }

  values = c(names(table(dataframe[[variable]])), "NA")
  count = as.numeric(table(dataframe[[variable]], useNA = "always"))
  prop = round(prop.table(count), precision)

  out = data.table::data.table(
    values = values,
    count = count,
    prop = prop
  )

  return(out)
}

#' Diversity
#'
#' Calculate a number of diversity indices
#'
#' This function assumes that rows are independent samples and columns are counts of differing species. For some
#' diversity measures the input matrix must be positive and finite, if the output is NaN this is likely the issue.
#'
#' @name diversity
#' @param matrix The matrix containing counts
#' @param index Which diversity index to calculat
#' @return A vector of indices for each row of the matrix provided
#' @export
diversity = function(matrix, index = "shannon") {
  if (!is.matrix(matrix)) {
    rlang::inform("Input not a matrix, attempting to coerce to matrix.", class = "coercion")

    if (is.vector(matrix)) {
      dim(matrix) = c(1, length(matrix))
    } else {
      matrix = as.matrix(matrix)
    }
  }

  if (!is.numeric(matrix)) {
    rlang::abort("Input matrix must be numeric.", class = "invalid_type_error")
  }

  if (index == "shannon") {
    prop = matrix / rowSums(matrix)
    -rowSums(prop * log(prop, base = exp(1)), na.rm = TRUE)
  }
}