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
