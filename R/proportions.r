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
  if (!variable %in% colnames(dataframe) && typeof(variable) == "character") {
    rlang::abort(paste("Column", variable, "not found in dataframe."), class = "invalid_index_error")
  }

  if (!variable %in% seq_len(ncol(dataframe)) && typeof(variable) == "integer") {
    rlang::abort(paste("Column number", variable, "not found in dataframe."), class = "invalid_index_error")
  }

  tab = table(dataframe[[variable]], useNA = "always")

  data.table::data.table(
    values = c(names(tab)[-length(tab)], "NA"),
    count = as.numeric(tab),
    prop = round(as.numeric(prop.table(tab)), precision)
  )
}
