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

  values = c(names(table(dataframe[[variable]])), "NA")
  count = as.numeric(table(dataframe[[variable]], useNA = "always"))
  prop = round(prop.table(count), precision)

  out = tibble::tibble(
    values = values,
    count = count,
    prop = prop)

  return(out)
}
