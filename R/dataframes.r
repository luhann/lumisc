#' Proportions
#'
#' Calculate proportions for a given data.frame, this extends \code{proportions} to work on data.frames.
#'
#' @name proportions
#' @param x a data.frame containing only numeric entries
#' @param margin a vector giving the margins to split by.
#' E.g., for a data.frame '1' indicates rows, '2' indicates columns.
#' @export
proportions = function(x, margin = NULL) {
  UseMethod("proportions")
}

#' @export
proportions.data.frame = function(x, margin = NULL) {
  # here we convert to matrix so marginSums and sweep can work
  # this will error if there are non-numeric entries in the matrix
  # I am ok with that
  x = base::as.matrix(x)

  if (length(margin))
    out = sweep(x, margin, marginSums(x, margin), `/`, check.margin = FALSE)
  else out = x / sum(x)

  data.table::as.data.table(out)
}



#' Write Dataframes
#'
#' Write a list of rectangular dataframes to a single csv keeping column names, and appending a single space between
#' dataframes.
#'
#' This function never writes row names to the csv file.
#'
#' @name write_df
#' @param list The list of dataframes to append to a single csv file.
#' @param file File name of the final csv.
#' @param overwrite Boolean value, if true will always overwrite file even if it exists.
#' @param ... Additional options passed to the write.table command.
#' @export
write_df = function(list, file, overwrite = TRUE, ...) {
  for (i in seq_len(length(list))) {
    # on the first write we check if overwrite is true and overwrite the file if it exists
    if (i == 1 && overwrite == TRUE) {
      utils::write.table(list[[i]], file, row.names = FALSE, col.names = TRUE, sep = ",", append = FALSE, ...)
      utils::write.table("", file, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE, ...)
      next
    }

    if (i == length(list)) {
      utils::write.table(list[[i]], file, row.names = FALSE, col.names = TRUE, sep = ",", append = TRUE, ...)
      next
    }

    utils::write.table(list[[i]], file, row.names = FALSE, col.names = TRUE, sep = ",", append = TRUE, ...)
    utils::write.table("", file, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE, ...)
  }
}
