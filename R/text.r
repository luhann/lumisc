#' Count Lines
#'
#' Count the number of lines in a given text file
#'
#' This function counts the lines of a given text file line by line to prevent reading the entire file into memory at
#' once. Higher batch numbers generally equal quicker read times.
#'
#' @name count_lines
#' @param filepath A path to the text file to read
#' @param batch The number of lines to read in at one time
#' @return An integer indicating number of lines
#' @export
count_lines = function(filepath, batch) {
  con = file(filepath, open = "r")
  n = 0
  while (TRUE) {
    lines = readLines(con, n = batch)
    present = length(lines)
    n = n + present
    if (present < batch) {
      break
    }
  }

  # close file connection
  close(con)
  return(n)
}