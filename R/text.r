#' Count Lines
#'
#' Count the number of lines in a given text file
#'
#' This function counts newline bytes in chunks to avoid reading the entire file into memory at once. A final line
#' without a trailing newline is counted. Gzip, bzip2 and xz compressed files are read transparently.
#'
#' @name count_lines
#' @param filepath A path to the text file to read
#' @param chunk The number of bytes to read in at one time
#' @return The number of lines
#' @export
count_lines = function(filepath, chunk = 2^20) {
  con = gzfile(filepath, open = "rb")
  on.exit(close(con))
  newline = as.raw(10L)
  n = 0
  last = newline
  repeat {
    bytes = readBin(con, "raw", chunk)
    if (!length(bytes)) {
      break
    }
    n = n + sum(bytes == newline)
    last = bytes[length(bytes)]
  }
  n + (last != newline)
}
