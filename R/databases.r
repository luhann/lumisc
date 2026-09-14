#' Read a token and other credentials from a file
#'
#' Reads a csv of credentials, one row per credential, and returns the single row matching every filter supplied in
#' `...`. The file must contain a `token` column; any other columns are optional and returned as-is. Lines starting
#' with `#` are treated as comments. All fields are read as character so tokens and ids keep leading zeros.
#'
#' @param path_credential The path to the credentials file.
#' @param ... Named filters, each matching a column of the credentials file, e.g. `project_id = 123` or
#'   `username = "luke"`.
#' @param check_url Should the `uri` of the matched credential be validated. Defaults to FALSE.
#' @return A named list holding the matched row.
#'
#' @export
retrieve_credentials = function(path_credential, ..., check_url = FALSE) {
  filters = list(...)
  if (!rlang::is_named2(filters)) {
    rlang::abort("All filters in `...` must be named.", class = "invalid_filter_error")
  }

  credentials = data.table::fread(path_credential, colClasses = "character", comment.char = "#")

  missing_cols = setdiff(c("token", if (check_url) "uri", names(filters)), names(credentials))
  if (length(missing_cols)) {
    rlang::abort(
      sprintf("The credentials file lacks column(s): %s.", toString(missing_cols)),
      class = "invalid_credentials_error"
    )
  }

  hits = Reduce(`&`, Map(\(col, value) credentials[[col]] == value, names(filters), filters), rep(TRUE, nrow(credentials)))
  credential = credentials[which(hits), ]

  if (nrow(credential) != 1L) {
    rlang::abort(
      sprintf("%d credentials matched the filters; expected exactly one.", nrow(credential)),
      class = "credential_match_error"
    )
  }

  if (check_url && !is_valid_url(credential$uri)) {
    rlang::abort("The matched uri does not appear to be valid, please check your credentials file.")
  }

  as.list(credential)
}


is_valid_url = function(url) {
  host = r"{([A-Za-z0-9-]+\.)+[A-Za-z]{2,}|localhost|(\d{1,3}\.){3}\d{1,3}}"
  grepl(sprintf(r"{^((https?|ftp)://)?(%s)(:\d{2,5})?(/\S*)?$}", host), url, perl = TRUE)
}
