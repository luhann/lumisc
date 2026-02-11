#' @name
#' retrieve_credentials
#'
#' @title
#' Read a token and other credentials from a file
#'
#' @description
#' This function helps manage tokens securely for connecting to various online data sources.
#' @param path_credential The path to the credentials file.
#' @param project_id Allows project_id to be specified, particularly useful when interacting with the redcap api.
#' @param check_url Should the urls in the credentials file be validated. Defaults to FALSE.
#' @param username Username to search for in credentials file.
#' @param uri_name uri of project to search for in credentials file.
#'
#' @export
retrieve_credentials = function(
  path_credential,
  project_id = NA_character_,
  check_url = FALSE,
  username = NA_character_,
  uri_name = NA_character_
) {
  # check if REDCapR is installed
  check_package("REDCapR")

  # return column names and types
  credentials = utils::read.table(
    file = path_credential,
    comment.char = "#",
    header = TRUE,
    colClasses = c(
      uri = "character",
      uri_name = "character",
      username = "character",
      project_id = "integer",
      token = "character",
      comment = "character"
    ),
    sep = ",",
    fill = TRUE,
    fileEncoding = "UTF-8"
  )

  # convert to data.table
  data.table::setDT(credentials)

  # Check that it's a data.frame with valid variable names
  if (!inherits(credentials, "data.frame")) {
    rlang::abort(
      "The credentials file was not correctly transformed into a base::data.frame()]. Make sure it's a well-formed CSV."
    )
  } else if (
    !identical(
      intersect(colnames(credentials), c("uri", "uri_name", "username", "project_id", "token", "comment")),
      colnames(credentials)
    )
  ) {
    rlang::abort(
      "The credentials file did not contain the proper variables of: `uri`, `username`, `project_id`, `token`, and `comment`."
    )
  }

  # Select only the records with a matching project id.
  if (!is.na(project_id)) {
    credential = credentials[credentials$project_id == project_id, ]
  }

  if (!is.na(uri_name)) {
    credential = credentials[credentials$uri_name == uri_name, ]
  }

  # If specified, select only the records with a matching username.
  if (!is.na(username)) {
    credential = credentials[credentials$username == username, ]
  }

  # Check that one and only one record matches the project id.
  if (nrow(credential) == 0L) {
    stop(
      "The project_id or uri was not found in the csv credential file."
    )
  } else if (1L < nrow(credential)) {
    stop(
      "More than one matching project_id was found in the csv credential file. There should be only one."
    )
  } else {
    credential = list(
      uri = credential$uri[1L],
      username = credential$username[1L],
      project_id = credential$project_id[1L],
      token = credential$token[1L],
      comment = credential$comment[1L]
    )
  }

  if (check_url == TRUE) {
    if (!check_url(credential$uri)) {
      rlang::abort("The matched uri does not appear to be valid, please check your credentials file.")
    }
  }

  credential
}


check_url = function(url) {
  site_regex = "^((http|https|ftp)://)?([A-Za-z0-9-]+\\.)+[A-Za-z]{2,}(:\\d{2,5})?(/\\S*)?$"
  grepl(site_regex, url, perl = TRUE)
}