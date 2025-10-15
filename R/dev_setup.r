#' Create dir if it doesn't exist
#'
#' If supplied a filepath with multiple directories will create all parent directories
#'
#' @param name Name of directory
#' @export
create_dir = function(name) {
  if (!dir.exists(name)) {
    dir.create(name, recursive = TRUE)
  }
}

#' Make an .rproj file in specified directory
#'
#' @param path The name of the .Rproj file to create
#' @return TRUE if file was successfully created
#' @export
create_rproj = function(path) {
  name = basename(path)

  # if the provided name does not include .Rproj extension add it on
  if (!grepl(r"{\.rproj$}", name, ignore.case = TRUE)) {
    name = paste0(name, ".Rproj")
    if (dirname(path) == ".") {
      path = name
    } else {
      path = paste0(dirname(path), "/", name)
      create_dir(dirname(path))
    }
  }

  if (file.exists(path)) {
    rlang::inform("Rproj file already exists")
    invisible(FALSE)
  } else {
    rproj = c(
      "Version: 1.0",
      NA,
      "RestoreWorkspace: No",
      "SaveWorkspace: No",
      "AlwaysSaveHistory: Default",
      NA,
      "EnableCodeIndexing: Yes"
    )

    data.table::fwrite(list(rproj), file = path, quote = FALSE)
    invisible(TRUE)
  }
}

#' Make an .lintr file in specified directory
#'
#' @param path The path to place the .lintr file
#' @return TRUE if file was successfully created
#' @export
create_lintr = function(path) {
  if (missing(path)) {
    path = "."
  }

  path = file.path(path, ".lintr")

  create_dir(dirname(path))

  if (file.exists(path)) {
    rlang::inform(".lintr file already exists")
    invisible(FALSE)
  } else {
    lintr = c(
      "linters: linters_with_defaults(",
      "  line_length_linter = line_length_linter(128),",
      "  assignment_linter = NULL,",
      "  object_usage_linter = NULL,",
      "  commented_code_linter = NULL)",
      "encoding: \"UTF-8\""
    )

    data.table::fwrite(list(lintr), file = path, quote = FALSE)
    invisible(TRUE)
  }
}

#' Make an air.toml file in specified directory
#'
#' @param path The path to place the air.toml file
#' @return TRUE if file was successfully created
#' @export
create_air = function(path) {
  if (missing(path)) {
    path = "."
  }

  path = file.path(path, "air.toml")

  create_dir(dirname(path))

  if (file.exists(path)) {
    rlang::inform("air.toml file already exists")
    invisible(FALSE)
  } else {
    air = c(
      "[format]",
      "line-width = 128",
      "indent-width = 2",
      "indent-style = \"space\"",
      "line-ending = \"auto\"",
      "persistent-line-breaks = true",
      "exclude = []",
      "default-exclude = true",
      "skip = []"
    )
    data.table::fwrite(list(air), file = path, quote = FALSE)
    invisible(TRUE)
  }
}

#' Return error message and abort if package not found
#'
#' @param package The name of the package that to be installed
check_package = function(package) {
  if (requireNamespace(package, quietly = TRUE)) {
    TRUE
  } else {
    rlang::abort(
      paste(package, "is not installed, please install it to use these functions"),
      class = "package_not_installed"
    )
  }
}

