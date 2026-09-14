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
  if (!grepl(r"{\.rproj$}", path, ignore.case = TRUE)) {
    path = paste0(path, ".Rproj")
  }
  use_template("rproj", path)
}

#' Make an .lintr file in specified directory
#'
#' @param path The path to place the .lintr file
#' @return TRUE if file was successfully created
#' @export
create_lintr = function(path = ".") {
  use_template("lintr", file.path(path, ".lintr"))
}

#' Make an air.toml file in specified directory
#'
#' @param path The path to place the air.toml file
#' @return TRUE if file was successfully created
#' @export
create_air = function(path = ".") {
  use_template("air.toml", file.path(path, "air.toml"))
}

use_template = function(template, path) {
  if (file.exists(path)) {
    rlang::inform(sprintf("%s already exists", basename(path)))
    return(invisible(FALSE))
  }
  create_dir(dirname(path))
  invisible(file.copy(system.file("templates", template, package = "lumisc", mustWork = TRUE), path))
}

#' Return error message and abort if package not found
#'
#' @param package The name of the package that to be installed
#' @noRd
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
