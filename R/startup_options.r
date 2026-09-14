#' Set Nice Startup Options
#'
#' Sets nicer options. All arguments are passed to the \code{options} function.
#' This function also sets \code{ipck = TRUE} in \code{rc.settings}.
#'
#' @param digits Default \code{4}
#' @param bitmapType Default "cairo".
#' @param show.signif.stars Default \code{FALSE}
#' @param useFancyQuotes Default \code{FALSE}
#' @param width Default \code{88}
#' @param Ncpus Default number of CPUs - 1. Used for parallel pkg installs.
#' @param max.print Default 100 to avoid blow up
#' @param servr.daemon Default \code{TRUE}. For xaringan presentations
#' @param mc.cores Default number of CPUs - 1. Used for parallel computing
#' @param error Default \code{"rlang"}, which sets \code{rlang::entrace}. Otherwise passed to \code{options} as is.
#' @param menu.graphics Default \code{FALSE}. Logical: should graphical menus be used if available?
#' @param continue Default \code{"-_- "}. Set the prompt used for lines which continue over one line.
#' @param warnPartialMatchArgs Default \code{TRUE}. Warn if using partial arguments.
#' @param warnPartialMatchDollar Default \code{TRUE}. Warns if partial matching is used for extraction by $.
#' @param warnPartialMatchAttr Default \code{TRUE}. Warns if partial matching is used to extract attributes via attr.
#' @param nwarnings Default \code{1e6}.
#' @param scipen Default \code{999}. Always print out full numbers, i.e. not 1e2
#' @param datatable.print.class Default \code{TRUE}. Always print data.table column class.
#' @param browser Default \code{xdg-open}. Browser to open http help documents.
#' @param HTTPUserAgent Used by RStudio Package Manager (RSPM).
#' @param download.file.extra Used by RSPM for curl/wget installs, e.g. Rscript.
#' @param ... Other arguments passed to \code{options}.
#' @export
set_startup_options = function(
  digits = 4L,
  bitmapType = "cairo",
  show.signif.stars = FALSE, # nolint
  useFancyQuotes = FALSE, # nolint
  width = 88L,
  Ncpus = max(1L, parallel::detectCores() - 1L, na.rm = TRUE),
  max.print = 100L,
  servr.daemon = TRUE,
  mc.cores = max(1L, parallel::detectCores() - 1L, na.rm = TRUE),
  error = "rlang",
  menu.graphics = FALSE,
  continue = "-_- ",
  warnPartialMatchArgs = TRUE, # nolint
  warnPartialMatchDollar = TRUE, # nolint
  warnPartialMatchAttr = TRUE, # nolint
  nwarnings = 1e6,
  scipen = 999L, # nolint
  datatable.print.class = TRUE,
  browser = "xdg-open",
  HTTPUserAgent = sprintf(
    "R/%s R (%s)",
    getRversion(),
    paste(
      getRversion(),
      R.version$platform,
      R.version$arch,
      R.version$os
    )
  ),
  download.file.extra = sprintf(
    "--header \"User-Agent: R (%s)\"",
    paste(
      getRversion(),
      R.version$platform,
      R.version$arch,
      R.version$os
    )
  ),
  ...
) {
  utils::rc.settings(ipck = TRUE)

  options(
    digits = digits,
    bitmapType = bitmapType,
    show.signif.stars = show.signif.stars, # nolint
    useFancyQuotes = useFancyQuotes, # nolint
    width = width,
    Ncpus = Ncpus,
    max.print = max.print,
    servr.daemon = servr.daemon,
    mc.cores = mc.cores,
    error = if (identical(error, "rlang")) rlang::entrace else error,
    menu.graphics = menu.graphics,
    continue = continue,
    warnPartialMatchArgs = warnPartialMatchArgs,
    warnPartialMatchDollar = warnPartialMatchDollar, # nolint
    warnPartialMatchAttr = warnPartialMatchAttr, # nolint
    scipen = scipen,
    nwarnings = nwarnings,
    browser = browser,
    HTTPUserAgent = HTTPUserAgent,
    download.file.extra = download.file.extra,
    datatable.print.class = datatable.print.class,
    ...
  )
}
