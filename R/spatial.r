#' Donut Geomasking
#'
#' Displace each point to a random location uniformly distributed over the area of an annulus (donut) centred on the
#' original point, so no masked point lies closer than `min_dist` or further than `max_dist` from its origin.
#'
#' With `units = "m"` coordinates are taken as longitude (x) and latitude (y) in decimal degrees and distances in
#' metres, converted with the WGS84 length of a degree at each point's latitude. With `units = "coord"` distances are
#' in the same units as the coordinates.
#'
#' @name donut_geomask
#' @param xcoords X coordinates (longitude) to transform
#' @param ycoords Y coordinates (latitude) to transform
#' @param min_dist The minimum distance the new transformed points must be from the original point
#' @param max_dist The maximum distance the new transformed points must be from the original point
#' @param units Either "m" for metres on lon:lat coordinates, or "coord" for coordinate units
#' @return A list containing new x and y coordinate vectors
#' @author Maia Lesosky
#' @author Luke Hannan
#' @export
donut_geomask = function(xcoords, ycoords, min_dist = 5, max_dist = 10, units = c("m", "coord")) {
  units = match.arg(units)
  n = length(xcoords)

  if (n != length(ycoords)) {
    rlang::abort("x and y coordinates different lengths, please try again")
  }

  if (min_dist >= max_dist) {
    rlang::abort("A donut, not an involution, please.")
  }

  # r = sqrt(U(min^2, max^2)) gives density proportional to r, i.e. uniform over the annulus area
  r = sqrt(stats::runif(n, min_dist^2, max_dist^2))
  theta = stats::runif(n, 0, 2 * pi)
  dx = r * cos(theta)
  dy = r * sin(theta)

  if (units == "m") {
    phi = ycoords * pi / 180
    dx = dx / (111412.84 * cos(phi) - 93.5 * cos(3 * phi) + 0.118 * cos(5 * phi))
    dy = dy / (111132.92 - 559.82 * cos(2 * phi) + 1.175 * cos(4 * phi) - 0.0023 * cos(6 * phi))
  }

  list(xnew = xcoords + dx, ynew = ycoords + dy)
}
