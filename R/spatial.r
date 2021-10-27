
#' Donut Geomasking
#'
#' Transform x:y coordinates to new random x:y coordinates within a defined donut to mask original location.
#' This function works with GPS coordinates as well. Work is being done on supporting lon:lat coordinates.
#'
#'
#' @name donut_geomask
#' @param xcoords X coordinates to transform
#' @param ycoords Y coordinates to transform
#' @param min_dist The minimum distance the new transformed points must be from the original point
#' @param max_dist The maximum distance the new transformed points must be from the original point
#' @param units The units to do the transformation in by default meters, else will perform transformation on coordinate
#' data
#' @return A list containing new x and y coordinate vectors
#' @author Maia Lesosky
#' @author Luke Hannan
#' @export
donut_geomask = function(xcoords, ycoords, min_dist = 5, max_dist = 10, units = "m") {

  # check
  if (length(xcoords) != length(ycoords)) {
    stop("x and y coordinates different lengths, please try again")
  }

  if (min_dist >= max_dist) {
    stop("A donut, not an involution, please.")
  }

  # convert distance in meters to gps distance
  # 1° = 111 111m
  # 0.00001° = 1.11 m
  if (units == "m") {
    min_dist = min_dist / 111000
    max_dist = max_dist / 111000
  }

  # randomly sample distance - should actually sample from uniform and transform, same below
  dis = stats::runif(min = min_dist, max = max_dist, n = length(xcoords))

  # random sample rotations in degress and convert to radians
  rot_rad = stats::runif(min = 0, max = 360, n = length(xcoords)) * pi / 180 # degrees to radians

  # calc new x and y coord
  move = mapply(function(dis, rot_rad) {
    h = matrix(c(dis, 0), nrow = 2, ncol = 1, byrow = TRUE)

    rot_mat = matrix(c(cos(rot_rad), -sin(rot_rad), sin(rot_rad), cos(rot_rad)), nrow = 2, ncol = 2, byrow = TRUE)

    rot_mat %*% h
  }, dis = dis, rot_rad = rot_rad)

  xnew = move[1, ] + xcoords
  ynew = move[2, ] + ycoords

  return(list(xnew = xnew, ynew = ynew))
}