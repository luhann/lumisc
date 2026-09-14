coords = list(x = sample(1:1000, 10, replace = TRUE), y = sample(1:1000, 10, replace = TRUE))

expect_equal(length(coords$x), length(donut_geomask(coords$x, coords$y)$xnew))
expect_equal(length(coords$y), length(donut_geomask(coords$x, coords$y)$ynew))

expect_equal(
  length(coords$x),
  length(donut_geomask(coords$x, coords$y, min_dist = 5, max_dist = 6)$xnew)
)

# test that min distance must be smaller than max
expect_error(donut_geomask(coords$x, coords$y, min_dist = 50, max_dist = 6))
# error out on unequal coordinate lengths
expect_error(donut_geomask(c(coords$x, 1), coords$y, min_dist = 50, max_dist = 6))

# test that displaced points are within expected distance range
set.seed(123)
x = rep(0, 100)
y = rep(0, 100)
result = donut_geomask(x, y, min_dist = 5, max_dist = 10, units = "coord")
dists = sqrt(result$xnew^2 + result$ynew^2)
expect_true(all(dists >= 5))
expect_true(all(dists <= 10))

# test with units = "m" that coordinates are displaced (not checking exact meter conversion)
result_m = donut_geomask(c(0, 0), c(0, 0), min_dist = 100, max_dist = 200, units = "m")
expect_equal(length(result_m$xnew), 2L)
expect_true(result_m$xnew[1] != 0 || result_m$ynew[1] != 0)
set.seed(1)
area = donut_geomask(rep(0, 1e5), rep(0, 1e5), 5, 10, units = "coord")
expect_equal(mean(sqrt(area$xnew^2 + area$ynew^2) < 7.5), (7.5^2 - 5^2) / (10^2 - 5^2), tolerance = 0.02)

haversine = function(lon1, lat1, lon2, lat2) {
  rad = pi / 180
  a = sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371008.8 * asin(sqrt(a))
}
for (lat in c(-60, -34, 0, 45)) {
  masked = donut_geomask(rep(18, 1e4), rep(lat, 1e4), min_dist = 100, max_dist = 200)
  metres = haversine(18, lat, masked$xnew, masked$ynew)
  expect_true(all(metres > 99 & metres < 201), info = sprintf("displacement within donut at latitude %d", lat))
}

expect_error(donut_geomask(0, 0, 1, 2, units = "km"))
