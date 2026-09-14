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