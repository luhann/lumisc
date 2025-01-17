coords = list(x = sample(1:1000, 10, replace = TRUE), y = sample(1:1000, 10, replace = TRUE))

expect_equal(length(coords$x), length(donut_geomask(coords$x, coords$y)$xnew))
expect_equal(length(coords$y), length(donut_geomask(coords$x, coords$y)$ynew))

expect_equal(
  length(coords$x), length(donut_geomask(coords$x, coords$y, min_dist = 5, max_dist = 6)$xnew)
)

# test that min distance must be smaller than max
expect_error(donut_geomask(coords$x, coords$y, min_dist = 50, max_dist = 6))
# error out on unequal coordinate lengths
expect_error(donut_geomask(c(coords$x, 1), coords$y, min_dist = 50, max_dist = 6))
