coords = list(x = sample(1:1000, 10, replace = TRUE), y = sample(1:1000, 10, replace = TRUE))

expect_equal(length(coords$x), length(donut_geomask(coords$x, coords$y)$x))
expect_equal(length(coords$y), length(donut_geomask(coords$x, coords$y)$y))