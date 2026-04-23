library(terra)
set.seed(2025-01-03)

# Create a sample raster
rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)

test_jittered = sam_field(rast_grid, size = 10, method = sample_jittered(amount = 5))
test_random = sam_field(rast_grid, size = 10, method = sample_random())
test_clustered = sam_field(rast_grid, size = 10, method = sample_clustered(nclusters = 3, radius = 5))

# Check if the function returns an sf object
expect_true(inherits(test_jittered, "sf"), info = "Jittered sampling should return an sf object")
expect_true(inherits(test_random, "sf"), info = "Random sampling should return an sf object")
expect_true(inherits(test_clustered, "sf"), info = "Clustered sampling should return an sf object")

# Check if the function returns the correct number of samples
expect_equal(nrow(test_jittered), 12, info = "Jittered sampling should return the correct number of samples")
expect_equal(nrow(test_random), 10, info = "Random sampling should return the correct number of samples")
expect_equal(nrow(test_clustered), 12, info = "Clustered sampling should return the correct number of samples")

# Check pipe-friendly usage
test_pipe = rast_grid |>
  sam_field(size = 10, method = sample_random())
expect_true(inherits(test_pipe, "sf"), info = "Pipe-friendly usage should work")

# Check reusable method
random_method = sample_random()
test_reusable = sam_field(rast_grid, size = 10, method = random_method)
expect_true(inherits(test_reusable, "sf"), info = "Reusable method should work")
