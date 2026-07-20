library(terra)
set.seed(2025-01-03)

# Create a sample raster
rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
rast_vals = rast_grid
terra::values(rast_vals) = stats::runif(terra::ncell(rast_vals))
rast_multi = c(rast_vals, rast_vals * 2)
names(rast_multi) = c("cov1", "cov2")

test_jittered = sam_field(rast_grid, size = 10, method = sample_jittered(amount = 5))
test_random = sam_field(rast_grid, size = 10, method = sample_random())
test_clustered = sam_field(rast_grid, size = 10, method = sample_clustered(nclusters = 3, radius = 5))
test_preferential = sam_field(rast_vals, size = 10, method = sample_preferential(strength = 2))
test_preferential_multi = sam_field(rast_multi, size = 10, method = sample_preferential(covariate = c("cov1", "cov2"), combine = "mean"))
test_preferential_fun = sam_field(rast_vals, size = 10, method = sample_preferential(fun = function(v) stats::dnorm(v, 0.5, 0.1)))
test_preferential_range = sam_field(rast_vals, size = 10, method = sample_preferential(range = list(lyr.1 = c(0.4, 0.6))))
test_preferential_replace = sam_field(rast_vals, size = 20, method = sample_preferential(replace = TRUE))

# Check if the function returns an sf object
expect_true(inherits(test_jittered, "sf"), info = "Jittered sampling should return an sf object")
expect_true(inherits(test_random, "sf"), info = "Random sampling should return an sf object")
expect_true(inherits(test_clustered, "sf"), info = "Clustered sampling should return an sf object")
expect_true(inherits(test_preferential, "sf"), info = "Preferential sampling should return an sf object")
expect_true(inherits(test_preferential_multi, "sf"), info = "Preferential sampling with multiple covariates should return an sf object")
expect_true(inherits(test_preferential_fun, "sf"), info = "Preferential sampling with custom fun should return an sf object")

# Check if the function returns the correct number of samples
expect_equal(nrow(test_jittered), 10, info = "Jittered sampling should return the correct number of samples")
expect_equal(nrow(test_random), 10, info = "Random sampling should return the correct number of samples")
expect_equal(nrow(test_clustered), 9, info = "Clustered sampling should return the correct number of samples")
expect_true(nrow(test_clustered) <= 10, info = "Clustered sampling should not exceed requested size")
expect_equal(nrow(test_preferential), 10, info = "Preferential sampling should return the correct number of samples")
expect_equal(nrow(test_preferential_multi), 10, info = "Preferential sampling with multiple covariates should return the correct number of samples")
expect_equal(nrow(test_preferential_fun), 10, info = "Preferential sampling with custom fun should return the correct number of samples")

expect_error(sam_field(rast_grid, size = 10, method = sample_preferential()), info = "Preferential sampling should error when raster has no values")
expect_true(all(test_preferential_range[[1]] >= 0.4 & test_preferential_range[[1]] <= 0.6), info = "Preferential sampling should respect the covariate value range")
expect_true(all(c("cov1", "cov2") %in% names(test_preferential_multi)), info = "Preferential sampling should carry covariate values as attributes")
expect_equal(nrow(test_preferential_replace), 20, info = "Preferential sampling with replacement should return the correct number of samples")
expect_error(sam_field(rast_vals, size = 10, method = sample_preferential(range = list(lyr.1 = c(2, 3)))), info = "Preferential sampling should error when no cells fall in the range")

# Check clustered sample size for non-divisible size and nclusters
test_clustered_remainder = sam_field(
  rast_grid,
  size = 11,
  method = sample_clustered(nclusters = 3, radius = 5)
)
expect_equal(
  nrow(test_clustered_remainder),
  9,
  info = "Clustered sampling should not include centroids"
)
expect_true(
  nrow(test_clustered_remainder) <= 11,
  info = "Clustered sampling should not exceed requested size"
)

# Check pipe-friendly usage
test_pipe = rast_grid |>
  sam_field(size = 10, method = sample_random())
expect_true(inherits(test_pipe, "sf"), info = "Pipe-friendly usage should work")

# Check reusable method
random_method = sample_random()
test_reusable = sam_field(rast_grid, size = 10, method = random_method)
expect_true(inherits(test_reusable, "sf"), info = "Reusable method should work")
