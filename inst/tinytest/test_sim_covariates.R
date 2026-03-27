library(terra)
library(gstat)

# Create a sample raster
rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)

# Test basic Gaussian simulation
test_sim_covariates = sim_covariates(rast_grid, n = 4, method = simulate_gaussian(range = 25))

expect_true(inherits(test_sim_covariates, "SpatRaster"), info = "sim_covariates should return a SpatRaster object")
expect_equal(nlyr(test_sim_covariates), 4, info = "sim_covariates should return the correct number of layers")

# Check if the function handles variogram model correctly
vgm_model = gstat::vgm(model = "Exp", psill = 1, range = 10)
test_vgm = sim_covariates(rast_grid, n = 4, method = simulate_gaussian(vgm = vgm_model))
expect_true(inherits(test_vgm, "SpatRaster"), info = "sim_covariates should handle variogram model correctly")

# Check if the function handles additional arguments
test_additional_args = sim_covariates(rast_grid, n = 4, method = simulate_gaussian(range = 25, beta = 30, nmax = 50))
expect_true(inherits(test_additional_args, "SpatRaster"), info = "sim_covariates should handle additional arguments correctly")

# Test reusable simulation engine
gauss = simulate_gaussian(range = 25)
test_reusable = sim_covariates(rast_grid, n = 4, method = gauss)
expect_true(inherits(test_reusable, "SpatRaster"), info = "sim_covariates should work with reusable method")

# Test simulate_random
test_random = sim_covariates(rast_grid, n = 4, method = simulate_random())
expect_true(inherits(test_random, "SpatRaster"), info = "simulate_random should return a SpatRaster")
expect_equal(nlyr(test_random), 4, info = "simulate_random should return the correct number of layers")

# Test seed reproducibility
test_seed1 = sim_covariates(rast_grid, n = 2, method = simulate_random(seed = 42))
test_seed2 = sim_covariates(rast_grid, n = 2, method = simulate_random(seed = 42))
expect_equivalent(terra::values(test_seed1), terra::values(test_seed2), info = "seed should produce reproducible results")
