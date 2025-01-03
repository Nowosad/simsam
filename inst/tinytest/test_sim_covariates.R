library(terra)
library(gstat)

# Create a sample raster
rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
test_sim_covariates = sim_covariates(rast_grid, range = 25, n = 4)

expect_true(inherits(test_sim_covariates, "SpatRaster"), info = "sim_covariates should return a SpatRaster object")
expect_equal(nlyr(test_sim_covariates), 4, info = "sim_covariates should return the correct number of layers")

# Check if the function handles variogram model correctly
vgm_model = gstat::vgm(model = "Exp", psill = 1, range = 10)
test_vgm = sim_covariates(rast_grid, vgm = vgm_model, n = 4)
expect_true(inherits(test_vgm, "SpatRaster"), info = "sim_covariates should handle variogram model correctly")

# Check if the function handles additional arguments
test_additional_args = sim_covariates(rast_grid, range = 25, n = 4, beta = 30, nmax = 50)
expect_true(inherits(test_additional_args, "SpatRaster"), info = "sim_covariates should handle additional arguments correctly")
