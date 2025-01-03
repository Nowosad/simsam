library(terra)

# Create sample rasters
r1 = rast(nrows = 10, ncols = 10)
values(r1) = runif(ncell(r1))
r2 = rast(nrows = 10, ncols = 10)
values(r2) = runif(ncell(r2))
r3 = rast(nrows = 10, ncols = 10)
values(r3) = runif(ncell(r3))
cov_grid = c(r1, r2, r3)
names(cov_grid) = c("cov1", "cov2", "cov3")

formula = y ~ cov1 + cov2 * cov3
blended_raster = blend_rasters(cov_grid, formula)

expect_true(inherits(blended_raster, "SpatRaster"), info = "blend_rasters should return a SpatRaster object")

expected_values = values(cov_grid[[1]]) + values(cov_grid[[2]]) * values(cov_grid[[3]])
expect_equivalent(values(blended_raster), expected_values, info = "blend_rasters should handle the formula correctly")

formula2 = y ~ cov1 + cov2 + cov3
blended_raster2 = blend_rasters(cov_grid[[1]], formula2, cov_grid[[2]], cov_grid[[3]])
expected_values2 = values(r1) + values(r2) + values(r3)
expect_equivalent(values(blended_raster2), expected_values2, info = "blend_rasters should handle additional rasters correctly")

formula3 = y ~ cov1^2 + sqrt(cov2) - log(cov3 + 1)
blended_raster3 = blend_rasters(cov_grid, formula3)
expected_values3 = values(cov_grid[[1]])^2 + sqrt(values(cov_grid[[2]])) - log(values(cov_grid[[3]]) + 1)
expect_equivalent(values(blended_raster3), expected_values3, info = "blend_rasters should handle complex formulas correctly")

expect_equal(names(blended_raster), "outcome", info = "blend_rasters should assign the correct name to the output raster")
