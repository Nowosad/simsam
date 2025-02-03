library(terra)

cov_grid = rast(nrows = 10, ncols = 10)
values(cov_grid) = runif(ncell(cov_grid))

proxy_raster = make_proxy(cov_grid, type = "coordinates")
expect_true(inherits(proxy_raster, "SpatRaster"), info = "make_proxy should return a SpatRaster object")

cov_grid2 = rast(nrows = 10, ncols = 10, crs = "local")
values(cov_grid2) = runif(ncell(cov_grid2))

proxy_raster2 = make_proxy(cov_grid2, type = "edf")
expect_equal(nlyr(proxy_raster2), 5, info = "make_proxy should return the correct number of layers")

proxy_raster3 = make_proxy(cov_grid, type = "ogc", opts = list(n = 3))
expect_equal(nlyr(proxy_raster3), 3, info = "make_proxy should return the correct number of layers")

