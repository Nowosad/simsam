library(terra)

cov_grid = rast(nrows = 10, ncols = 10)
values(cov_grid) = runif(ncell(cov_grid))

# Test coordinates
proxy_raster = make_proxy(cov_grid, method = proxy_coordinates())
expect_true(inherits(proxy_raster, "SpatRaster"), info = "make_proxy should return a SpatRaster object")
expect_equal(nlyr(proxy_raster), 2, info = "coordinates proxy should return 2 layers")

# Test EDF
cov_grid2 = rast(nrows = 10, ncols = 10, crs = "local")
values(cov_grid2) = runif(ncell(cov_grid2))

proxy_raster2 = make_proxy(cov_grid2, method = proxy_edf())
expect_equal(nlyr(proxy_raster2), 5, info = "EDF proxy should return 5 layers (default)")

# Test OGC
proxy_raster3 = make_proxy(cov_grid, method = proxy_ogc(n = 3))
expect_equal(nlyr(proxy_raster3), 3, info = "OGC proxy should return the specified number of layers")

# Test add_proxy
proxy_raster4 = add_proxy(cov_grid, method = proxy_coordinates())
expect_equal(nlyr(proxy_raster4), 3, info = "add_proxy should add 2 layers to existing raster")

# Test reusable method
coords_method = proxy_coordinates()
proxy_raster5 = make_proxy(cov_grid, method = coords_method)
expect_true(inherits(proxy_raster5, "SpatRaster"), info = "Reusable method should work")
