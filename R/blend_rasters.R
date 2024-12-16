#' Blend rasters based on a provided formula
#'
#' @param r A `SpatRaster` object with one or more layers
#' @param formula A formula that specifies how to combine the rasters
#' @param ... Additional `SpatRaster` objects to blend
#'
#' @return A `SpatRaster` object with the combined layers
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' sf1 = sim_covariates(rast_grid, range = 25,
#'   vgm = gstat::vgm(model = "Exp", psill = 1, range = 10), n = 4)
#' g1 = blend_rasters(sf1, y ~ cov1 + cov4)
#' g2 = blend_rasters(g1, y ~ outcome^2 + (cov2 + 2), sf1)
#' terra::plot(g1)
#' terra::plot(g2)
blend_rasters = function(r, formula, ...){
  r = c(r, ...)
  f = as_function(formula)
  output_raster = terra::lapp(r, f, usenames = TRUE)
  names(output_raster) = "outcome"
  return(output_raster)
}
# https://stackoverflow.com/a/36680963 by https://stackoverflow.com/users/601303/thilo
as_function = function(formula) {
    cmd = utils::tail(as.character(formula), 1)
    exp = parse(text = cmd)
    function(...) eval(exp, list(...))
}
