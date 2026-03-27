#' Blend rasters based on a provided formula
#'
#' @param x A `SpatRaster` object with one or more layers
#' @param formula A formula specifying how to combine the rasters
#' @param ... Additional `SpatRaster` objects to blend
#'
#' @return A `SpatRaster` object with the combined layers
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#'
#' sf1 = sim_covariates(
#'   rast_grid,
#'   n = 4,
#'   method = simulate_gaussian(range = 25)
#' )
#'
#' g1 = blend_rasters(sf1, ~ cov1 + cov4)
#' g2 = blend_rasters(g1, ~ outcome^2 + (cov2 + 2), sf1)
#'
#' terra::plot(g1)
#' terra::plot(g2)
blend_rasters = function(x, formula, ...) {

  rasters = list(x, ...)
  rasters = do.call(c, rasters)

  if (!inherits(rasters, "SpatRaster")) {
    stop("All inputs must be SpatRaster objects")
  }
  if (is.null(names(rasters))) {
    stop("All raster layers must have names")
  }
  if (anyDuplicated(names(rasters))) {
    stop("Raster layer names must be unique")
  }

  f = as_function(formula)

  output = terra::lapp(rasters, f, usenames = TRUE)

  if (terra::nlyr(output) == 1) {
    names(output) = "outcome"
  }

  return(output)
}


# Convert formula to function (but left-hand side is ignored)
as_function = function(formula) {

  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula")
  }

  env = environment(formula)

  rhs = if (length(formula) == 3) {
    formula[[3]]
  } else {
    formula[[2]]
  }

  function(...) {
    eval(rhs, envir = list(...), enclos = env)
  }
}
