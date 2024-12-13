#' Title
#'
#' @param formula
#' @param cov_grid
#'
#' @return
#' @export
#'
#' @examples
blend_rasters = function(cov_grid, formula, ...){
  cov_grid = c(cov_grid, ...)
  f = as_function(formula)
  output_raster = terra::lapp(cov_grid, f, usenames = TRUE)
  names(output_raster) = "outcome"
  return(output_raster)
}

# idea: option to add used covariates to the raster (piping)

# rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
# sf1 = sim_covariates(rast_grid, range = 25)
# g = generate_outcome(y ~ cov1 + cov4, sf1)
#
# terra::plot(g)
# terra::plot(sf1[[1]] + sf1[[4]])

# https://stackoverflow.com/a/36680963 by https://stackoverflow.com/users/601303/thilo
as_function = function(formula) {
    cmd = tail(as.character(formula), 1)
    exp = parse(text = cmd)
    function(...) eval(exp, list(...))
}
