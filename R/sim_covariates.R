#' Simulate a raster stack of covariates based on a variogram model
#'
#' @param rast_grid A raster object with the desired dimensions
#' @param vgm A variogram model object
#' @param n The number of covariates to simulate
#' @param ... Additional arguments to be passed to the `gstat::vgm()` function; `beta`, `nmax`, `indicators` (see Examples)
#'
#' @return A raster object with the simulated covariates
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' sf1 = sim_covariates(rast_grid, range = 25,
#'   vgm = gstat::vgm(model = "Exp", psill = 1, range = 10), n = 4)
#' sf2 = sim_covariates(rast_grid, range = 50, n = 4)
#' sf3 = sim_covariates(rast_grid, range = 25,
#'   vgm = gstat::vgm(model = "Nug", psill = 2, range = 0), n = 4)
#' #sf4 = sim_covariates(rast_grid, range = 25,
#' #  vgm = gstat::vgm(model = "Exp", psill = 10, range = 100), n = 1,
#' #   beta = 30, indicators = TRUE)
#' terra::plot(sf1)
#' terra::plot(sf2)
#' terra::plot(sf3)
#' #terra::plot(sf4)
sim_covariates = function(rast_grid, vgm = NULL, n = 6, ...){

  additional_args = list(...)
  rast_grid_coords = terra::crds(rast_grid, df = TRUE)

  if (inherits(vgm, "variogramModel")){
    cov_mod = vgm
  } else {
    cov_mod = gstat::vgm(model = check_args("model", additional_args, ifnotfound = "Sph"),
                         psill = check_args("psill", additional_args, ifnotfound = 1),
                         range = check_args("range", additional_args, ifnotfound = stop("range must be provided")),
                         nugget = check_args("nugget", additional_args, ifnotfound = 0))
  }

  beta = check_args("beta", additional_args, ifnotfound = 0)
  nmax = check_args("nmax", additional_args, ifnotfound = 30)

  cov_mod = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = beta,
                         model = cov_mod, nmax = nmax, locations = ~x + y)
  suppressWarnings({cov_stack = quiet(stats::predict(cov_mod, rast_grid_coords, nsim = n,
                                   indicators = check_args("indicators", additional_args, ifnotfound = 0)))})
  new_crs = ifelse(terra::crs(rast_grid) == "", "local", terra::crs(rast_grid))
  cov_stack = terra::rast(cov_stack, crs = new_crs)
  names(cov_stack) = paste0("cov", seq_len(n))
  return(cov_stack)
}

check_args = function(arg_name, additional_args, ifnotfound){
  ifelse(arg_name %in% names(additional_args), get(arg_name, additional_args), ifnotfound)
}
