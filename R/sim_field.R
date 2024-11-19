#' Create a simulated field with covariates and outcome
#'
#' @param rast_grid A raster object with the desired dimensions
#' @param range The range of the semivariogram
#' @param scenario The type of error to add to the outcome: 'autocor' or 'random'
#' @param n_covariates The number of covariates to simulate
#'
#' @return A raster object with the simulated field and related covariates
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' sf1 = sim_field(rast_grid, range = 25, scenario = "autocor")
#' sf2 = sim_field(rast_grid, range = 50, scenario = "autocor")
#' sf3 = sim_field(rast_grid, range = 25, scenario = "random")
#' terra::plot(sf1)
#' terra::plot(sf2)
#' terra::plot(sf3)
sim_field = function(rast_grid, range, scenario, n_covariates = 6) {

  rast_grid_coords = terra::crds(rast_grid, df = TRUE)

  # Simulate covariates from a semivariogram and stack
  cov_mod = gstat::vgm(model = "Sph", psill = 1, range = range, nugget = 0)
  cov_mod = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = 0,
                         model = cov_mod, nmax = 100, locations = ~x + y)
  cov_stack = quiet(stats::predict(cov_mod, rast_grid_coords, nsim = n_covariates))
  cov_stack = terra::rast(cov_stack)
  names(cov_stack) = paste0("cov", seq_len(n_covariates))

  # Generate outcome
  out_rast = generate_outcome(cov_stack)

  if (scenario == "autocor") {
    # If error is autocorrelated
    autocor_mod = gstat::vgm(model = "Sph", psill = 1, range = 25, nugget = 0)
    autocor_mod = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = 0,
                              model = autocor_mod, nmax = 100, locations = ~x + y)

    autocor_pred = quiet(stats::predict(autocor_mod, rast_grid_coords, nsim = 1))
    autocor_pred = terra::rast(autocor_pred)
    out_rast = out_rast + autocor_pred
    names(out_rast) = "outcome"
    all_stack = c(out_rast, cov_stack)

  } else if (scenario == "random"){
    # otherwise add random noise
    vals = stats::rnorm(terra::ncell(rast_grid), sd = 1)
    rnoise = terra::setValues(rast_grid, vals)
    out_rast = out_rast + rnoise
    names(out_rast) = "outcome"
    all_stack = c(out_rast, cov_stack)
  }

  terra::crs(all_stack) = terra::crs(rast_grid)
  return(all_stack)
}

generate_outcome = function(cov_stack){
  covs = names(cov_stack)
  out_rast = 0

  for (i in seq(1, length(covs), by = 3)) {
    if (i + 2 <= length(covs)) {
      out_rast = out_rast + cov_stack[[covs[i]]] + cov_stack[[covs[i + 1]]] * cov_stack[[covs[i + 2]]]
    } else if (i + 1 <= length(covs)) {
      out_rast = out_rast + cov_stack[[covs[i]]] + cov_stack[[covs[i + 1]]]
    } else {
      out_rast = out_rast + cov_stack[[covs[i]]]
    }
  }
  return(out_rast)
}

