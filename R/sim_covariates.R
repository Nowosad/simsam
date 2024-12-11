sim_covariates = function(rast_grid, range, n = 6, model = NULL, ...){

  additional_args = list(...)
  rast_grid_coords = terra::crds(rast_grid, df = TRUE)

  if (inherits(model, "vgm")){
    cov_mod = model
  } else if (!missing(range)){
    cov_mod = gstat::vgm(model = "Sph", psill = 1, range = range, nugget = 0)
  } else if ("range" %in% names(additional_args)) {
    cov_mod = gstat::vgm(model = "Sph", psill = 1, range = additional_args$range, nugget = 0)
  }

  if ("beta" %in% names(additional_args)){
    beta = additional_args$beta
  } else {
    beta = 0
  }

  if ("nmax" %in% names(additional_args)){
    nmax = additional_args$nmax
  } else {
    nmax = 30
  }

  cov_mod = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = beta,
                         model = cov_mod, nmax = nmax, locations = ~x + y)
  cov_stack = quiet(stats::predict(cov_mod, rast_grid_coords, nsim = n))
  cov_stack = terra::rast(cov_stack)
  names(cov_stack) = paste0("cov", seq_len(n))
  return(cov_stack)
}
