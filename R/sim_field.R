sim_field = function(rast_grid, seed, range, scenario) {

  # Print progress
  # print(seed)
  set.seed(seed)

  # Create an empty results object
  res = data.frame()

  # Create grids (raster and point format) and sampling and extrapolation area
  # rast_grid = terra::rast(ncols = 300, nrows = 100,
  #                         xmin = 0, xmax = 300,
  #                         ymin = 0, ymax = 100)
  point_grid = sf::st_as_sf(terra::as.points(rast_grid, values = FALSE, na.rm = FALSE))

  # Simulate 6 covariates from a semivariogram and stack
  cov_mod = gstat::vgm(model = "Sph", psill = 1, range = range, nugget = 0)
  cov_mod = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = 0,
                         model = cov_mod, nmax = 100)
  cov_points = quiet(stats::predict(cov_mod, point_grid, nsim = 6))
  cov_points = cbind(as.data.frame(sf::st_coordinates(point_grid)),
                     sf::st_drop_geometry(cov_points))
  cov_stack = terra::rast(cov_points)
  names(cov_stack) = paste0("cov", 1:6)
  rm("cov_mod")

  # Generate outcome
  out_rast = cov_stack$cov1 + cov_stack$cov2 * cov_stack$cov3 +
    cov_stack$cov4 + cov_stack$cov5 * cov_stack$cov6

  # If error is autocorrelated
  if (scenario == "autocor") {
    snoise_mod = gstat::vgm(model = "Sph", psill = 1, range = 25, nugget = 0)
    snoise_mod = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = 0,
                              model = snoise_mod, nmax = 100)
    snoise_points = quiet(stats::predict(snoise_mod, point_grid, nsim = 1))
    snoise_points = cbind(as.data.frame(sf::st_coordinates(point_grid)),
                          sf::st_drop_geometry(snoise_points))
    snoise = terra::rast(snoise_points)
    names(snoise) = "error"
    out_rast = out_rast + snoise
    names(out_rast) = "outcome"
    all_stack = c(out_rast, cov_stack)
  } else if (scenario == "random_noise"){
    # otherwise add random noise
    rnoise = terra::rast(ncols = 300, nrows = 100,
                         xmin = 0, xmax = 300,
                         ymin = 0, ymax = 100)
    vals = stats::rnorm(300 * 100, sd = 1)
    rnoise = terra::setValues(rnoise, vals)
    out_rast = out_rast + rnoise
    names(out_rast) = "outcome"
    all_stack = c(out_rast, cov_stack)
  }

  # Generate spatial proxies: coordinates
  p_c = proxy_coordinates(rast_grid)

  # Generate spatial proxies: EDF
  p_edf = proxy_edf(rast_grid)

  all_stack = c(all_stack, p_c, p_edf)

  # Generate spatial proxies: RFsp
  # all_points = sf::st_as_sf(terra::as.points(rast_grid, na.rm = FALSE))
  # proxies_dist_RFsp = sf::st_distance(point_grid, all_points)
  # proxies_RFsp = cbind(proxies_dist_RFsp, point_grid)
  # proxies_RFsp = cbind(as.data.frame(sf::st_coordinates(proxies_RFsp)), sf::st_drop_geometry(proxies_RFsp))
  # proxies_stack_RFsp = terra::rast(proxies_RFsp)
  # names(proxies_stack_RFsp) = paste0("RFsp", 1:(ncol(proxies_RFsp)-2))
  # all_stack = c(all_stack, proxies_stack_RFsp)

  # Return the generated raster data
  return(all_stack)
}
