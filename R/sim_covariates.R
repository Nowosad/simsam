#' Simulate a raster stack of covariates
#'
#' Generic function for simulating spatial covariates using a specified method.
#'
#' @param rast_grid A `SpatRaster` object with the desired dimensions
#' @param n The number of covariates to simulate (must be >= 1)
#' @param method A simulation **factory function** —- call the function with
#'   parameters to create the method (remember to not pass the function itself).
#'   For example: `method = simulate_gaussian(range = 25)`, not
#'   `method = simulate_gaussian`. See [simulate_gaussian()] and
#'   [simulate_random()] for examples.
#'
#' @return A `SpatRaster` with `n` layers named `cov1`, `cov2`, ..., `covN`
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' # Gaussian simulation
#' sf1 = sim_covariates(rast_grid, n = 4, method = simulate_gaussian(range = 25))
#' sf2 = sim_covariates(rast_grid, n = 4, method = simulate_gaussian(range = 50, model = "Exp"))
#' # Reusable simulation engine
#' gauss = simulate_gaussian(range = 25)
#' sf3 = sim_covariates(rast_grid, n = 4, method = gauss)
#' # Random simulation
#' sf4 = sim_covariates(rast_grid, n = 4, method = simulate_random())
#' terra::plot(sf1)
#' terra::plot(sf2)
#' terra::plot(sf3)
#' terra::plot(sf4)
sim_covariates = function(rast_grid, n = 6, method) {

  if (!inherits(rast_grid, "SpatRaster")) {
    stop("`rast_grid` must be a SpatRaster")
  }
  if (!is.numeric(n) || n < 1) {
    stop("`n` must be a positive integer")
  }
  if (!is.function(method)) {
    stop("`method` must be a function")
  }

  cov_stack = method(rast_grid = rast_grid, n = n)

  if (!inherits(cov_stack, "SpatRaster")) {
    stop("`method` must return a SpatRaster")
  }

  names(cov_stack) = paste0("cov", seq_len(n))

  return(cov_stack)
}

#' Simulate Gaussian random fields
#'
#' Returns a function that simulates spatially correlated Gaussian random fields
#' using a variogram model.
#'
#' @param vgm A variogram model object (`variogramModel` or `gstatVariogramModel`).
#'   If `NULL`, one is built from `model`, `psill`, `range`, and `nugget`.
#' @param model Variogram model type (e.g., `"Sph"`, `"Exp"`, `"Gau"`, `"Nug"`).
#'   Ignored if `vgm` is provided.
#' @param psill Partial sill (default: 1). Ignored if `vgm` is provided.
#' @param range Spatial range parameter. **Required** if `vgm` is `NULL`.
#' @param nugget Nugget effect (default: 0). Ignored if `vgm` is provided.
#' @param beta Mean of the Gaussian field (default: 0). Passed to `gstat::gstat()`.
#' @param nmax The number of nearest observations used for kriging simulation (default: 30).
#'   Passed to `gstat::gstat()`.
#' @param indicators Indicator thresholds (default: `FALSE`). Passed to
#'   `stats::predict()`. Use `TRUE` for default thresholds or a numeric vector
#'   for custom thresholds.
#' @param seed Optional random seed for reproducibility. Set this to ensure
#'   that the same random field is generated every time the returned function
#'   is called.
#'
#' @return A function that accepts `rast_grid` and `n` and returns a `SpatRaster`
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' # Direct usage
#' sim_fn = simulate_gaussian(range = 25)
#' sf1 = sim_covariates(rast_grid, n = 4, method = sim_fn)
#' # With custom variogram
#' vgm = gstat::vgm(model = "Exp", psill = 1, range = 10)
#' sim_fn2 = simulate_gaussian(vgm = vgm)
#' sf2 = sim_covariates(rast_grid, n = 4, method = sim_fn2)
#' terra::plot(sf1)
#' terra::plot(sf2)
simulate_gaussian = function(
  vgm = NULL,
  psill = 1,
  model = "Sph",
  range = NULL,
  nugget = 0,
  beta = 0,
  nmax = 30,
  indicators = FALSE,
  seed = NULL
) {

  # Create simulation function
  function(rast_grid, n) {

    if (!is.null(seed)) {
      base::set.seed(seed)
    }

    rast_grid_coords = terra::crds(rast_grid, df = TRUE)

    if (inherits(vgm, c("variogramModel", "gstatVariogramModel"))) {
      cov_mod = vgm
    } else {
      if (is.null(range)) {
        stop("`range` must be provided when `vgm` is NULL")
      }
      cov_mod = gstat::vgm(
        model  = model,
        psill  = psill,
        range  = range,
        nugget = nugget
      )
    }

    g = gstat::gstat(
      formula   = z ~ 1,
      dummy     = TRUE,
      beta      = beta,
      model     = cov_mod,
      nmax      = nmax,
      locations = ~x + y
    )

    # Suppress gstat messages from predict()
    suppressWarnings({cov_stack = quiet((stats::predict(
      g,
      rast_grid_coords,
      nsim = n,
      indicators = indicators
    )))})

    crs = terra::crs(rast_grid)
    if (crs == "") crs = "local"

    cov_stack = terra::rast(cov_stack, crs = crs)

    return(cov_stack)
  }
}

#' Simulate uncorrelated random fields
#'
#' Returns a function that simulates uncorrelated random fields (white noise).
#'
#' @param mean Mean of the random values (default: 0)
#' @param sd Standard deviation of the random values (default: 1)
#' @param seed Optional random seed for reproducibility
#'
#' @return A function that accepts `rast_grid` and `n` and returns a `SpatRaster`
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' sim_fn = simulate_random()
#' sf1 = sim_covariates(rast_grid, n = 4, method = sim_fn)
#' terra::plot(sf1)
simulate_random = function(mean = 0, sd = 1, seed = NULL) {

  function(rast_grid, n) {

    if (!is.null(seed)) {
      base::set.seed(seed)
    }

    r = terra::rast(rast_grid, nlyrs = n)
    terra::values(r) = stats::rnorm(terra::ncell(r) * n, mean = mean, sd = sd)

    return(r)
  }
}
