#' Sample spatial field
#'
#' Create a sample of a spatial field.
#'
#' @param x A raster object (`SpatRaster`).
#' @param size Number of samples to create.
#' @param method Sampling method (function created by [sample_random()],
#'   [sample_jittered()], or [sample_clustered()]).
#'
#' @return An `sf` object with sampled points
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' sam_field(
#'   rast_grid,
#'   100,
#'   method = sample_jittered(amount = 5)
#' )
#'
sam_field = function(
  x,
  size,
  method = sample_random()
) {

  if (!inherits(x, "SpatRaster")) {
    stop("`x` must be a SpatRaster")
  }
  if (!is.function(method)) {
    stop("`method` must be a function")
  }

  pts = method(x = x, size = size)

  if (!inherits(pts, "sf")) {
    stop("Sampling method must return an sf object")
  }

  return(pts)
}

#' Random sampling method
#'
#' Returns a function that performs random sampling on a spatial field.
#'
#' @param ... Additional arguments passed to `terra::spatSample()`
#'
#' @return A function that accepts `x` (SpatRaster) and `size` and returns an `sf` object
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' sam_field(rast_grid, 100, method = sample_random())
sample_random = function(...) {

  function(x, size) {

    pts = terra::spatSample(
      x,
      size = size,
      method = "random",
      as.points = TRUE,
      ...
    )

    return(sf::st_as_sf(pts))
  }
}

#' Jittered sampling method
#'
#' Returns a function that performs jittered sampling on a spatial field.
#' Regular points are generated first, then jittered by a random amount.
#'
#' @param amount Jitter amount in map units
#' @param ... Additional arguments passed to `terra::spatSample()`
#'
#' @return A function that accepts `x` (SpatRaster) and `size` and returns an `sf` object
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' sam_field(rast_grid, 100, method = sample_jittered(amount = 5))
sample_jittered = function(amount, ...) {

  function(x, size) {

    jittered_sample(
      x = x,
      size = size,
      amount = amount,
      ...
    )
  }
}

#' Clustered sampling method
#'
#' Returns a function that performs clustered sampling on a spatial field.
#' Cluster centers are randomly distributed, then samples are drawn within
#' a buffer radius around each cluster.
#'
#' @param nclusters Number of clusters
#' @param radius Cluster radius (in map units)
#' @param ... Additional arguments passed to `terra::spatSample()`
#'
#' @return A function that accepts `x` (SpatRaster) and `size` and returns an `sf` object
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' sam_field(rast_grid, 200, method = sample_clustered(nclusters = 5, radius = 10))
sample_clustered = function(nclusters, radius, ...) {

  function(x, size) {

    clustered_sample(
      x = x,
      size = size,
      nclusters = nclusters,
      radius = radius,
      ...
    )
  }
}

# Jittered sampling engine
jittered_sample = function(x, size, amount, ...) {

  if (!terra::hasValues(x)) {
    terra::values(x) = 1
  }

  res = terra::spatSample(
    x,
    size = size,
    method = "regular",
    exact = TRUE,
    xy = TRUE,
    ...
  )

  res$X2 = res$x + stats::runif(nrow(res), -amount, amount)
  res$Y2 = res$y + stats::runif(nrow(res), -amount, amount)

  res_t = terra::vect(res, geom = c("X2", "Y2"), crs = terra::crs(x))
  interF = which(!terra::relate(res_t, x, relation = "intersects"))

  max_iter = 100
  iter = 0

  while (length(interF) > 0 && iter < max_iter) {

    res$X2[interF] = res$x[interF] +
      stats::runif(length(interF), -amount, amount)

    res$Y2[interF] = res$y[interF] +
      stats::runif(length(interF), -amount, amount)

    res_t = terra::vect(res, geom = c("X2", "Y2"), crs = terra::crs(x))
    interF = which(!terra::relate(res_t, x, relation = "intersects"))

    iter = iter + 1
  }

  res = terra::vect(res[, c("X2", "Y2")],
                    geom = c("X2", "Y2"),
                    crs = terra::crs(x))

  return(sf::st_as_sf(res))
}

# Clustered sampling engine
clustered_sample = function(
  x,
  size,
  nclusters,
  radius,
  ...
) {

  if (!terra::hasValues(x)) {
    terra::values(x) = 1
  }

  npcluster = floor(size / nclusters)

  clusters = terra::spatSample(
    x,
    size = nclusters,
    method = "random",
    as.points = TRUE,
    ...
  )

  children_list = vector("list", nrow(clusters))

  for (i in seq_len(nrow(clusters))) {

    buf = terra::buffer(clusters[i, ], radius)
    raster_buf = terra::crop(x, buf, mask = TRUE, snap = "out")

    children = suppressWarnings(
      terra::spatSample(
        raster_buf,
        size = npcluster,
        method = "random",
        na.rm = TRUE,
        as.points = TRUE#,
        # ...
      )
    )

    children_list[[i]] = children
  }

  res = do.call(rbind, children_list)

  return(sf::st_as_sf(res))
}

#' Biased (preferential) sampling method
#'
#' Returns a function that performs preferential sampling on a spatial field.
#' A weights layer is built from one or more covariates (as a `SpatRaster`),
#' and cells are sampled with probability proportional to those weights, so
#' samples can be biased towards particular parts of the covariate value range.
#'
#' @param covariate Name(s) or index/indices of the raster layer(s) to use as
#'   the biasing covariate(s). Defaults to all layers of `x`.
#' @param strength Numeric controlling the strength of the bias applied to the
#'   rescaled covariate values. `strength = 0` gives (approximately) uniform
#'   sampling, `strength = 1` weights sampling linearly by covariate value, and
#'   larger values give a stronger preference for high-value cells. Negative
#'   values bias towards low-value cells.
#' @param fun Optional function applied to the combined, rescaled (`[0, 1]`)
#'   covariate raster to produce a weights raster. When supplied it overrides
#'   `strength`. Must accept and return a numeric vector (it is passed to
#'   `terra::app()`).
#' @param combine How to combine multiple covariates into a single weight,
#'   either `"prod"` (product) or `"mean"`. Ignored for a single covariate.
#' @param range Optional named list giving `c(min, max)` value ranges used to
#'   restrict sampling to cells whose covariate values fall within the range,
#'   e.g. `list(elevation = c(100, 500))`. Cells outside the range are masked out.
#' @param replace Logical; should cells be sampled with replacement?
#' @param ... Reserved for future use.
#'
#' @return A function that accepts `x` (SpatRaster) and `size` and returns an
#'   `sf` object.
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#' terra::values(rast_grid) = runif(terra::ncell(rast_grid))
#'
#' sam_field(rast_grid, 100, method = sample_biased(strength = 2))
sample_biased <- function(
  covariate = NULL,
  strength = 1,
  fun = NULL,
  combine = c("prod", "mean"),
  range = NULL,
  replace = FALSE,
  ...
) {
  combine <- match.arg(combine)

  function(x, size) {
    biased_sample(
      x = x,
      size = size,
      covariate = covariate,
      strength = strength,
      fun = fun,
      combine = combine,
      range = range,
      replace = replace,
      ...
    )
  }
}

# Biased (preferential) sampling engine
biased_sample <- function(
  x,
  size,
  covariate = NULL,
  strength = 1,
  fun = NULL,
  combine = "prod",
  range = NULL,
  replace = FALSE,
  ...
) {
  if (!terra::hasValues(x)) {
    stop("`x` must have values to perform biased sampling")
  }

  if (is.null(covariate)) {
    covariate <- names(x)
  }
  x_cov <- terra::subset(x, covariate)

  if (!is.null(range)) {
    if (is.null(names(range))) {
      stop("`range` must be a named list matching covariate layer names")
    }
    for (nm in names(range)) {
      if (!nm %in% names(x_cov)) {
        stop("`range` name '", nm, "' is not a selected covariate")
      }
      rng <- range[[nm]]
      lyr <- x_cov[[nm]]
      keep <- (lyr >= rng[1]) & (lyr <= rng[2])
      x_cov <- terra::mask(x_cov, keep, maskvalues = c(FALSE, NA))
    }
  }

  rng <- terra::minmax(x_cov)
  mn <- rng["min", ]
  mx <- rng["max", ]
  span <- mx - mn
  span[span == 0] <- 1
  scaled <- (x_cov - mn) / span

  # na.rm = FALSE => any cell missing a covariate becomes NA (omitted)
  if (terra::nlyr(scaled) == 1) {
    combined <- scaled
  } else if (combine == "prod") {
    combined <- terra::app(scaled, fun = prod, na.rm = FALSE)
  } else {
    combined <- terra::app(scaled, fun = mean, na.rm = FALSE)
  }

  if (!is.null(fun)) {
    if (!is.function(fun)) {
      stop("`fun` must be a function")
    }
    weights <- terra::app(combined, fun = fun)
  } else {
    weights <- (combined + 1e-9)^strength
  }
  names(weights) <- "weight"

  samp <- terra::spatSample(
    weights,
    size = size,
    method = "weights",
    replace = replace,
    na.rm = TRUE, # drop NA incl. partially observed cells
    xy = TRUE,
    cells = TRUE,
    values = FALSE,
    ...
  )

  cov_vals <- terra::extract(x_cov, samp$cell)
  cov_vals$ID <- NULL

  out <- data.frame(
    x = samp$x,
    y = samp$y,
    cov_vals
  )

  res <- sf::st_as_sf(
    out,
    coords = c("x", "y"),
    crs = sf::st_crs(x)
  )

  return(res)
}
