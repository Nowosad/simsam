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
    buf = terra::crop(x, buf, mask = TRUE)

    children = suppressWarnings(
      terra::spatSample(
        buf,
        size = npcluster,
        method = "random",
        as.points = TRUE,
        ...
      )
    )

    children_list[[i]] = children
  }

  res = do.call(rbind, c(list(clusters), children_list))

  return(sf::st_as_sf(res))
}
