#' Sample spatial field
#'
#' Create a sample of a spatial field.
#'
#' @param x A raster object.
#' @param size Number of samples to create.
#' @param type Expected spatial distribution of the samples. Possible values are:
#' `"jittered"`, `"random"`, `"clustered"`.
#' @param value Amount of jitter to apply to the samples (in map units; only used when `type = "jittered"`)
#' or radius of the buffer around each cluster (only used when `type = "clustered"`).
#' @param nclusters Number of clusters to simulate. Only used when `type = "clustered"`.
#' @param ... Additional arguments passed to `terra::spatSample()`.
#'
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' sam_field(rast_grid, 100, "jittered", 5)
sam_field = function(x, size, type = "", value, nclusters, ...) {
  # if (!inherits(x, "sf") || !(sf::st_geometry_type(x, by_geometry = FALSE) %in% c("POLYGON", "MULTIPOLYGON"))) {
  #   x = sf::st_as_sf(terra::as.polygons(terra::ext(x)))
  # }
  if (type == "jittered") {
    simpoints = jittered_sample(x, size, value, ...)
  } else if (type == "random") {
    simpoints = terra::spatSample(x, size = size, method = "random", as.points = TRUE, ...)
    simpoints = sf::st_as_sf(simpoints)
    # simpoints = sf::st_sample(x, size, ...)
  } else if (type == "clustered") {
    simpoints = clustered_sample(x, size, nclusters, value, ...)
  } else {
    simpoints = terra::spatSample(x, size = size, as.points = TRUE, ...)
    simpoints = sf::st_as_sf(simpoints)
  }
  simpoints = sf::st_sf(geometry = sf::st_geometry(simpoints))
  return(simpoints)
}

#' Simulates regular sample jittered by an amount of noise
#'
#' Simulates regular sample jittered by an amount of noise ~ U(-amount, amount).
#'
#' @param x sf/sfc polygon where samples will be simulated.
#' @param size Number of samples to create.
#' @param amount Amount of jitter to apply.
#' @param ... Additional arguments passed to `terra::spatSample()`.
jittered_sample = function(x, size, amount, ...){
  if (!terra::hasValues(x)){
    terra::values(x) = 1
  }

  # Simulate regular points, jitter
  res = terra::spatSample(x, size = size, method = "regular", xy = TRUE, ...)
  res$X2 = res$x + stats::runif(nrow(res), -amount, amount)
  res$Y2 = res$y + stats::runif(nrow(res), -amount, amount)

  # Ensure they fall within the sampling window, if not try again until they do
  res_t = terra::vect(res, geom = c("X2", "Y2"), crs = terra::crs(x))
  interF = which(!terra::relate(res_t, terra::ext(x), relation = "intersects"))
  while (length(interF) > 0) {
    res$X2[interF] = res$X[interF] + stats::runif(length(interF), -amount, amount)
    res$Y2[interF] = res$Y[interF] + stats::runif(length(interF), -amount, amount)
    res_t = terra::vect(res, geom = c("X2", "Y2"), crs = terra::crs(x))
    interF = which(!terra::relate(res_t, terra::ext(x), relation = "intersects"))
  }

  res$x = NULL
  res$y = NULL
  res = terra::vect(res, geom = c("X2", "Y2"), crs = terra::crs(x))
  return(sf::st_as_sf(res))
}
# jittered_sample0 = function(x, size, amount, ...) {
#   # Simulate regular points, jitter
#   res = sf::st_sample(x, size, type = "regular", ...)
#   res = as.data.frame(sf::st_coordinates(res))
#   res$X2 = res$X + stats::runif(nrow(res), -amount, amount)
#   res$Y2 = res$Y + stats::runif(nrow(res), -amount, amount)
#
#   # Ensure they fall within the sampling window, if not try again until they do
#   res_sf = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(x))
#   interF = !sf::st_intersects(res_sf, x, sparse = FALSE)
#   while (any(interF)) {
#     res$X2[interF] = res$X[interF] + stats::runif(sum(interF), -amount, amount)
#     res$Y2[interF] = res$Y[interF] + stats::runif(sum(interF), -amount, amount)
#     res_sf = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(x))
#     interF = !sf::st_intersects(res_sf, x, sparse = FALSE)
#   }
#
#   res$X = NULL
#   res$Y = NULL
#   res = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(x))
#   res
# }

#' Simulates clustered samples
#'
#' Simulates clustered samples by simulating a number of randomly sampled
#' clusters, and then randomly simulate points within a buffer of the clusters.
#'
#' @param x sf/sfc polygon where samples will be simulated.
#' @param size Number of samples to create.
#' @param nclusters Number of clusters to simulate.
#' @param radius Radius of the buffer for intra-cluster simulation.
#' @param ... Additional arguments passed to `terra::spatSample()`.
clustered_sample = function(x, size, nclusters, radius, ...){
  if (!terra::hasValues(x)){
    terra::values(x) = 1
  }

  # Number of points per cluster
  npcluster = round((size - nclusters) / nclusters, 0)

  # Simulate clusters
  clusters = terra::spatSample(x, size = nclusters, method = "random", as.points = TRUE, ...)
  res = clusters

  # Simulate points per cluster
  for (i in 1:nrow(clusters)) {
    # Generate buffer and cut parts outside of the area of study
    buf = terra::buffer(clusters[i, ], radius)
    buf = terra::crop(x, buf, mask = TRUE)

    # Simulate points
    suppressWarnings({children = terra::spatSample(buf, size = npcluster,
                                                   method = "random", as.points = TRUE, ...)})
    res = rbind(res, children)
  }
  res$lyr.1 = NULL
  return(sf::st_as_sf(res))
}
# clustered_sample0 = function(x, size, nclusters, radius) {
#   # Number of points per cluster
#   npcluster = round((size - nclusters) / nclusters, 0)
#
#   # Simulate clusters
#   clusters = sf::st_sf(geometry = sf::st_sample(x, nclusters, type = "random"))
#   res = clusters
#
#   # Simulate points per cluster
#   for (i in 1:nrow(clusters)) {
#     # Generate buffer and cut parts outside of the area of study
#     buf = sf::st_buffer(clusters[i, ], dist = radius)
#     buf = sf::st_intersection(buf, x)
#
#     # Simulate points
#     children = sf::st_sf(geometry = sf::st_sample(buf, npcluster, type = "random"))
#     res = rbind(res, children)
#   }
#
#   return(res)
# }


