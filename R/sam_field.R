#' Sample spatial field
#'
#' Create a sample of a spatial field.
#'
#' @param x Any object that can be coerced to an sf object.
#' @param size Number of samples to create.
#' @param type Expected spatial distribution of the samples. Possible values are:
#' `"jittered"`, `"random"`, `"clust"`.
#' @param value Amount of jitter to apply to the samples (in map units; only used when `type = "jittered"`)
#' or radius of the buffer around each cluster (only used when `type = "clust"`).
#' @param nclusters Number of clusters to simulate. Only used when `type = "clust"`.

#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' sam_field(rast_grid, 100, "jittered", 5)
#' vect_grid = sf::st_as_sf(terra::as.polygons(rast_grid))
#' sam_field(vect_grid, 100, "random")
sam_field = function(x, size, type, value, nclusters) {
  if (!inherits(x, "sf") || !(sf::st_geometry_type(x, by_geometry = FALSE) %in% c("POLYGON", "MULTIPOLYGON"))) {
    x = sf::st_as_sf(terra::as.polygons(terra::ext(x)))
  }
  if (type == "jittered") {
    simpoints = jitterreg_sample(x, size, value)
  } else if (type == "random") {
    simpoints = sf::st_sample(x, size)
  } else if (type == "clust") {
    simpoints = clustered_sample(x, size, nclusters, value)
  }
  simpoints = sf::st_sf(geometry = simpoints)
  return(simpoints)
}

#' Simulates regular sample jittered by an amount of noise
#'
#' Simulates regular sample jittered by an amount of noise ~ U(-amount, amount).
#'
#' @param x sf/sfc polygon where samples will be simulated.
#' @param size Number of samples to create.
#' @param amount Amount of jitter to apply.
jitterreg_sample = function(x, size, amount) {
  # Simulate regular points, jitter
  res = sf::st_sample(x, size, type = "regular")
  res = as.data.frame(sf::st_coordinates(res))
  res$X2 = res$X + stats::runif(nrow(res), -amount, amount)
  res$Y2 = res$Y + stats::runif(nrow(res), -amount, amount)

  # Ensure they fall within the sampling window, if not try again until they do
  res_sf = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(x))
  interF = !sf::st_intersects(res_sf, x, sparse = FALSE)
  while (any(interF)) {
    res$X2[interF] = res$X[interF] + stats::runif(sum(interF), -amount, amount)
    res$Y2[interF] = res$Y[interF] + stats::runif(sum(interF), -amount, amount)
    res_sf = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(x))
    interF = !sf::st_intersects(res_sf, x, sparse = FALSE)
  }

  res$X = NULL
  res$Y = NULL
  res = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(x))
  res
}

#' Simulates clustered samples
#'
#' Simulates clustered samples by simulating a number of randomly sampled
#' clusters, and then randomly simulate points within a buffer of the clusters.
#'
#' @param x sf/sfc polygon where samples will be simulated.
#' @param size Number of samples to create.
#' @param nclusters Number of clusters to simulate.
#' @param radius Radius of the buffer for intra-cluster simulation.
clustered_sample = function(x, size, nclusters, radius) {
  # Number of points per cluster
  npcluster = round((size - nclusters) / nclusters, 0)

  # Simulate clusters
  clusters = sf::st_sf(geometry = sf::st_sample(x, nclusters, type = "random"))
  res = clusters

  # Simulate points per cluster
  for (i in 1:nrow(clusters)) {
    # Generate buffer and cut parts outside of the area of study
    buf = sf::st_buffer(clusters[i, ], dist = radius)
    buf = sf::st_intersection(buf, x)

    # Simulate points
    children = sf::st_sf(geometry = sf::st_sample(buf, npcluster, type = "random"))
    res = rbind(res, children)
  }

  return(res)
}
