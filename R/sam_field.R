#' Sample simulation
#' @details
#' Simulates a series of sampling points for simulation problem 1.
#' @param nsamples Integer. Number of samples to simulate.
#' @param dsamples Character. Spatial distribution of the samples. 5 are
#' possible: sregular, wregular, random, wclust, sclust.
sam_field = function(sarea, nsamples, dsamples) {
  # sarea = sf::st_as_sf(terra::as.polygons(terra::ext(rast_grid)))
  if (dsamples == "sregular") {
    simpoints = jitterreg_sample(sarea, nsamples, 2)
  } else if (dsamples == "wregular") {
    simpoints = jitterreg_sample(sarea, nsamples, 5)
  } else if (dsamples == "random") {
    simpoints = sf::st_sample(sarea, nsamples)
  } else if (dsamples == "wclust") {
    simpoints = clustered_sample(sarea, nsamples, 25, 8)
  } else if (dsamples == "sclust") {
    simpoints = clustered_sample(sarea, nsamples, 10, 6)
  }
  simpoints = sf::st_sf(geometry = simpoints)
  return(simpoints)
}

#' Simulates regular samples jittered by an amount of noise.
#' @details
#' Simulates regular samples jittered by an amount of noise ~ U(-amount, amount).
#' @param sarea sf/sfc polygon where samples will be simulated.
#' @param nsamples Integer. Number of samples to simulate.
#' @param amount Numeric. Amount of jitter to apply.
jitterreg_sample = function(sarea, nsamples, amount) {
  # Simulate regular points, jitter
  res = sf::st_sample(sarea, nsamples, type = "regular")
  res = as.data.frame(sf::st_coordinates(res))
  res$X2 = res$X + runif(nrow(res), -amount, amount)
  res$Y2 = res$Y + runif(nrow(res), -amount, amount)

  # Ensure they fall within the sampling window, if not try again until they do
  res_sf = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(sarea))
  interF = !sf::st_intersects(res_sf, sarea, sparse = FALSE)
  while (any(interF)) {
    res$X2[interF] = res$X[interF] + runif(sum(interF), -amount, amount)
    res$Y2[interF] = res$Y[interF] + runif(sum(interF), -amount, amount)
    res_sf = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(sarea))
    interF = !sf::st_intersects(res_sf, sarea, sparse = FALSE)
  }

  # Convert to geometries
  res$X = NULL
  res$Y = NULL
  res = sf::st_as_sf(res, coords = c("X2", "Y2"), crs = sf::st_crs(sarea))
  res
}

#' Simulates clustered samples.
#' @details
#' Simulates clustered samples by simulating a number of randomly sampled
#' parents, and then randomly simulate children within a buffer of the parents.
#' @param sarea sf/sfc polygon where samples will be simulated.
#' @param nsamples Integer. Number of samples to simulate.
#' @param nparents Integer. Number of parents to simulate.
#' @param radius Numeric. Radius of the buffer for children simulation.
clustered_sample = function(sarea, nsamples, nparents, radius) {
  # Number of offspring per parent
  nchildren = round((nsamples - nparents) / nparents, 0)

  # Simulate parents
  parents = sf::st_sf(geometry = sf::st_sample(sarea, nparents, type = "random"))
  res = parents

  # Simulate offspring
  for (i in 1:nrow(parents)) {
    # Generate buffer and cut parts outside of the area of study
    buf = sf::st_buffer(parents[i, ], dist = radius)
    buf = sf::st_intersection(buf, sarea)

    # Simulate children
    children = sf::st_sf(geometry = sf::st_sample(buf, nchildren, type = "random"))
    res = rbind(res, children)
  }

  return(res)
}
