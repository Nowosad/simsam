proxy_coordinates = function(rast_grid){
  point_grid = sf::st_as_sf(terra::as.points(rast_grid, values = FALSE, na.rm = FALSE))

  # Generate spatial proxies: coordinates
  point_grid$x = sf::st_coordinates(point_grid)[, 1]
  point_grid$y = sf::st_coordinates(point_grid)[, 2]

  point_grid = cbind(as.data.frame(sf::st_coordinates(point_grid)),
                     sf::st_drop_geometry(point_grid))
  proxy_coords_stack = terra::rast(point_grid)
  names(proxy_coords_stack) = c("X", "Y")
  return(proxy_coords_stack)
}

proxy_edf = function(rast_grid){
  point_grid = sf::st_as_sf(terra::as.points(rast_grid, values = FALSE, na.rm = FALSE))

  # Generate spatial proxies: EDF
  rast_extent = as.vector(terra::ext(rast_grid))

  xmin = rast_extent[1]; xmax = rast_extent[2]
  ymin = rast_extent[3]; ymax = rast_extent[4]

  top_left = c(xmin, ymax); top_right = c(xmax, ymax)
  bottom_left = c(xmin, ymin); bottom_right = c(xmax, ymin)
  center = c((xmin + xmax) / 2, (ymin + ymax) / 2)

  point_grid$EDF1 = sf::st_distance(point_grid, sf::st_sfc(sf::st_point(c(0, 100))))
  point_grid$EDF2 = sf::st_distance(point_grid, sf::st_sfc(sf::st_point(c(300, 100))))
  point_grid$EDF3 = sf::st_distance(point_grid, sf::st_sfc(sf::st_point(c(0, 0))))
  point_grid$EDF4 = sf::st_distance(point_grid, sf::st_sfc(sf::st_point(c(300, 0))))
  point_grid$EDF5 = sf::st_distance(point_grid, sf::st_sfc(sf::st_point(c(150, 50))))

  point_grid = cbind(as.data.frame(sf::st_coordinates(point_grid)),
                     sf::st_drop_geometry(point_grid))
  proxy_edf_stack = terra::rast(point_grid)
  return(proxy_edf_stack)
}

