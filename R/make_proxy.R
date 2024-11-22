#' Generate spatial proxies
#'
#' The `make_proxy()` function generates spatial proxies for a given raster object.
#' It generates three types of proxies: coordinates, Euclidean Distance Fields (EDF), and Oblique Geographic Coordinates (OGC).
#' Additionally, the `add_proxy()` function can be used to add a proxy to an existing raster object.
#'
#' @param rast_grid A raster object with the desired dimensions
#' @param type The type of proxy to generate. Options are coordinates ("coordinates"), Euclidean Distance Fields ("edf"), and Oblique Geographic Coordinates ("ogc")
#' @param n The number of angles to use when generating oblique geographic coordinates. Only used when `type = "ogc"`
#'
#' @return A SpatRaster object with the generated spatial proxies
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' proxy_coords = make_proxy(rast_grid, "coordinates")
#' proxy_edf = make_proxy(rast_grid, "edf")
#' proxy_ogc = make_proxy(rast_grid, "ogc", 5)
#' terra::plot(proxy_coords)
#' terra::plot(proxy_edf)
#' terra::plot(proxy_ogc)
make_proxy = function(rast_grid, type, n){
  if(type == "coordinates"){
    return(proxy_coordinates(rast_grid))
  } else if(type == "edf"){
    return(proxy_edf(rast_grid))
  } else if(type == "ogc"){
    return(proxy_ogc(rast_grid, n))
  }
}
#' @rdname make_proxy
add_proxy = function(rast_grid, type, n){
  c(rast_grid, make_proxy(rast_grid, type, n))
}

proxy_coordinates = function(rast_grid){
  rast_x = terra::init(rast_grid, "x")
  rast_y = terra::init(rast_grid, "y")
  proxy_coords_stack = c(rast_x, rast_y)
  names(proxy_coords_stack) = c("X", "Y")
  return(proxy_coords_stack)
}

proxy_edf = function(rast_grid){
  proxy_edf_stack = terra::rast(rast_grid, nl = 5)
  rast_grid_coords = terra::crds(rast_grid)

  # Generate spatial proxies: EDF
  rast_extent = as.vector(terra::ext(rast_grid))

  xmin = rast_extent[1]; xmax = rast_extent[2]
  ymin = rast_extent[3]; ymax = rast_extent[4]

  top_left = cbind(xmin, ymax); top_right = cbind(xmax, ymax)
  bottom_left = cbind(xmin, ymin); bottom_right = cbind(xmax, ymin)
  center = cbind((xmin + xmax) / 2, (ymin + ymax) / 2)

  ll_crs = is_lonlat(rast_grid)

  EDF1 = terra::distance(rast_grid_coords, top_left, lonlat = ll_crs)
  EDF2 = terra::distance(rast_grid_coords, top_right, lonlat = ll_crs)
  EDF3 = terra::distance(rast_grid_coords, bottom_left, lonlat = ll_crs)
  EDF4 = terra::distance(rast_grid_coords, bottom_right, lonlat = ll_crs)
  EDF5 = terra::distance(rast_grid_coords, center, lonlat = ll_crs)

  proxy_edf_stack_vals = as.data.frame(cbind(EDF1, EDF2, EDF3, EDF4, EDF5))
  proxy_edf_stack = terra::setValues(proxy_edf_stack, proxy_edf_stack_vals)
  names(proxy_edf_stack) = c("EDF1", "EDF2", "EDF3", "EDF4", "EDF5")
  return(proxy_edf_stack)
}

proxy_ogc = function(rast_grid, n){
  proxy_ogc_stack = vector("list", n)
  rast_x = terra::init(rast_grid, "x")
  rast_y = terra::init(rast_grid, "y")

  angles = seq(0, 180, length.out = n + 1) * pi / 180
  angles = angles[1:n]

  for (i in seq_along(angles)){
    proxy_ogc_stack[[i]] = cos(angles[i] - atan(rast_y / rast_x)) * sqrt(rast_x^2 + rast_y^2)
  }

  proxy_ogc_stack = terra::rast(proxy_ogc_stack)
  # proxy_ogc_stack = terra::mask(proxy_ogc_stack, rast_grid) # to mask or not to mask?
  names(proxy_ogc_stack) = paste0("Angle_", angles * 180 / pi)
  return(proxy_ogc_stack)
}

# proxy_ = function(rast_grid){
#   # Generate spatial proxies: RFsp
#   # 1
#   point_grid = sf::st_as_sf(terra::as.points(rast_grid, values = FALSE, na.rm = FALSE))
#   all_points = sf::st_as_sf(terra::as.points(rast_grid, na.rm = FALSE))
#   proxies_dist_RFsp = sf::st_distance(point_grid, all_points)
#   proxies_RFsp = cbind(proxies_dist_RFsp, point_grid)
#   proxies_RFsp = cbind(as.data.frame(sf::st_coordinates(proxies_RFsp)), sf::st_drop_geometry(proxies_RFsp))
#   proxies_stack_RFsp = terra::rast(proxies_RFsp)
#   names(proxies_stack_RFsp) = paste0("RFsp", 1:(ncol(proxies_RFsp)-2))
#   all_stack = c(all_stack, proxies_stack_RFsp)
#
#
#   # 2 (tries)
#   rast_grid_coords = terra::vect(terra::crds(rast_grid), crs = crs(rast_grid))
#   rast_grid_vals = terra::as.points(rast_grid, na.rm = FALSE)
#   d0 = terra::distance(rast_grid_vals, rast_grid_coords)
# }

