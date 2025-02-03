#' Generate spatial proxies
#'
#' The `make_proxy()` function generates spatial proxies for a given raster object.
#' It generates three types of proxies: coordinates, Euclidean Distance Fields (EDF), and Oblique Geographic Coordinates (OGC).
#' Additionally, the `add_proxy()` function can be used to add a proxy to an existing raster object.
#'
#' @param rast_grid A raster object with the desired dimensions
#' @param type The type of proxy to generate. Options are coordinates ("coordinates"), Euclidean Distance Fields ("edf"), and Oblique Geographic Coordinates ("ogc")
#' @param opts A list of options to pass to the proxy generation function. Only used when `type = "edf"` or `type = "ogc"`.
#' When `type = "edf"`, the "`coords`" option is and sf object a matrix with two columns (x, y) representing the coordinates to use when generating Euclidean Distance Fields. When `type = "ogc"`, the "`n`" option is an integer representing the number of angles to use when generating Oblique Geographic Coordinates.
#'
#' @references Behrens, T., Schmidt, K., Viscarra Rossel, R. A., Gries, P., Scholten, T., & MacMillan, R. A. (2018). Spatial modelling with Euclidean distance fields and machine learning. European journal of soil science, 69(5), 757-770.
#' @references Møller, A. B., Beucher, A. M., Pouladi, N., & Greve, M. H. (2020). Oblique geographic coordinates as covariates for digital soil mapping. Soil, 6(2), 269-289.
#'
#' @return A SpatRaster object with the generated spatial proxies
#' @export
#'
#' @examples
#' rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
#' proxy_coords = make_proxy(rast_grid, "coordinates")
#' proxy_edf = make_proxy(rast_grid, "edf")
#' proxy_ogc = make_proxy(rast_grid, "ogc", opts = list(n = 5))
#' terra::plot(proxy_coords)
#' terra::plot(proxy_edf)
#' terra::plot(proxy_ogc)
make_proxy = function(rast_grid, type, opts = NULL){
  if(type == "coordinates"){
    return(proxy_coordinates(rast_grid))
  } else if(type == "edf"){
    return(proxy_edf(rast_grid, opts$coords))
  } else if(type == "ogc"){
    return(proxy_ogc(rast_grid, opts$n))
  }
}
#' @export
#' @rdname make_proxy
add_proxy = function(rast_grid, type, opts = NULL){
  c(rast_grid, make_proxy(rast_grid, type, opts))
}

proxy_coordinates = function(rast_grid){
  rast_x = terra::init(rast_grid, "x")
  rast_y = terra::init(rast_grid, "y")
  proxy_coords_stack = c(rast_x, rast_y)
  names(proxy_coords_stack) = c("X", "Y")
  return(proxy_coords_stack)
}

proxy_edf = function(rast_grid, coords = NULL) {

  # If no custom coordinates are provided, use default: four corners and center.
  if (is.null(coords)) {
    rast_extent = as.vector(terra::ext(rast_grid))
    xmin = rast_extent[1]; xmax = rast_extent[2]
    ymin = rast_extent[3]; ymax = rast_extent[4]
    coords = rbind(
      EDF1 = c(xmin, ymax),
      EDF2 = c(xmax, ymax),
      EDF3 = c(xmin, ymin),
      EDF4 = c(xmax, ymin),
      EDF5 = c((xmin + xmax) / 2, (ymin + ymax) / 2)
    )
  } else {
    # Ensure coords is a matrix with two columns.
    if (inherits(coords, "sf")) {
      coords = terra::vect(coords)
    }
    if (inherits(coords, "SpatVector")) {
      coords = terra::crds(coords)
    }
    coords = as.matrix(coords)
    if (ncol(coords) != 2) {
      stop("coords must be a matrix (or convertible to) with two columns (x, y).")
    }
    if (is.null(rownames(coords))) {
      rownames(coords) = paste0("EDF", seq_len(nrow(coords)))
    }
  }

  # Use appropriate coordinate flag based on CRS (lonlat or not)
  ll_crs = is_lonlat(rast_grid)

  rast_grid_coords = terra::crds(rast_grid)
  # Compute distances for each provided point; resulting in a data.frame with one column per point.
  proxy_vals_list = lapply(1:nrow(coords), function(i) {
    terra::distance(rast_grid_coords, coords[i, , drop = FALSE], lonlat = ll_crs)
  })
  proxy_edf_stack_vals = as.data.frame(do.call(cbind, proxy_vals_list))
  proxy_edf_stack = terra::rast(rast_grid, nl = ncol(proxy_edf_stack_vals))
  proxy_edf_stack = terra::setValues(proxy_edf_stack, proxy_edf_stack_vals)
  names(proxy_edf_stack) = rownames(coords)

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

