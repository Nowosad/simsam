#' Generate spatial proxies
#'
#' The `make_proxy()` function generates spatial proxies for a given raster object.
#' It generates spatial proxies such as:
#' - coordinates
#' - Euclidean Distance Fields (EDF)
#' - Oblique Geographic Coordinates (OGC)
#'
#' @param rast_grid A `SpatRaster` object
#' @param method A proxy generation method created by `proxy_*()` functions
#'
#' @references Behrens, T., Schmidt, K., Viscarra Rossel, R. A., Gries, P., Scholten, T., & MacMillan, R. A. (2018). Spatial modelling with Euclidean distance fields and machine learning. European journal of soil science, 69(5), 757-770.
#' @references Møller, A. B., Beucher, A. M., Pouladi, N., & Greve, M. H. (2020). Oblique geographic coordinates as covariates for digital soil mapping. Soil, 6(2), 269-289.
#'
#' @return A `SpatRaster` object
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' proxy_coords = make_proxy(rast_grid, proxy_coordinates())
#' proxy_edf = make_proxy(rast_grid, proxy_edf())
#' proxy_ogc = make_proxy(rast_grid, proxy_ogc(n = 5))
#'
#' terra::plot(proxy_coords)
#' terra::plot(proxy_edf)
#' terra::plot(proxy_ogc)
make_proxy = function(
  rast_grid,
  method = proxy_coordinates()
) {

  if (!inherits(rast_grid, "SpatRaster")) {
    stop("`rast_grid` must be a SpatRaster")
  }
  if (!is.function(method)) {
    stop("`method` must be a function")
  }

  proxy = method(rast_grid)

  if (!inherits(proxy, "SpatRaster")) {
    stop("Proxy method must return a SpatRaster")
  }

  proxy
}

#' Add spatial proxy to raster
#'
#' @param rast_grid A `SpatRaster`
#' @param method A proxy method created by `proxy_*()` functions
#'
#' @return `SpatRaster`
#' @export
#' @rdname make_proxy
add_proxy = function(
  rast_grid,
  method = proxy_coordinates()
) {
  c(rast_grid, make_proxy(rast_grid, method))
}

#' Coordinate proxy factory
#'
#' Returns a function that generates coordinate layers (X, Y) for a raster.
#'
#' @return A function that accepts `rast_grid` and returns a `SpatRaster`
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' make_proxy(rast_grid, proxy_coordinates())
proxy_coordinates = function() {

  function(rast_grid) {
    rast_x = terra::init(rast_grid, "x")
    rast_y = terra::init(rast_grid, "y")
    proxy_coords_stack = c(rast_x, rast_y)
    names(proxy_coords_stack) = c("X", "Y")
    return(proxy_coords_stack)
  }
}

#' Euclidean Distance Field proxy factory
#'
#' Returns a function that generates Euclidean Distance Fields from specified
#' locations (default: four corners and center of the raster extent).
#'
#' @param coords Optional coordinates. Can be a matrix, data frame, `sf`, or
#'   `SpatVector` with x and y columns. If `NULL`, uses default locations: four corners and center of the raster extent.
#'
#' @return A function that accepts `rast_grid` and returns a `SpatRaster`
#' @export
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' # Default: four corners and center
#' make_proxy(rast_grid, proxy_edf())
#'
#' # Custom coordinates
#' custom_coords = matrix(c(50, 50, 150, 150), ncol = 2, byrow = TRUE)
#' rownames(custom_coords) = c("point1", "point2")
#' make_proxy(rast_grid, proxy_edf(coords = custom_coords))
proxy_edf = function(coords = NULL) {

  function(rast_grid) {

    # If no custom coordinates are provided, use default: four corners and center.
    if (is.null(coords)) {
      rast_extent = as.vector(terra::ext(rast_grid))
      xmin = rast_extent[1]; xmax = rast_extent[2]
      ymin = rast_extent[3]; ymax = rast_extent[4]
      coords_local = rbind(
        EDF1 = c(xmin, ymax),
        EDF2 = c(xmax, ymax),
        EDF3 = c(xmin, ymin),
        EDF4 = c(xmax, ymin),
        EDF5 = c((xmin + xmax) / 2, (ymin + ymax) / 2)
      )
    } else {
      # Ensure coords is a matrix with two columns.
      coords_local = coords
      if (inherits(coords_local, "sf")) {
        coords_local = terra::vect(coords_local)
      }
      if (inherits(coords_local, "SpatVector")) {
        coords_local = terra::crds(coords_local)
      }
      coords_local = as.matrix(coords_local)
      if (ncol(coords_local) != 2) {
        stop("coords must be a matrix (or convertible to) with two columns (x, y).")
      }
      if (is.null(rownames(coords_local))) {
        rownames(coords_local) = paste0("EDF", seq_len(nrow(coords_local)))
      }
    }

    # Use appropriate coordinate flag based on CRS (lonlat or not)
    ll_crs = is_lonlat(rast_grid)

    rast_grid_coords = terra::crds(rast_grid)
    # Compute distances for each provided point; resulting in a data.frame with one column per point.
    proxy_vals_list = lapply(1:nrow(coords_local), function(i) {
      terra::distance(rast_grid_coords, coords_local[i, , drop = FALSE], lonlat = ll_crs)
    })
    proxy_edf_stack_vals = as.data.frame(do.call(cbind, proxy_vals_list))
    proxy_edf_stack = terra::rast(rast_grid, nl = ncol(proxy_edf_stack_vals))
    proxy_edf_stack = terra::setValues(proxy_edf_stack, proxy_edf_stack_vals)
    names(proxy_edf_stack) = rownames(coords_local)

    return(proxy_edf_stack)
  }
}

#' Oblique Geographic Coordinates proxy factory
#'
#' Returns a function that generates Oblique Geographic Coordinates (OGC) as covariates.
#'
#' @param n Number of angles (default: 5)
#'
#' @return A function that accepts `rast_grid` and returns a `SpatRaster`
#' @export
#'
#' @references
#' Møller, A. B., Beuchert, A. M., Pouladi, N., & Greve, M. H. (2020).
#' Oblique geographic coordinates as covariates for digital soil mapping.
#' Soil, 6(2), 269-289.
#'
#' @examples
#' rast_grid = terra::rast(
#'   ncols = 300, nrows = 100,
#'   xmin = 0, xmax = 300,
#'   ymin = 0, ymax = 100
#' )
#'
#' make_proxy(rast_grid, proxy_ogc(n = 5))
proxy_ogc = function(n = 5) {

  function(rast_grid) {
    proxy_ogc_stack = vector("list", n)
    rast_x = terra::init(rast_grid, "x")
    rast_y = terra::init(rast_grid, "y")

    theta = terra::app(
      c(rast_x, rast_y),
      function(xy) atan2(xy[2], xy[1])
    )
    r = sqrt(rast_x^2 + rast_y^2)

    angles = seq(0, 180, length.out = n + 1)
    angles = angles[-length(angles)] * pi / 180

    out = vector("list", n)

    for (i in seq_along(angles)) {
      out[[i]] = cos(angles[i] - theta) * r
    }

    out = terra::rast(out)

    names(out) = paste0("OGC_", round(angles * 180 / pi, 1))

    out
  }
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
