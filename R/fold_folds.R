#' Title
#'
#' @param train_points
#' @param rast_grid
#' @param aoi
#'
#' @return
#' @export
#'
#' @examples
fold_folds = function(train_points, rast_grid, aoi){
    if (missing(rast_grid) && missing(aoi)){
        folds = sample((1:nrow(train_points) %% 5) + 1, nrow(train_points))
    } else {
        point_grid = sf::st_as_sf(terra::as.points(rast_grid, values = FALSE, na.rm = FALSE))
        folds = suppressMessages(suppressWarnings(
            CAST::knndm(train_points,
                        predpoints = sf::st_intersection(point_grid, aoi),
                        k = 5, maxp = 0.5)$clusters
            ))
    }
    return(folds)
}

# sampling_area <- matrix(c(0,0,100,0,100,100,0,100,0,0), ncol=2, byrow=TRUE)
# sampling_area <- sf::st_sf(geom=sf::st_sfc(sf::st_polygon(list(sampling_area))))
# train_points <- sam_field(sampling_area,200,"random")

# fold1 = fold_folds(train_points)
# fold1

# fold2 = fold_folds(train_points, rast_grid, sampling_area)
# fold2
