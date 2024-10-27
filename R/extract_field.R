extract_field = function(rast_data, y) {
    if (missing(y)) {
        df_extract = terra::as.data.frame(rast_data)
    } else {
        df_extract = terra::extract(rast_data, y, ID = FALSE)
    }
    return(df_extract)
}

# rast_data = s1
# y = sam1

# sampling_area <- matrix(c(0,0,100,0,100,100,0,100,0,0), ncol=2, byrow=TRUE)
# sampling_area <- sf::st_sf(geom=sf::st_sfc(sf::st_polygon(list(sampling_area))))
# extra_area <- matrix(c(200,0,300,0,300,100,200,100,200,0), ncol=2, byrow=TRUE)
# extra_area <- sf::st_sf(geom=sf::st_sfc(sf::st_polygon(list(extra_area))))
# y = extra_area
