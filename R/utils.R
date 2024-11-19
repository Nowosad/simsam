# evaluate quietly
quiet = function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# check if raster is lonlat
is_lonlat = function(rast_grid){
  result = suppressWarnings(terra::is.lonlat(rast_grid))
  if (is.na(result) || !result){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
