generate_outcome = function(formula, cov_grid){
  formula_function = function(...) {
    eval(parse(text = formula))
  }

  output_raster = formula_function(cov_grid)

  return(output_raster)
}

rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
sf1 = sim_covariates(rast_grid, range = 25)
generate_outcome("~cov1 + cov2", sf1)
