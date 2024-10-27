devtools::document()
devtools::load_all()

rast_grid = terra::rast(ncols = 300, nrows = 100,
                        xmin = 0, xmax = 300,
                        ymin = 0, ymax = 100)

# proxies ------------------------------------
## coordinates
p_c = proxy_coordinates(rast_grid)
terra::plot(p_c)

## EDF
pc_edf = proxy_edf(rast_grid)
terra::plot(pc_edf)

# simulations -------------------------------------
s1 = sim_field(1, 25, "autocor")
terra::plot(s1)

library(purrr)
param_df = expand.grid(
  seed = 1,
  range = c(10, 40),
  scenario = c("random_noise", "autocor")
)
s_many = pmap(param_df, sim_field)
s_many_outcomes = terra::rast(map(s_many, \(x) x[[1]]))
names(s_many_outcomes) = paste0(param_df$scenario, "_", param_df$range)
terra::plot(s_many_outcomes, nc = 2)

# sampling -------------------------------------

# sregular, wregular, random, wclust, sclust.
sarea = sf::st_as_sf(terra::as.polygons(terra::ext(rast_grid)))
sam1 = sam_field(sarea, 100, "sregular")

library(purrr)
param_df = expand.grid(
  rast_grid = list(rast_grid),
  nsamples = c(100, 500),
  dsamples = c("sregular", "wregular", "random", "wclust", "sclust")
)
param_df$sams = pmap(param_df, sam_field)
sam_many = dplyr::select(param_df, sams, nsamples, dsamples)
sam_many = sf::st_as_sf(tidyr::unnest(sam_many, cols = c(sams)))

library(tmap)
tm_shape(sam_many) +
  tm_dots() +
  tm_facets_grid(rows = "nsamples", columns = "dsamples")

