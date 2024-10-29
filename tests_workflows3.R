library(terra)
library(tidyr)
library(purrr)
devtools::load_all()
set.seed(2024-10-29)

# sim -------------------------------------
rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
param_sims = expand_grid(rast_grid = list(rast_grid),
                         range = c(10, 40),
                         scenario = c("autocor", "random_noise"))
param_sims$sims = pmap(param_sims, sim_field)

# sam -------------------------------------
specify_area = function(ext){
  sf::st_sf(geom = sf::st_as_sfc(sf::st_bbox(terra::ext(ext))))
}
sampling_area = specify_area(c(0, 100, 0, 100))

param_sams = expand_grid(sarea = list(sampling_area),
                         nsamples = c(50, 200),
                         dsamples = c("sregular", "wregular", "random", "wclust", "sclust"))

param_sams$sams = pmap(param_sams, sam_field)

# create folds -----------------------------
extra_area = specify_area(c(200, 300, 0, 100))

param_sams$random_folds = map(param_sams$sams, make_folds)
param_sams$inter_folds = map(param_sams$sams, make_folds, rast_grid, sampling_area)
param_sams$extra_folds = map(param_sams$sams, make_folds, rast_grid, extra_area)

# extract training data --------------------
data_all = expand_grid(param_sims, param_sams)

data_all$train_data = map2(data_all$sims, data_all$sams, extract_field)

# create model -----------------------------
cov_sets = list(paste0("cov", 1:6),
                c(paste0("cov", 1:6), "X", "Y", paste0("EDF", 1:5)))
data_all = expand_grid(data_all, cov_sets)

data_all$mod = map2(data_all$train_data, data_all$cov_sets, tune_mod)

# prep testing sets ------------------------
testing_inter = sam_field(sampling_area, 100, "random")
testing_extra = sam_field(extra_area, 100, "random")

data_all$testinter_data = map(data_all$sims, extract_field, testing_inter)
data_all$testextra_data = map(data_all$sims, extract_field, testing_extra)
data_all$gridinter_data = map(data_all$sims, extract_field, sampling_area)
data_all$gridextra_data = map(data_all$sims, extract_field, extra_area)

# evaluate models --------------------------
post_resample_rmse = function(mod, testing_set){
  caret::postResample(pred = predict(mod, newdata = testing_set), obs = testing_set$outcome)["RMSE"]
}
global_validation_rmse = function(mod){
  CAST::global_validation(mod)["RMSE"]
}
## 1. sample model quality -- interpolation area
data_all$mod_rmse_testinter = map2_dbl(data_all$mod, data_all$testinter_data, post_resample_rmse)
## 2. sample model quality -- extrapolation area
data_all$mod_rmse_testextra = map2_dbl(data_all$mod, data_all$testextra_data, post_resample_rmse)
## 3. complete model quality -- interpolation area
data_all$mod_rmse_gridinter = map2_dbl(data_all$mod, data_all$gridinter_data, post_resample_rmse)
## 4. complete model quality -- extrapolation area
data_all$mod_rmse_gridextra = map2_dbl(data_all$mod, data_all$gridextra_data, post_resample_rmse)
## 5. area of applicability for interpolation and extrapolation area
data_all$mod_aoa_gridinter = map2_dbl(data_all$mod, data_all$gridinter_data, aoa_prop)
data_all$mod_aoa_gridextra = map2_dbl(data_all$mod, data_all$gridextra_data, aoa_prop)
## 6. feature importance (impact of spatial proxies)
data_all$mod_varimp = map_dbl(data_all$mod, varimp_perc, covariates = paste0("cov", 1:6))
## 7. random cv -- interpolation area
data_all$modrandomcv = pmap(list(train_data = data_all$train_data,
                                 tune_mod = data_all$mod,
                                 folds = data_all$random_folds,
                                 covariates = data_all$cov_sets),
                             mod_cv)
data_all$modrandomcv_rmse = map_dbl(data_all$modrandomcv, global_validation_rmse)
## 8. knndm cv -- interpolation area
data_all$modknndmcv = pmap(list(train_data = data_all$train_data,
                                tune_mod = data_all$mod,
                                folds = data_all$inter_folds,
                                covariates = data_all$cov_sets),
                           mod_cv)
data_all$modknndmcv_rmse = map_dbl(data_all$modknndmcv, global_validation_rmse)

## 9. knndm cv -- extrapolation area
data_all$modknndmcv_extra = pmap(list(train_data = data_all$train_data,
                                      tune_mod = data_all$mod,
                                      folds = data_all$extra_folds,
                                      covariates = data_all$cov_sets),
                                 mod_cv)
data_all$modknndmcv_rmse_extra = map_dbl(data_all$modknndmcv_extra, global_validation_rmse)

## 10. spatial predictions
data_all$mod_pred = map2(data_all$sims, data_all$mod, predict)
data_all$modrandomcv_pred = map2(data_all$sims, data_all$modrandomcv, predict)
data_all$modknndmcv_pred = map2(data_all$sims, data_all$modknndmcv, predict)
data_all$modknndmcv_pred_extra = map2(data_all$sims, data_all$modknndmcv_extra, predict)

# all -----------------------------------------------------
data_all |>
  dplyr::select(-rast_grid, -sarea, -sams) |>
  print(width = Inf)

# analysis -------------------------------------------------------------------
# quick rmse viz
library(ggplot2)
library(dplyr)
df_rmse = data_all |>
  select(range, scenario, nsamples, dsamples, contains("rmse")) |>
  pivot_longer(cols = contains("rmse"), names_to = "metric", values_to = "rmse")

ggplot(df_rmse, aes(x = forcats::fct_reorder(metric, rmse), y = rmse,
                    color = dsamples, shape = as.factor(nsamples))) +
  geom_point(size = 3) +
  facet_grid(scenario ~ range) +
  coord_flip()

# quick spatial comparison
library(bespatial)
data_all$be = map(data_all$mod_pred, bes_g_gao, method = "hierarchy", relative = TRUE) |>
  map_dbl("value")

sel_id = c(which(data_all$be == min(data_all$be)),
           which(data_all$be == sort(data_all$be)[40]),
           which(data_all$be == max(data_all$be)))

panel(rast(data_all$mod_pred[sel_id]),
      main = round(data_all$be[sel_id], 2), nc = 1)

plot(data_all$mod_rmse_gridextra, data_all$be)
cor(data_all$mod_rmse_gridextra, data_all$be)

plot(data_all$mod_rmse_gridinter, data_all$be)
cor(data_all$mod_rmse_gridinter, data_all$be)
