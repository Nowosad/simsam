library(terra)
library(tidyr)
library(purrr)
specify_area = function(ext){
  sf::st_sf(geom = sf::st_as_sfc(sf::st_bbox(terra::ext(ext))))
}
devtools::load_all()

rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)

# sim -------------------------------------
param_sims = expand_grid(rast_grid = list(rast_grid),
                         range = c(10, 40),
                         scenario = c("autocor", "random_noise"))
set.seed(2024-10-29)
param_sims$sims = pmap(param_sims, sim_field)

# sam -------------------------------------
sampling_area = specify_area(c(0, 100, 0, 100))

param_sams = expand_grid(sarea = list(sampling_area),
                         nsamples = c(100, 200),
                         dsamples = c("sregular", "wregular", "random", "wclust", "sclust"))

param_sams$sams = pmap(param_sams, sam_field)

# extract training data --------------------
data_all = expand_grid(param_sims, param_sams)
data_all$train_data = map2(data_all$sims, data_all$sams, extract_field)

# create folds -----------------------------
extra_area = specify_area(c(200, 300, 0, 100))
data_all$random_folds = map(data_all$sams, fold_folds)
data_all$inter_folds = map(data_all$sams, fold_folds, rast_grid, sampling_area)
data_all$extra_folds = map(data_all$sams, fold_folds, rast_grid, extra_area)

# create model -----------------------------
data_all$mod1 = map(data_all$train_data, mod_tune, covariates = paste0("cov", 1:6))
# data_all$mod2 = map(data_all$train_data, mod_tune, covariates = c(paste0("cov", 1:6), "X", "Y", paste0("EDF", 1:5)))

# prep testing sets ------------------------
testing_inter = sam_field(sampling_area, 100, "random")
testing_extra = sam_field(extra_area, 100, "random")

data_all$testinter_data = map(data_all$sims, extract_field, testing_inter)
data_all$testextra_data = map(data_all$sims, extract_field, testing_extra)
data_all$gridinter_data = map(data_all$sims, extract_field, sampling_area)
data_all$gridextra_data = map(data_all$sims, extract_field, extra_area)

# evaluate models --------------------------
post_resample_rmse = function(mod, testing_set){
  postResample(pred = predict(mod, newdata = testing_set), obs = testing_set$outcome)["RMSE"]
}
global_validation_rmse = function(mod){
  CAST::global_validation(mod)["RMSE"]
}
## 1. sample model quality -- interpolation area
data_all$mod1_rmse_testinter = map2_dbl(data_all$mod1, data_all$testinter_data, post_resample_rmse)
## 2. sample model quality -- extrapolation area
data_all$mod1_rmse_testextra = map2_dbl(data_all$mod1, data_all$testextra_data, post_resample_rmse)
## 3. complete model quality -- interpolation area
data_all$mod1_rmse_gridinter = map2_dbl(data_all$mod1, data_all$gridinter_data, post_resample_rmse)
## 4. complete model quality -- extrapolation area
data_all$mod1_rmse_gridextra = map2_dbl(data_all$mod1, data_all$gridextra_data, post_resample_rmse)
## 5. area of applicability for interpolation and extrapolation area
data_all$mod1_aoa_gridinter = map2_dbl(data_all$mod1, data_all$gridinter_data, aoa_prop)
data_all$mod1_aoa_gridextra = map2_dbl(data_all$mod1, data_all$gridextra_data, aoa_prop)
## 6. feature importance (impact of spatial proxies)
data_all$mod1_varimp = map_dbl(data_all$mod1, varimp_perc, covariates = paste0("cov", 1:6))
## 7. random cv -- interpolation area
data_all$mod1randomcv = pmap(list(train_data = data_all$train_data,
                                  tune_mod = data_all$mod1,
                                  folds = data_all$random_folds),
                             mod_cv, covariates = paste0("cov", 1:6))
data_all$mod1randomcv_rmse = map_dbl(data_all$mod1randomcv, global_validation_rmse)
## 8. knndm cv -- interpolation area
data_all$mod1knndmcv = pmap(list(train_data = data_all$train_data,
                                tune_mod = data_all$mod1,
                                folds = data_all$inter_folds),
                           mod_cv, covariates = paste0("cov", 1:6))
data_all$mod1knndmcv_rmse = map_dbl(data_all$mod1knndmcv, global_validation_rmse)

## 9. knndm cv -- extrapolation area
data_all$mod1knndmcv_extra = pmap(list(train_data = data_all$train_data,
                                      tune_mod = data_all$mod1,
                                      folds = data_all$extra_folds),
                                 mod_cv, covariates = paste0("cov", 1:6))
data_all$mod1knndmcv_rmse_extra = map_dbl(data_all$mod1knndmcv_extra, global_validation_rmse)

## 10. spatial predictions
data_all$mod1_pred = map2(data_all$sims, data_all$mod1, predict)
data_all$mod1randomcv_pred = map2(data_all$sims, data_all$mod1randomcv, predict)
data_all$mod1knndmcv_pred = map2(data_all$sims, data_all$mod1knndmcv, predict)
data_all$mod1knndmcv_pred_extra = map2(data_all$sims, data_all$mod1knndmcv_extra, predict)

data_all |> print(width = Inf)

# quick rmse viz
library(ggplot2)
library(dplyr)
df_rmse = data_all |>
  select(range, scenario, nsamples, dsamples, contains("rmse")) |>
  pivot_longer(cols = contains("rmse"), names_to = "metric", values_to = "rmse")

ggplot(df_rmse, aes(x = dsamples, y = rmse, color = metric, shape = as.factor(nsamples))) +
  geom_point(size = 3) +
  facet_grid(scenario ~ range)

# quick spatial comparison
library(bespatial)
data_all$be = map(data_all$mod1_pred, bes_g_gao, method = "hierarchy", relative = TRUE) |>
  map_dbl("value")

plot(data_all$mod1knndmcv_rmse, data_all$be)
cor(data_all$mod1knndmcv_rmse, data_all$be)
