library(CAST)
library(caret)
specify_area = function(ext){
  sf::st_sf(geom = sf::st_as_sfc(sf::st_bbox(terra::ext(ext))))
}
models = function(train_data, covariates, tune_mod, random_folds, inter_folds, extra_folds){
  tune_mod = mod_tune(train_data, covariates)
  random_mod = mod_cv(train_data, covariates, tune_mod, random_folds)
  inter_mod = mod_cv(train_data, covariates, tune_mod, inter_folds)
  extra_mod = mod_cv(train_data, covariates, tune_mod, extra_folds)
  return(list(tune_mod, random_mod, inter_mod, extra_mod))
}
get_testing_sets = function(rast_data, sampling_area, extra_area){
  testing_inter = sam_field(sampling_area, 100, "random")
  testinter_data = extract_field(rast_data, testing_inter)
  testing_extra = sam_field(extra_area, 100, "random")
  testextra_data = extract_field(rast_data, testing_extra)
  gridinter_data = extract_field(rast_data, sampling_area)
  gridextra_data = extract_field(rast_data, extra_area)
  return(list(testinter_data, testextra_data, gridinter_data, gridextra_data))
}
devtools::load_all()

# simulate data ---------------------------------------------------
rast_grid = terra::rast(ncols = 300, nrows = 100,
                        xmin = 0, xmax = 300, ymin = 0, ymax = 100)
s1 = sim_field(rast_grid, 1, 25, "autocor")

# prep data -----------------------------------------------------
sampling_area = specify_area(c(0, 100, 0, 100))
extra_area = specify_area(c(200, 300, 0, 100))

train_points = sam_field(sampling_area, 200, "random")
traindf = extract_field(s1, train_points)

# prep folds ----------------------------------------------------
random_folds = fold_folds(train_points)
inter_folds = fold_folds(train_points, rast_grid, sampling_area)
extra_folds = fold_folds(train_points, rast_grid, extra_area)

# build models --------------------------------------------------
covariates_baseline = paste0("cov", 1:6)
ms_baseline = models(traindf, covariates, tune_mod, random_folds, inter_folds, extra_folds)

covariates_coords = c(covariates_baseline, "x", "y")
ms_coords = models(traindf, covariates, tune_mod, random_folds, inter_folds, extra_folds)

covariates_edf = c(covariates_baseline, "x", "y", paste0("EDF", 1:5))
ms_edf = models(traindf, covariates, tune_mod, random_folds, inter_folds, extra_folds)

# evaluate models ------------------------------------------------
testing_sets = get_testing_sets(s1, sampling_area, extra_area)

get_rmse = function(mods, testing_sets){
  test_inter = postResample(pred = predict(mods[[1]], newdata = testing_sets[[1]]),
                            obs = testing_sets[[1]]$outcome)["RMSE"]
  test_extra = postResample(pred = predict(mods[[1]], newdata = testing_sets[[2]]),
                            obs = testing_sets[[2]]$outcome)["RMSE"]
  surface_inter = postResample(pred = predict(mods[[1]], newdata = testing_sets[[3]]),
                               obs = testing_sets[[3]]$outcome)["RMSE"]
  surface_extra = postResample(pred = predict(mods[[1]], newdata = testing_sets[[4]]),
                               obs = testing_sets[[4]]$outcome)["RMSE"]
  return(list(test_inter, test_extra, surface_inter, surface_extra))
}

get_rmse(ms_baseline, testing_sets)

get_rmse_cv = function(mods){
  random_rmse = global_validation(random_mod)["RMSE"]
  inter_rmse = global_validation(inter_mod)["RMSE"]
  extra_rmse = global_validation(extra_mod)["RMSE"]
  return(list(random_rmse, inter_rmse, extra_rmse))
}

get_rmse_cv(ms_baseline)

# cvs -------------------------------------------------------------
pred_all = function(mods){
  tune_mod_pred = terra::predict(s1, mods[[1]])
  random_mod_pred = terra::predict(s1, mods[[2]])
  inter_mod_pred = terra::predict(s1, mods[[3]])
  extra_mod_pred = terra::predict(s1, mods[[4]])
  return(c(tune_mod_pred, random_mod_pred, inter_mod_pred, extra_mod_pred))
}

mods_pred = c(s1[["outcome"]], pred_all(ms_baseline))
terra::panel(mods_pred, col = hcl.colors(n = 100, palette = "PrGn"), nc = 1)

# aoa -------------------------------------------------------------
aoa_prop(testing_sets[[3]], ms_baseline[[1]])
aoa_prop(testing_sets[[4]], ms_baseline[[1]])

# varimp ----------------------------------------------------------
varimp(ms_baseline[[1]], covariates)

