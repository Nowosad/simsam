library(CAST)
library(caret)
devtools::load_all()

# simulate data ---------------------------------------------------
rast_grid = terra::rast(ncols = 300, nrows = 100,
                        xmin = 0, xmax = 300, ymin = 0, ymax = 100)
s1 = sim_field(1, 25, "autocor")
s1

# sample data -----------------------------------------------------
specify_area = function(ext){
  sf::st_sf(geom = sf::st_as_sfc(sf::st_bbox(terra::ext(ext))))
}

sampling_area = specify_area(c(0, 100, 0, 100))
extra_area = specify_area(c(200, 300, 0, 100))

train_points = sam_field(sampling_area, 200, "random")
test_inter = sam_field(sampling_area, 100, "random")
test_extra = sam_field(extra_area, 100, "random")

# prep elements ---------------------------------------------------
covariates = setdiff(names(s1), "outcome")
traindf = extract_field(s1, train_points)

gridinter_data = extract_field(s1, sampling_area)
gridextra_data = extract_field(s1, extra_area)
testinter_data = extract_field(s1, test_inter)
testextra_data = extract_field(s1, test_extra)

# tune model ------------------------------------------------------
tune_ctrl = trainControl(method = "oob")

mtry = round(seq(2, length(covariates), length.out = 5))
mtry = mtry[!duplicated(mtry)]
tune_grid = data.frame(mtry = mtry, splitrule = "variance", min.node.size = 5)

tune_mod = caret::train(traindf[covariates], traindf[, "outcome"],
                        method = "ranger", importance = "impurity",
                        num.trees = 100, trControl = tune_ctrl, tuneGrid = tune_grid)

## accuracy
surface_inter = postResample(pred = predict(tune_mod, newdata = gridinter_data),
                             obs = gridinter_data$outcome)
surface_extra = postResample(pred = predict(tune_mod, newdata = gridextra_data),
                             obs = gridextra_data$outcome)
test_inter = postResample(pred = predict(tune_mod, newdata = testinter_data),
                          obs = testinter_data$outcome)
test_extra = postResample(pred = predict(tune_mod, newdata = testextra_data),
                          obs = testextra_data$outcome)

# cvs -------------------------------------------------------------
train_cv = function(x, covariates, tune_mod, folds) {
  indxs = CreateSpacetimeFolds(data.frame(ID = folds), spacevar = "ID", k = 5)
  tgrid = data.frame(mtry = tune_mod$bestTune$mtry, splitrule = "variance", min.node.size = 5)
  tctrl = trainControl(method = "cv", index = indxs$index, savePredictions = "final")
  tmodl = caret::train(x[covariates], traindf[, "outcome"],
                       method = "ranger", importance = "none",
                       num.trees = 100,  trControl = tctrl, tuneGrid = tgrid)
  return(tmodl)
}

random_folds = fold_folds(train_points)
inter_folds = fold_folds(train_points, rast_grid, sampling_area)
extra_folds = fold_folds(train_points, rast_grid, extra_area)

random_mod = train_cv(traindf, covariates, tune_mod, random_folds)
inter_mod = train_cv(traindf, covariates, tune_mod, inter_folds)
extra_mod = train_cv(traindf, covariates, tune_mod, extra_folds)

global_validation(random_mod)
global_validation(inter_mod)
global_validation(extra_mod)

random_mod_pred = terra::predict(s1, random_mod)
inter_mod_pred = terra::predict(s1, inter_mod)
extra_mod_pred = terra::predict(s1, extra_mod)
terra::panel(c(random_mod_pred, inter_mod_pred, extra_mod_pred), nr = 3)

# aoa -------------------------------------------------------------
aoa_prop = function(grid, tune_mod){
    maoa = suppressMessages(CAST::aoa(grid, tune_mod, verbose = FALSE))
    sum(maoa$AOA == 1) / length(maoa$AOA) * 100
}

aoa_prop(gridinter_data, tune_mod)
aoa_prop(gridextra_data, tune_mod)

# varimp ----------------------------------------------------------
varimp = function(tune_mod, covariates){
    if (all(covariates %in% paste0("cov", 1:6))){
        impfeat = 100
    } else {
        impfeat = ranger::importance(tune_mod$finalModel, type = 2)
        impfeat = sum(impfeat[names(impfeat) %in% paste0("cov", 1:6)]) / sum(impfeat) * 100
    }
    return(impfeat)
}
varimp(tune_mod, covariates)

# rfgls -----------------------------------------------------------


if(scenario == "proxyonly") {
    X_RFGLS = matrix(rep(1, nrow(traindf)), ncol = 1)
    predinter_RFGLS = matrix(rep(1, nrow(gridinter_data)), ncol = 1)
    predextra_RFGLS = matrix(rep(1, nrow(gridextra_data)), ncol = 1)
} else {
    X_RFGLS = as.matrix(traindf[covariates])
    predinter_RFGLS = as.matrix(gridinter_data[, covariates])
    predextra_RFGLS = as.matrix(gridextra_data[, covariates])
}

suppressWarnings({
    mod_RFGLS = RandomForestsGLS::RFGLS_estimate_spatial(
        coords = as.matrix(traindf[c("X", "Y")]),
        y = traindf$outcome,
        X = X_RFGLS,
        ntree = 100,
        mtry = tune_mod$bestTune$mtry,
        param_estimate = TRUE)
})
suppressWarnings({
    RFGLS_surface_inter = RandomForestsGLS::RFGLS_predict_spatial(mod_RFGLS,
                                                as.matrix(gridinter_data[, c("X", "Y")]),
                                                predinter_RFGLS)$prediction
})
suppressWarnings({
    RFGLS_surface_extra = RandomForestsGLS::RFGLS_predict_spatial(mod_RFGLS,
                                                as.matrix(gridextra_data[, c("X", "Y")]),
                                                predextra_RFGLS)$prediction
})
mod_RFGLS = data.frame(
    RFGLS_surface_inter = sqrt(mean((RFGLS_surface_inter-gridinter_data$outcome)^2)),
    RFGLS_surface_extra = sqrt(mean((RFGLS_surface_extra-gridextra_data$outcome)^2))
)

