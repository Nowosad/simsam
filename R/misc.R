mod_tune = function(train_data, covariates){
  tune_ctrl = caret::trainControl(method = "oob")

  mtry = round(seq(2, length(covariates), length.out = 5))
  mtry = mtry[!duplicated(mtry)]
  tune_grid = data.frame(mtry = mtry, splitrule = "variance", min.node.size = 5)

  tune_mod = caret::train(train_data[covariates], train_data[, "outcome"],
                          method = "ranger", importance = "impurity",
                          num.trees = 100, trControl = tune_ctrl, tuneGrid = tune_grid)
  return(tune_mod)
}

mod_cv = function(train_data, covariates, tune_mod, folds) {
  indxs = CAST::CreateSpacetimeFolds(data.frame(ID = folds), spacevar = "ID", k = 5)
  tgrid = data.frame(mtry = tune_mod$bestTune$mtry, splitrule = "variance", min.node.size = 5)
  tctrl = caret::trainControl(method = "cv", index = indxs$index, savePredictions = "final")
  tmodl = caret::train(train_data[covariates], train_data[, "outcome"],
                       method = "ranger", importance = "none",
                       num.trees = 100,  trControl = tctrl, tuneGrid = tgrid)
  return(tmodl)
}

aoa_prop = function(grid, tune_mod){
  maoa = suppressMessages(CAST::aoa(grid, tune_mod, verbose = FALSE))
  sum(maoa$AOA == 1) / length(maoa$AOA) * 100
}

varimp_perc = function(covariates, tune_mod){
  if (all(covariates %in% paste0("cov", 1:6))){
    impfeat = 100
  } else {
    impfeat = ranger::importance(tune_mod$finalModel, type = 2)
    impfeat = sum(impfeat[names(impfeat) %in% paste0("cov", 1:6)]) / sum(impfeat) * 100
  }
  return(impfeat)
}


