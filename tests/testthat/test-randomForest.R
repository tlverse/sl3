library(randomForest)

test_that("Lrnr_randomForest predictions are the same as original package", {
  data(cpp_imputed)
  covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
  task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
  
  lrnr_rf <- make_learner(Lrnr_randomForest)
  set.seed(123)
  lrnr_rf_fit <- lrnr_rf$train(task)
  sl3_preds <- as.numeric(lrnr_rf_fit$predict())
  sl3_mse <- mean((task$Y - sl3_preds)^2)
  
  set.seed(123)
  rf_fit <- randomForest(x = task$X, y = task$Y, 
                         ntree = lrnr_rf$params$ntree,
                         keep.forest = lrnr_rf$params$keep.forest,
                         nodesize = lrnr_rf$params$keep.forest,
                         mtry = floor(ncol(task$X)))
  rf_preds <- as.numeric(predict(rf_fit, task$data))
  classic_mse <- mean((task$Y - rf_preds)^2)
  
  expect_equal(sl3_mse, classic_mse, tolerance = 0.05)
})
