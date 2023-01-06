context("test-mean.R -- Fitting Intercept Models")

test_that("Lrnr_mean predictions are the same as simple means", {
  data(cpp_imputed)
  covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
  task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
  
  lrnr_mean <- make_learner(Lrnr_mean)
  mean_fit <- lrnr_mean$train(task)
  mean_preds <- mean_fit$predict()
  
  expect_equal(mean_preds, rep(mean(task$Y), nrow(cpp_imputed)))
})
