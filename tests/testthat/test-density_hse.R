context("test-density_hse.R -- Lrnr_density_hse")

test_that("density_hse produces same results as density estimates from glm", {
  data(cpp_imputed)
  task <- sl3_Task$new(
    cpp_imputed, 
    covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs"), 
    outcome = "haz")
  
  lrnr_density_hse <- Lrnr_density_hse$new(mean_learner = Lrnr_glm$new())
  lrnr_glm <- Lrnr_glm$new()
  
  fit_density_hse <- lrnr_density_hse$train(task)
  fit_glm <- lrnr_glm$train(task)
  
  # density hse
  preds_density_hse <- fit_density_hse$predict()
  
  # density from glm
  mean_preds <- fit_glm$predict()
  errors <- task$Y - mean_preds
  dens_fit <- density(errors)
  preds_glm_density <- approx(dens_fit$x, dens_fit$y, errors, rule = 2)$y
  
  expect_equal(preds_density_hse, preds_glm_density)
})
