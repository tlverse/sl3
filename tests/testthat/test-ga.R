context("test Lrnr_ga.R -- Nonlinear Optimization via Genetic Algorithm (GA)")
skip_on_cran()
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

test_that("GA works as a metalearner", {
  data(cpp_imputed)
  covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
  task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")

  lasso_lrnr <- Lrnr_glmnet$new()
  glm_lrnr <- Lrnr_glm$new()
  ranger_lrnr <- Lrnr_ranger$new()
  lrnrs <- c(lasso_lrnr, glm_lrnr, ranger_lrnr)
  names(lrnrs) <- c("lasso", "glm", "ranger")
  lrnr_stack <- make_learner(Stack, lrnrs)

  ga <- Lrnr_ga$new()
  sl <- Lrnr_sl$new(lrnr_stack, ga)
  sl_fit <- sl$train(task)

  expect_equal(sum(sl_fit$coefficients), 1)
})
