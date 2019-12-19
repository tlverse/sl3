context("test-cv-selector.R -- Lrnr_cv_selector")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  devtools::check() # runs full check
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

# library(data.table) library(origami)
set.seed(1)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

test_that("Lrnr_cv_selector selects the learner that minimizes mean cv risk", {
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_lasso <- make_learner(Lrnr_glmnet)
  lrnr_mean <- make_learner(Lrnr_mean)
  base_lrnrs <- make_learner(Stack, lrnr_glm, lrnr_lasso, lrnr_mean)

  metalearner <- make_learner(Lrnr_cv_selector)

  sl <- make_learner(Lrnr_sl, base_lrnrs, metalearner)
  fit <- sl$train(task)
  preds <- fit$predict()

  cv_stack_fit <- fit$fit_object$cv_fit
  stack_risks <- cv_stack_fit$cv_risk(loss_squared_error)
  cv_selector_index <- which.min(stack_risks$mean_risk)

  fit_lrnrs <- fit$fit_object$cv_fit$fit_object$full_fit$fit_object$learner_fits
  cv_selector_fit <- fit_lrnrs[[cv_selector_index]]
  preds2 <- cv_selector_fit$predict(task)
  expect_equal(preds, preds2)
})
