context("test_cv_sl.R -- cross-validated super learner")

library(sl3)
library(origami)
library(SuperLearner)

test_that("cross-validated super learner works", {
  data(cpp_imputed)
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  outcome <- "haz"
  task <- sl3_Task$new(data.table::copy(cpp_imputed),
    covariates = covars,
    outcome = outcome,
    folds = 3
  )
  glm_learner <- Lrnr_glm$new()
  glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
  earth_learner <- Lrnr_earth$new()
  learners <- list(glm_learner, glmnet_learner, earth_learner)
  sl <- make_learner(Lrnr_sl, learners, glm_learner)
  sl_fit <- sl$train(task)
  cv_sl_fit <- cv_sl(sl_fit, loss_squared_error)

  expect_false(any(is.na(cv_sl_fit)))
  expect_equal(nrow(cv_sl_fit$cv_preds), task$nrow)
})
