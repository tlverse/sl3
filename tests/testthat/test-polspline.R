context("test-polspline.R -- Lrnr_polspline")

library(polspline)
data(cpp_imputed)
task <- sl3_Task$new(cpp_imputed,
                     covariates = c("apgar1", "apgar5", "parity", "gagebrth",
                                    "mage", "meducyrs", "sexn"),
                     outcome = "haz")

test_that("Results of Lrnr_polspline match those of polspline::polymars", {
  # get predictions from Lrnr_* wrapper
  set.seed(4738)
  lrnr_polspline <- make_learner(Lrnr_polspline)
  fit <- lrnr_polspline$train(task)
  preds <- fit$predict(task)

  # get predictions from classic implementation
  set.seed(4738)
  fit_classic <- polymars(predictors = task$X, responses = task$Y)
  preds_classic <- predict(fit_classic, x = task$X, type = "response")

  # check equality of predictions
  expect_equal(preds, as.numeric(preds_classic))
})


# change to binary outcome to check polspline::polyclass wrapper
task <- sl3_Task$new(cpp_imputed,
                     covariates = c("apgar1", "apgar5", "parity", "gagebrth",
                                    "mage", "meducyrs", "haz"),
                     outcome = "sexn")

test_that("Results of Lrnr_polspline match those of polspline::polyclass", {
  # get predictions from Lrnr_* wrapper
  set.seed(4738)
  lrnr_polspline <- make_learner(Lrnr_polspline)
  fit <- lrnr_polspline$train(task)
  preds <- fit$predict(task)

  # get predictions from classic implementation
  set.seed(4738)
  fit_classic <- polyclass(cov = task$X, data = task$Y, cv = 5)
  preds_classic <- ppolyclass(fit = fit_classic, cov = task$X)[, 2]

  # check equality of predictions
  expect_equal(preds, as.numeric(preds_classic))
})
