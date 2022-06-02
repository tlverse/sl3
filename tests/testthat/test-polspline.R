context("test-polspline.R -- Lrnr_polspline")

library(polspline)
data(cpp_imputed)
task <- sl3_Task$new(cpp_imputed,
  covariates = c(
    "apgar1", "apgar5", "parity", "gagebrth",
    "mage", "meducyrs", "sexn"
  ),
  outcome = "haz"
)

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
  covariates = c(
    "apgar1", "apgar5", "parity", "gagebrth",
    "mage", "meducyrs", "haz"
  ),
  outcome = "sexn"
)

test_that("Results of Lrnr_polspline match those of polspline::polyclass", {
  covars <- c("bmi", "haz", "mage", "parity")
  outcome_bin <- "sexn"
  d <- data.table::copy(cpp_imputed)
  d[[outcome_bin]] <- as.numeric(as.factor(d[[outcome_bin]]))
  task <- sl3_Task$new(d, covariates = covars, outcome = outcome_bin)

  # get predictions from Lrnr_* wrapper
  set.seed(4738)
  lrnr_polspline <- make_learner(Lrnr_polspline)
  fit <- lrnr_polspline$train(task)
  preds <- fit$predict(task)

  # get predictions from classic implementation
  set.seed(4738)
  fit_classic <- polyclass(cov = task$X, data = task$Y)
  preds_classic <- ppolyclass(fit = fit_classic, cov = task$X)[, 2]

  # check equality of predictions
  expect_equal(preds, as.numeric(preds_classic))
})

test_that("Lrnr_polspline predictions match polspilne: categorial outcome", {
  covars <- c("bmi", "haz", "mage", "sexn")
  outcome <- "parity"
  d <- data.table::copy(cpp_imputed)
  d[[outcome]] <- as.numeric(as.factor(d[[outcome]]))
  task <- sl3_Task$new(d, covariates = covars, outcome = outcome)

  lrnr <- Lrnr_polspline$new()
  set.seed(73964)
  fit <- lrnr$train(task)
  prd <- unpack_predictions(fit$predict())

  outcome_type <- lrnr$get_outcome_type(task)
  set.seed(73964)
  fit2 <- polspline::polyclass(cov = task$X, data = outcome_type$format(task$Y))
  preds <- polspline::ppolyclass(fit = fit2, cov = task$X)

  expect_equal(prd, preds)
})
