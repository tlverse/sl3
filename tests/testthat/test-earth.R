context("test-earth.R -- Lrnr_earth")

library(earth)
data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed,
  covariates = covars,
  outcome = outcome
)

test_that("Lrnr_earth produces results matching those of earth::earth", {
  # get predictions from Lrnr_* wrapper
  set.seed(4738)
  lrnr_earth <- make_learner(Lrnr_earth)
  fit <- lrnr_earth$train(task)
  preds <- fit$predict(task)

  # get predictions from classic implementation
  set.seed(4738)
  fit_classic <- earth::earth(
    x = task$X, y = task$Y, degree = 2, penalty = 3,
    pmethod = "backward", nfold = 0, ncross = 1,
    minspan = 0, endspan = 0
  )
  preds_classic <- predict(fit_classic, newdata = task$X, type = "response")

  # check equality of predictions
  expect_equal(preds, as.numeric(preds_classic))
})

test_that("Test Lrnr_earth earth offset matches earth::earth", {
  covars <- c(
    "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
    "sexn"
  )
  outcome <- "haz"
  task <- sl3_Task$new(cpp_imputed,
                       covariates = covars,
                       outcome = outcome,
                       offset = "waz"
  )
  # get predictions from Lrnr_* wrapper
  set.seed(4738)
  lrnr_earth <- make_learner(Lrnr_earth)
  fit <- lrnr_earth$train(task)
  preds <- fit$predict(task)
  
  # get predictions from classic implementation
  formula <- as.formula(paste(paste(task$nodes$outcome, paste("offset", "(", task$nodes$offset, ")", sep = ""), sep = "~"), paste(task$nodes$covariates, collapse = "+"), sep = "+"))
  fit_classic <- earth::earth(
    formula, data = task$data, degree = 2, penalty = 3,
    pmethod = "backward", nfold = 0, ncross = 1,
    minspan = 0, endspan = 0
  )
  preds_classic <- predict(fit_classic, newdata = task$data, type = "response")
  
  # check equality of predictions
  expect_equal(preds, as.numeric(preds_classic))
})
