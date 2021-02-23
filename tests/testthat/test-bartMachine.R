context("test-bartMachine.R -- Lrnr_bartMachine")

library(bartMachine)

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

test_that("Lrnr_bartMachine produces results matching those of bartMachine::bartMachine", {
  # get predictions from Lrnr_* wrapper
  lrnr_bartMachine <- make_learner(Lrnr_bartMachine, seed = 123)
  fit <- lrnr_bartMachine$train(task)
  preds <- fit$predict(task)

  # get predictions from classic implementation
  fit_classic <- bartMachine::bartMachine(
    X = data.frame(task$X), y = task$Y, num_trees = 50,
    num_burn_in = 250, seed = 123
  )
  preds_classic <- predict(fit_classic, new_data = task$X)

  # check equality of predictions
  expect_equal(preds, as.numeric(preds_classic))
})
