context("test-bartMachine.R -- Lrnr_bartMachine")

test_that("Lrnr_bartMachine produces warning when java parameters are not set", {
  expect_warning(Lrnr_bartMachine$new())
})


data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = outcome)

test_that("Lrnr_bartMachine produces results matching those of bartMachine::bartMachine", {
  # sl3 fit
  lrnr_bartMachine <- suppressWarnings(Lrnr_bartMachine$new(
    seed = 196, verbose = FALSE
  ))
  fit_sl3 <- lrnr_bartMachine$train(task)
  preds_sl3 <- fit_sl3$predict(task)

  # classic fit
  fit_classic <- bartMachine::bartMachine(
    X = data.frame(task$X), y = task$Y, seed = 196, verbose = FALSE
  )
  preds_classic <- as.numeric(predict(fit_classic, new_data = task$X))

  # check equality
  expect_equal(preds_sl3, preds_classic)
})

# test Lrnr_bartMachine does not fail when cross-validated
lrnr_bartMachine <- suppressWarnings(make_learner(
  Lrnr_bartMachine,
  verbose = FALSE
))
cv_lrnr_bartMachine <- Lrnr_cv$new(lrnr_bartMachine)
fit_cv <- cv_lrnr_bartMachine$train(task)
preds_cv <- fit_cv$predict(task)
