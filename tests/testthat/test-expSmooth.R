context("test-expSmooth.R -- Lrnr_expSmooth")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  load_all("./")
  # devtools::check() # runs full check
  setwd("..")
  # INSTALL W/ devtools:
  install("sl3", build_vignettes = FALSE, dependencies = FALSE)
}

set.seed(1)

data(bsds)
covars <- c("cnt")
outcome <- "cnt"
n_ahead_param <- 5

folds <- origami::make_folds(bsds,
  fold_fun = folds_rolling_window, window_size = 50,
  validation_size = 10, gap = 0, batch = 200
)
task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome, folds = folds)

test_that("Automatic Lrnr_expSmooth gives expected values", {
  expSmooth_learner <- Lrnr_expSmooth$new(n.ahead = 5)
  expSmooth_fit <- expSmooth_learner$train(task)
  expSmooth_preds <- expSmooth_fit$predict(task)

  expSmooth_fit_2 <- forecast::ets(bsds$cnt)
  expSmooth_preds_2 <- forecast::forecast(expSmooth_fit_2, h = n_ahead_param)
  expSmooth_preds_2 <- as.numeric(expSmooth_preds_2$mean)
  expSmooth_preds_2 <- structure(
    expSmooth_preds_2,
    names = seq_len(n_ahead_param)
  )

  # predictions should be exactly the same
  expect_equal(expSmooth_preds_2, expSmooth_preds)

  expect_equal(
    expSmooth_preds, expSmooth_preds_2,
    tolerance = 1e-10,
    scale = 1
  )
})
