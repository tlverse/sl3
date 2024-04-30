context("test_hal9001.R -- Lrnr_hal9001")
library(hal9001)
skip_on_cran()
if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data.
  # Ignores NAMESPACE:
  devtools::check() # runs full check
  setwd("..")
  install("sl3",
    build_vignettes = FALSE,
    dependencies = FALSE
  ) # INSTALL W/ devtools:
}

test_that("Lrnr_hal9001 predictions match those from hal9001", {
  data(cpp_imputed)
  covars <- c(
    "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
    "sexn"
  )
  outcome <- "haz"
  interactions <- list(c("apgar1", "apgar5"))
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

  # initialize, fit, predict with Lrnr_hal9001
  set.seed(67391)
  hal_lrnr <- Lrnr_hal9001$new()
  hal_lrnr_fit <- hal_lrnr$train(task)
  hal_lrnr_preds <- hal_lrnr_fit$predict()

  # fit and predict with hal9001
  set.seed(67391)
  hal_fit <- hal9001::fit_hal(
    X = as.matrix(task$X), Y = task$Y,
    fit_control = list(foldid = origami::folds2foldvec(task$folds)),
    max_degree = 2, smoothness_orders = 1, num_knots = 5
  )
  hal_fit_preds <- predict(hal_fit, new_data = as.matrix(task$X))

  # check equality of predictions
  expect_equal(hal_lrnr_preds, expected = hal_fit_preds, tolerance = 1e-15)
})

test_that("Lrnr_hal9001 passes arguments correctly relative to hal9001", {
  # NOTE: this replicates a bug with the creation of cross-validation folds for
  #       that was resolved in https://github.com/tlverse/hal9001/pull/83
  data(mtcars)
  mtcars_task <- sl3_Task$new(
    data = mtcars,
    covariates = c(
      "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb"
    ),
    outcome = "mpg"
  )

  # initialize, fit, predict with Lrnr_hal9001
  set.seed(31298)
  hal_lrnr <- Lrnr_hal9001$new(
    max_degree = 2,
    smoothness_orders = 0,
    num_knots = 5
  )
  hal_lrnr_fit <- hal_lrnr$train(mtcars_task)
  hal_lrnr_preds <- hal_lrnr_fit$predict()

  # fit and predict with hal9001
  set.seed(31298)
  hal_fit <- hal9001::fit_hal(
    X = as.matrix(mtcars_task$X),
    Y = mtcars_task$Y,
    max_degree = 2,
    smoothness_orders = 0,
    num_knots = 5,
    fit_control = list(foldid = origami::folds2foldvec(mtcars_task$folds))
  )
  hal_fit_preds <- predict(hal_fit, new_data = as.matrix(mtcars_task$X))

  # check equality of predictions
  expect_equal(hal_lrnr_preds, expected = hal_fit_preds, tolerance = 1e-15)
})
