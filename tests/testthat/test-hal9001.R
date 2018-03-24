context("test_hal9001.R -- Lrnr_hal9001")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  devtools::check() # runs full check
  setwd("..")
  install("sl3",
    build_vignettes = FALSE,
    dependencies = FALSE
  ) # INSTALL W/ devtools:
}

library(testthat)
library(sl3)
# library(data.table)
# library(origami)
library(SuperLearner)
library(hal9001)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
interactions <- list(c("apgar1", "apgar5"))

task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
task_with_interactions <- task$add_interactions(interactions)

hal_lrnr <- Lrnr_hal9001$new()
sl3_hal <- Lrnr_sl$new(
  learners = list(hal_lrnr),
  metalearner = Lrnr_nnls$new()
)

test_that("Lrnr_hal9001 produces prediction similar to standard hal9001", {
  set.seed(67391)
  hal_lrnr_fit <- hal_lrnr$train(task)
  hal_lrnr_preds <- hal_lrnr_fit$predict()

  set.seed(67391)
  hal_fit <- hal9001::fit_hal(X = as.matrix(task$X), Y = task$Y, yolo = FALSE)
  hal_fit_preds <- predict(hal_fit, new_data = as.matrix(task$X))

  expect_equal(hal_lrnr_preds, expected = hal_fit_preds, tolerance = 1e-15)
})
