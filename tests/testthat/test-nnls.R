context("test-nnls.R -- Lrnr_nnls")

library(nnls)

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

test_that("Lrnr_nnls with convex = TRUE normalizes coefficients to sum to 1", {
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
  lrnr_nnls <- make_learner(Lrnr_nnls, convex = TRUE)
  fit <- lrnr_nnls$train(task)
  expect_equal(fit$coefficients, coef(fit), sum(coef(fit) == 1))
})

test_that("Lrnr_nnls prints", {
  lrnr_nnls <- make_learner(Lrnr_nnls)
  expect_output(print(lrnr_nnls))
})
