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
library(data.table)
data(cpp_imputed)
setDT(cpp_imputed)

test_that("Lrnr_nnls with convex = TRUE normalizes coefficients to sum to 1", {
  task <- sl3_Task$new(
    cpp_imputed,
    covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"),
    outcome = "haz"
  )
  lrnr_nnls <- make_learner(Lrnr_nnls, convex = TRUE)
  fit <- lrnr_nnls$train(task)
  expect_equal(fit$coefficients, coef(fit), sum(coef(fit) == 1))
})

test_that("Lrnr_nnls prints", {
  lrnr_nnls <- make_learner(Lrnr_nnls)
  expect_output(print(lrnr_nnls))
})


test_that("Lrnr_nnls with binary outcome with convex TRUE works", {
  cpp_imputed[, "haz_binary" := haz > median(haz)]
  task <- sl3_Task$new(
    cpp_imputed,
    covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"),
    outcome = "haz_binary"
  )
  lrnr_nnls <- make_learner(Lrnr_nnls, convex = TRUE)
  fit <- lrnr_nnls$train(task)
  nnls::nnls(as.matrix(task$X), task$Y)
  expect_equal(fit$coefficients, coef(fit), sum(coef(fit) == 1))
})

test_that("Lrnr_nnls coefficients with binary outcome match nnls coefficients", {
  cpp_imputed[, "haz_binary" := haz > median(haz)]
  task <- sl3_Task$new(
    cpp_imputed,
    covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"),
    outcome = "haz_binary"
  )
  lrnr_nnls <- make_learner(Lrnr_nnls)
  sl3_fit <- lrnr_nnls$train(task)
  sl3_fit_coefs <- coef(sl3_fit)
  library(nnls)
  nnls_fit <- nnls::nnls(as.matrix(task$X), task$Y)
  nnls_fit_coefs <- coef(nnls_fit)
  expect_equal(sl3_fit_coefs, nnls_fit_coefs)
})
