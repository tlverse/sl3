context("test-gam.R -- Lrnr_gam")

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

test_that("Lrnr_gam without specifying formula works", {
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
  lrnr_gam <- make_learner(Lrnr_gam)
  fit <- lrnr_gam$train(task)
  preds <- fit$predict(task)
  expect_equal(task$nrow, length(preds))
})
