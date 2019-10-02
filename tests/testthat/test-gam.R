context("test-gam.R -- Lrnr_gam")

library(mgcv)

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
covars <- c("bmi", "parity", "mage", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

test_that("Lrnr_gam with specifying formula and family works", {
  lrnr_gam <- make_learner(Lrnr_gam, 
                           formula = haz ~ s(bmi) + parity + s(mage) + sexn,
                           family = quasi)
  fit <- lrnr_gam$train(task)
  preds <- fit$predict(task)
  expect_equal(task$nrow, length(preds))
})

test_that("Lrnr_gam without specifying formula gives the predictions 
          that match those from gam", {
  ## instantiate Lrnr_gam, train on task, and predict on task
  lrnr_gam <- Lrnr_gam$new()
  fit_lrnr_gam <- lrnr_gam$train(task)
  prd_lrnr_gam <- fit_lrnr_gam$predict()
  
  ## fit gam using the data from the task
  fit_gam <- mgcv::gam(haz ~ s(bmi) + parity + s(mage) + sexn, 
                       method = "GCV.Cp", data = cpp_imputed)
  prd_gam <- as.numeric(predict(fit_gam, newdata = task$X))
  
  ## test equivalence of prediction from Lrnr_svm and svm::svm
  expect_equal(prd_lrnr_gam, prd_gam)
})
