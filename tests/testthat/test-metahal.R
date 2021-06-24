context("test_metahal.R -- Lrnr_metahal for HAL metalearner")

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
cpp_imputed = cpp_imputed[1:200,]
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
interactions <- list(c("apgar1", "apgar5"))

task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

base_lrnrs <- list()
for (covar in covars) {
  lrnr_subset = Lrnr_subset_covariates$new(covariates = covar)
  learner = make_learner(Pipeline, lrnr_subset, make_learner(sl3::Lrnr_glm))
  base_lrnrs[[covar]] = learner
}

lrnr_metahal <-  sl3::make_learner(sl3::Lrnr_metahal, 
                                   max_degree = 1, 
                                   keep = TRUE, 
                                   type.measure = "mse")
meta_hal <-  sl3::Lrnr_sl$new(learners = base_lrnrs,
                              metalearner = lrnr_metahal)
lrnr_metahal_manual <- sl3::make_learner(sl3::Lrnr_hal9001, 
                                         max_degree = 1, 
                                         keep = TRUE, 
                                         foldid = origami::folds2foldvec(task$folds),
                                         type.measure = "mse")
meta_hal_manual <-  sl3::Lrnr_sl$new(learners = base_lrnrs,
                                     metalearner = lrnr_metahal_manual)

test_that("Lrnr_metahal uses foldid in the task as the default foldid in cv_glmnet", {
  set.seed(67391)
  meta_hal_manual_fit <- meta_hal_manual$train(task)
  meta_hal_manual_preds <- meta_hal_manual_fit$predict()
  
  set.seed(67391)
  meta_hal_fit <- meta_hal$train(task)
  meta_hal_preds <- meta_hal_fit$predict()
  
  expect_equal(meta_hal_preds, expected = meta_hal_manual_preds, tolerance = 1e-15)
})
