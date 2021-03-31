context("test-glmnet.R -- Lrnr_glmnet")

library(origami)
library(data.table)

data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "ses")
task_contY <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
task_binY <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "sexn")

test_that("Lrnr_glmnet with stratify_cv works", {
  lrnr_glmnet <- Lrnr_glmnet$new(stratify_cv = TRUE)
  # expect_warning(ff <- lrnr_glmnet$train(task_contY))
  fit <- lrnr_glmnet$train(task_binY)
  preds <- fit$predict(task_binY)
})
