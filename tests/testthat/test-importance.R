library(testthat)
context("test_importance.R -- Variable Importance")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(data.table)
data(cpp_imputed)
setDT(cpp_imputed)
cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
covars <- c("apgar1", "parity_cat", "sexn")
outcome <- "haz"

task <- sl3_Task$new(cpp_imputed,
  covariates = covars, outcome = outcome,
  folds = origami::make_folds(cpp_imputed, V = 3)
)

lrnr_glmnet <- make_learner(Lrnr_glmnet, nfolds = 3)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glm <- make_learner(Lrnr_glm)
learners <- make_learner(Stack, lrnr_glmnet, lrnr_mean, lrnr_glm)
sl <- make_learner(Lrnr_sl, learners)
test_that("sl3 importance fails when fit isn't trained", {
  expect_error(importance(sl))
})
fit <- sl$train(task)

# Ensure various implementations of sl3 importance run
remove_validation_risk_ratio <- importance(fit)
remove_full_risk_ratio <- importance(fit, fold_number = "full")
remove_validation_risk_diff <- importance(fit, importance_metric = "difference")
remove_full_risk_diff <- importance(fit,
  fold_number = "full",
  importance_metric = "difference"
)
permute_validation_risk_ratio <- importance(fit, type = "permute")
permute_full_risk_ratio <- importance(fit, type = "permute", fold_number = "full")
permute_validation_risk_diff <- importance(fit,
  type = "permute",
  importance_metric = "difference"
)
permute_full_risk_diff <- importance(fit,
  type = "permute", fold_number = "full",
  importance_metric = "difference"
)

########## test covariate groups
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"
)
task <- sl3_Task$new(cpp_imputed,
  covariates = covars, outcome = "haz",
  folds = origami::make_folds(cpp_imputed, V = 3)
)
fit <- sl$train(task)
groups <- list(
  scores = c("apgar1", "apgar5"),
  maternal = c("parity", "mage", "meducyrs")
)
varimp <- importance(fit, covariate_groups = groups)

test_that("sl3 importance fails when groups with > 1 covariate unnamed", {
  names(groups)[1] <- ""
  expect_error(importance(fit, covariate_groups = groups))
})

test_that("sl3 importance fails when groups don't contain covariates", {
  groups <- c(groups, list("not_a_covariate" = "not_a_covariate"))
  expect_error(importance(fit, covariate_groups = groups))
})
