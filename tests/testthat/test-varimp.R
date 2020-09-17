library(testthat)
context("test_varimp.R -- Variable Importance")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)
library(data.table)
data(cpp_imputed)
setDT(cpp_imputed)
cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
covars <- c("apgar1", "apgar5", "parity_cat", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

task <- sl3_Task$new(data.table::copy(cpp_imputed),
  covariates = covars,
  outcome = outcome,
  folds = origami::make_folds(cpp_imputed, V = 3)
)

lrnr_glmnet <- make_learner(Lrnr_glmnet, nfolds = 3)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glm <- make_learner(Lrnr_glm)
learners <- make_learner(Stack, lrnr_glmnet, lrnr_mean, lrnr_glm)
sl <- make_learner(Lrnr_sl, learners)
fit <- sl$train(task)

permute_imp_ratio <- varimp(fit, loss_squared_error)
permute_imp_diff <- varimp(fit, loss_squared_error, type = "difference")
