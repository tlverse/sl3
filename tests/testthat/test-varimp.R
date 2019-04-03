library(testthat)
context("test_varimp.R -- Variable Importance")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp_imputed), covariates = covars, outcome = outcome)
lrnr_glm <- make_learner(Lrnr_glm)
fit <- lrnr_glm$train(task)
permute_imp <- varimp(fit, loss_squared_error, type = "permute")
sample_imp <- varimp(fit, loss_squared_error, type = "sample")
