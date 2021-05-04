context("test_Lrnr_stratified.R -- Lrnr_stratified")

set.seed(49753)
library(data.table)
library(dplyr)
library(origami)
library(hal9001)

# load example data set
data(cpp_imputed)

# use covariates of intest and the outcome to build a task object
covars <- c("apgar1", "apgar5", "sexn")
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = "haz")

# TRY stratified lrnr
hal_lrnr <- Lrnr_hal9001$new(fit_control = list(n_folds = 3))
stratified_hal <- Lrnr_stratified$new(
  learner = hal_lrnr,
  variable_stratify = "sexn"
)
stratified_hal_fit <- stratified_hal$train(task)

fit_object <- stratified_hal_fit$fit_object
names(fit_object)

stratified_prediction <- stratified_hal_fit$predict(task = task)

# DOES STACKING WORK?
mean_lrnr <- Lrnr_mean$new()
stratified_mean <- Lrnr_stratified$new(
  learner = mean_lrnr,
  variable_stratify = "sexn"
)
stack_strat <- make_learner(Stack, stratified_mean, stratified_hal)
