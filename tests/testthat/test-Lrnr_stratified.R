context("test_Lrnr_stratified.R -- Lrnr_stratified")

set.seed(49753)
suppressMessages(library(data.table))
library(dplyr)
library(origami)
library(sl3)
library(hal9001)

# load example data set
data(cpp)
cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

# use covariates of intest and the outcome to build a task object
covars <- c("apgar1", "apgar5", "sexn")
task <- sl3_Task$new(cpp, covariates = covars, outcome = "haz")

# TRY stratified lrnr
hal_lrnr <- Lrnr_hal9001$new(fit_type = "glmnet", n_folds = 3, use_min = TRUE)
stratified_hal <- Lrnr_stratified$new(lrnr = hal_lrnr, variable_stratify = 'sexn')
stratified_hal_fit <- stratified_hal$train(task)

fit_object <- stratified_hal_fit$fit_object
names(fit_object)

stratified_prediction <- stratified_hal_fit$predict(task = task)
