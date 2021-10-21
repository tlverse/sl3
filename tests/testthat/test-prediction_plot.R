context("test-prediction_plot.R -- prediction plots")

library(origami)
library(data.table)

data(cpp_imputed)
setDT(cpp_imputed)
cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
covars <- c("apgar1", "parity_cat", "sexn")
outcome <- "haz"
cpp_imputed[, haz_cat := factor(ifelse(haz < 0, "less0", ifelse(haz < 1, "less1", "over1")))]
cpp_imputed[, haz_bin := haz < mean(haz)]
folds <- origami::make_folds(cpp_imputed, V = 3)

task_con <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = "haz", folds = folds)
task_bin <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = "haz_bin", folds = folds)
task_cat <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = "haz_cat", folds = folds)

lrnr_mean <- make_learner(Lrnr_mean)
lrnr_rf <- make_learner(Lrnr_ranger)
lrnr_lasso <- make_learner(Lrnr_glmnet)
meta_cat <- make_learner(Lrnr_cv_selector, loss_loglik_multinomial)
sl <- Lrnr_sl$new(list(lrnr_mean, lrnr_rf, lrnr_lasso))
sl_cat <- Lrnr_sl$new(list(lrnr_mean, lrnr_rf, lrnr_lasso), meta_cat)
sl_fit_con <- sl$train(task_con)
sl_fit_bin <- sl$train(task_bin)
sl_fit_cat <- sl_cat$train(task_cat)

prediction_plot(sl_fit_con)
prediction_plot(sl_fit_bin)
prediction_plot(sl_fit_cat)
