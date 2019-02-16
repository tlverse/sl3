context("test-print-keep_extra.R -- keep_extra doesn't interfere with printing")
library(origami)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

fglm <- Lrnr_glm_fast$new()
xgb <- Lrnr_xgboost$new()
sl_small <- Lrnr_sl$new(
  learners = Stack$new(fglm, xgb),
  metalearner = Lrnr_nnls$new(),
  keep_extra = FALSE
)
sl_large <- Lrnr_sl$new(
  learners = Stack$new(fglm, xgb),
  metalearner = Lrnr_nnls$new(),
  keep_extra = TRUE
)

fit_small <- sl_small$train(task)
fit_large <- sl_large$train(task)

fit_small
fit_large
