# test SLs with ROCR risk
library(testthat)
context("test-ROCR_risk.R -- Lrnr_sl functionality with ROCR risks")

library(sl3)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"
)
cpp_imputed$haz_binary <- ifelse(cpp_imputed$haz < mean(cpp_imputed$haz), 0, 1)
task <- sl3_Task$new(
  data.table::copy(cpp_imputed),
  covariates = covars, outcome = "haz_binary"
)

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_xgboost <- make_learner(Lrnr_xgboost)

risk_aucpr <- custom_ROCR_risk("aucpr")
metalrnr_ga <- Lrnr_ga$new(
  learner_function = metalearner_logistic_binomial, eval_function = risk_aucpr
)

sl <- Lrnr_sl$new(
  learners = list(lrnr_glm, lrnr_xgboost), metalearner = metalrnr_ga
)
fit <- sl$train(task)
tbl <- fit$cv_risk(risk_aucpr)
cvSL <- cv_sl(fit, risk_aucpr)

cpp_imputed$weights <- rep(1.5, nrow(cpp_imputed))
cpp_imputed$id <- 1:nrow(cpp_imputed)
task2 <- sl3_Task$new(
  data.table::copy(cpp_imputed),
  covariates = covars, outcome = "haz_binary",
  weights = "weights", id = "id"
)
risk_tpr <- custom_ROCR_risk("tpr", name = "TPR")
lrnr_solnp_tpr <- Lrnr_solnp$new(
  learner_function = metalearner_logistic_binomial, eval_function = risk_tpr
)
sl <- Lrnr_sl$new(
  learners = list(lrnr_glm, lrnr_xgboost), metalearner = lrnr_solnp_tpr
)
fit2 <- sl$train(task2)
varimp <- importance(fit2, risk_tpr, type = "permute")
