context("test-timeout.R -- Learner timeouts")
library(origami)
library(SuperLearner)


data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

mean_learner <- Lrnr_mean$new()
glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_glmnet$new()
xgb_learner <- Lrnr_xgboost$new(nrounds=20)
learners <- list(mean = mean_learner,
                 glm = glm_learner, 
                 glmnet = glmnet_learner,
                 xgb =xgb_learner)
stack <- Stack$new(learners)
sl <- Lrnr_sl$new(learners)
system.time({fit <- sl$train(task)},gcFirst = FALSE)
# fit <- sl$train(task)
fit$runtime
# debugonce(fit$cv_risk)
fit$cv_risk(loss_squared_error)

