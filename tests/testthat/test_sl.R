library(sl3)
library(testthat)
library(origami)
library(SuperLearner)


data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
stack <- Stack$new(glm_learner, glmnet_learner)
stack2 <- Stack$new(stack)

nnls_learner <- Lrnr_nnls$new()
sl1 <- Lrnr_sl$new(learners = list(glm_learner, glmnet_learner), metalearner = nnls_learner)
sl1_fit <- sl1$train(task)
sl1_risk <- sl1_fit$cv_risk(loss_squared_error) 

sl2 <- Lrnr_sl$new(learners = stack, metalearner = nnls_learner)
sl2_fit <- sl2$train(task)
sl2_risk <- sl2_fit$cv_risk(loss_squared_error) 


test_that("Lrnr_sl can accept a pre-made stack", expect_equal(sl1_risk$mean,sl2_risk$mean, tolerance = 1e-3))
