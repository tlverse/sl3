options(sl3.verbose = TRUE)
library(sl3)
library(testthat)
library(origami)
library(SuperLearner)
context("Lrnr_sl Test")

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp), covariates = covars, outcome = outcome)
task2 <- sl3_Task$new(data.table::copy(cpp), covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
subset_apgar <- Lrnr_subset_covariates$new(covariates=c("apgar1","apgar5"))
sl1 <- Lrnr_sl$new(learners = list(glm_learner, glmnet_learner, subset_apgar), metalearner = glm_learner)
# delayed_fit <- delayed_learner_train(sl1,task)
# plot(delayed_fit)

sl1_fit <- sl1$train(task)
sl1_fit$predict()
sl1_fit$predict(task2)
full_stack <- sl1_fit$fit_object$full_fit$fit_object$learner_fits[[1]]
full_stack$base_chain(task2)
suppressWarnings({sl1_risk <- sl1_fit$cv_risk(loss_squared_error) })
stack <- Stack$new(glm_learner, glmnet_learner, subset_apgar)
sl2 <- Lrnr_sl$new(learners = stack, metalearner = glm_learner)
sl2_fit <- sl2$train(task)
suppressWarnings({sl2_risk <- sl2_fit$cv_risk(loss_squared_error) })

test_that("Lrnr_sl can accept a pre-made stack", expect_equal(sl1_risk$mean,sl2_risk$mean, tolerance = 1e-2))

sl_nnls <- Lrnr_sl$new(learners = list(glm_learner, glmnet_learner), metalearner = sl3::Lrnr_nnls$new())
sl_nnls_fit <- sl_nnls$train(task)
print(sl_nnls_fit)