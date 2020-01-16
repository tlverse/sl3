library(testthat)
context("test_sl.R -- Basic Lrnr_sl functionality")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp_imputed), covariates = covars, outcome = outcome)
task2 <- sl3_Task$new(data.table::copy(cpp_imputed), covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
subset_apgar <- Lrnr_subset_covariates$new(covariates = c("apgar1", "apgar5"))
learners <- list(glm_learner, glmnet_learner, subset_apgar)
sl1 <- make_learner(Lrnr_sl, learners, glm_learner)
# sl3_debug_mode()
# debugonce(sl1$.__enclos_env__$private$.train)
# debugonce(sl1$.__enclos_env__$private$.train_sublearners)
sl1_fit <- sl1$train(task)

sl1_risk <- sl1_fit$cv_risk(loss_squared_error)

expected_learners <- c(
  "Lrnr_glm_TRUE", "Lrnr_pkg_SuperLearner_SL.glmnet", "Lrnr_subset_covariates_c(\"apgar1\", \"apgar5\")_apgar1",
  "Lrnr_subset_covariates_c(\"apgar1\", \"apgar5\")_apgar5"
)
test_that("sl1_fit is based on the right learners", expect_equal(sl1_fit$fit_object$cv_meta_task$nodes$covariates, expected_learners))

stack <- make_learner(Stack, learners)
sl2 <- make_learner(Lrnr_sl, stack, glm_learner)

sl2_fit <- sl2$train(task)
sl2_risk <- sl2_fit$cv_risk(loss_squared_error)

test_that("Lrnr_sl can accept a pre-made stack", expect_equal(sl1_risk$mean_risk, sl2_risk$mean_risk, tolerance = 1e-2))

sl_nnls <- Lrnr_sl$new(learners = list(glm_learner, glmnet_learner), metalearner = sl3::Lrnr_nnls$new())
sl_nnls_fit <- sl_nnls$train(task)

sl1_small <- Lrnr_sl$new(learners = list(glm_learner, glmnet_learner, subset_apgar), metalearner = glm_learner, keep_extra = FALSE)
sl1_small_fit <- sl1_small$train(task)
expect_lt(length(sl1_small_fit$fit_object), length(sl1_fit$fit_object))
preds <- sl1_small_fit$predict(task)
preds_fold <- sl1_small_fit$predict_fold(task, "full")
test_that("predict_fold(task,'full') works if keep_extra=FALSE", expect_equal(preds, preds_fold))

# sl of a pipeline from https://github.com/tlverse/sl3/issues/81
data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
make_inter <- Lrnr_define_interactions$new(interactions=list(c("apgar1","parity"),c("apgar5","parity")))

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_glmnet$new(nlambda = 5)
learners = Stack$new(glm_learner, glmnet_learner)
pipe <- Pipeline$new(make_inter, learners)
sl1 <- make_learner(Lrnr_sl, pipe, metalearner = Lrnr_solnp$new())
fit <- sl1$train(task)
