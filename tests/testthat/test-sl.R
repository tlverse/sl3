context("test-sl.R -- Basic Lrnr_sl functionality")
options(sl3.verbose = TRUE)
library(origami)
library(SuperLearner)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(
  data.table::copy(cpp_imputed), covariates = covars,
  outcome = outcome
)
task2 <- sl3_Task$new(
  data.table::copy(cpp_imputed), covariates = covars,
  outcome = outcome
)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
subset_apgar <- Lrnr_subset_covariates$new(covariates = c("apgar1", "apgar5"))
learners <- list(glm_learner, glmnet_learner, subset_apgar)
sl1 <- make_learner(Lrnr_sl, learners, glm_learner)

sl1_fit <- sl1$train(task)
sl1_risk <- sl1_fit$cv_risk(loss_squared_error)

expected_learners <- c(
  "Lrnr_glm", "Lrnr_pkg_SuperLearner_SL.glmnet",
  "Lrnr_subset_covariates_c(\"apgar1\", \"apgar5\")_apgar1",
  "Lrnr_subset_covariates_c(\"apgar1\", \"apgar5\")_apgar5"
)
test_that(
  "sl1_fit is based on the right learners",
  expect_equal(
    sl1_fit$fit_object$cv_meta_task$nodes$covariates,
    expected_learners
  )
)

stack <- make_learner(Stack, learners)
sl2 <- make_learner(Lrnr_sl, stack, glm_learner)

sl2_fit <- sl2$train(task)
sl2_risk <- sl2_fit$cv_risk(loss_squared_error)

test_that(
  "Lrnr_sl can accept a pre-made stack",
  expect_equal(
    sl1_risk$mean_risk, sl2_risk$mean_risk,
    tolerance = 1e-2
  )
)

sl_nnls <- Lrnr_sl$new(
  learners = list(glm_learner, glmnet_learner),
  metalearner = sl3::Lrnr_nnls$new()
)
sl_nnls_fit <- sl_nnls$train(task)

sl1_small <- Lrnr_sl$new(
  learners = list(
    glm_learner, glmnet_learner,
    subset_apgar
  ),
  metalearner = glm_learner, keep_extra = FALSE
)
sl1_small_fit <- sl1_small$train(task)
expect_lt(length(sl1_small_fit$fit_object), length(sl1_fit$fit_object))
