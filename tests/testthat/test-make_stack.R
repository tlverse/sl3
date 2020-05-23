context("test-make_stack.R -- Convenience function for building stacks.")

# example data and sl3 task
data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)


# create stack of models with convenience function
sl_stack_easy <- make_learner_stack("Lrnr_mean", "Lrnr_glm_fast", "Lrnr_xgboost")


# manually create each learner
sl_mean <- make_learner(Lrnr_mean)
sl_glm_fast <- make_learner(Lrnr_glm_fast)
sl_xgboost <- make_learner(Lrnr_xgboost)
sl_stack_manual <- make_learner(Stack, list(sl_mean, sl_glm_fast, sl_xgboost))


# train both models and compare prediction results
# NOTE: the actual R6 objects produced contain trivial differences (e.g., uuid)
#       that make reduce testing exactness to a comparison of predictions.
sl_stack_easy_fit <- sl_stack_easy$train(task)
sl_stack_easy_fit_pred <- sl_stack_easy_fit$predict()

sl_stack_manual_fit <- sl_stack_manual$train(task)
sl_stack_manual_fit_pred <- sl_stack_manual_fit$predict()

test_that("Automatic and manually made learner stacks produce same preds", {
  expect_equal(sl_stack_easy_fit_pred, sl_stack_manual_fit_pred)
})


# easily construct a Stack while passing in extra arguments to some learners
sl_stack <- make_learner_stack(
  "Lrnr_mean",
  list("Lrnr_density_semiparametric", mean_learner = make_learner(Lrnr_glm))
)

# create density learner manually, train, and compare predictions
hse_density <- make_learner(Lrnr_density_semiparametric,
  mean_learner = make_learner(Lrnr_glm)
)

hse_density_fit <- hse_density$train(task)
hse_density_fit_pred <- hse_density_fit$predict()

sl_stack_hse_density <- sl_stack$params$learners[[2]]
sl_stack_hse_density_fit <- sl_stack_hse_density$train(task)
sl_stack_hse_density_fit_pred <- sl_stack_hse_density_fit$predict()

test_that("Learner from automatic stack behaves same as a standard learner", {
  expect_equal(sl_stack_hse_density_fit_pred, hse_density_fit_pred)
})
