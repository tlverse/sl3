context("test_multivariate.R -- Basic Multivariate functionality")

options(sl3.verbose = TRUE)
library(origami)
library(SuperLearner)

n <- 1000
p <- 5
pY <- 3
W <- matrix(rnorm(n * p), nrow = n)
colnames(W) <- sprintf("W%d", seq_len(p))
Y <- matrix(rnorm(n * pY, 0, 0.2) + W[, 1], nrow = n)
colnames(Y) <- sprintf("Y%d", seq_len(pY))
data <- data.table(W, Y)
covariates <- grep("W", names(data), value = TRUE)
outcomes <- grep("Y", names(data), value = TRUE)

# test a single mv learner
task <- sl3_Task$new(data.table::copy(data),
  covariates = covariates,
  outcome = outcomes
)
mv_learner <- make_learner(Lrnr_multivariate, make_learner(Lrnr_glm_fast))
mv_fit <- mv_learner$train(task)
preds <- mv_fit$predict(task)
preds <- unpack_predictions(preds)
test_that("Lrnr_mv preds are the correct dimensions", {
  expect_equal(ncol(preds), pY)
  expect_equal(nrow(preds), n)
})

# test a SL of mv learners
learners <- make_learner_stack(
  "Lrnr_glm_fast", "Lrnr_xgboost",
  "Lrnr_mean"
)$params$learners
mv_learners <- lapply(learners, function(learner) {
  make_learner(Lrnr_multivariate, learner)
})
mv_stack <- make_learner(Stack, mv_learners)

mv_sl <- make_learner(Lrnr_sl, mv_stack)
mv_sl_fit <- mv_sl$train(task)
mv_sl_preds <- mv_sl_fit$predict(task)
mv_sl_preds <- unpack_predictions(mv_sl_preds)
mv_sl_fit$cv_risk(loss_fun = loss_squared_error_multivariate)
test_that("Lrnr_sl for multivariate preds are the correct dimensions", {
  expect_equal(ncol(mv_sl_preds), pY)
  expect_equal(nrow(mv_sl_preds), n)
})

# test for a revere
dummy_revere_function <- function(task, fold_number) {
  return(task)
}
revere_task <- sl3_revere_Task$new(dummy_revere_function, task)
mv_sl_revere_fit <- mv_sl$train(revere_task)
