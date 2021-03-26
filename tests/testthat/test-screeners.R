library(testthat)
context("test_screeners.R -- Screening Procedures")

options(sl3.verbose = TRUE)
library(data.table)
data(cpp_imputed)
setDT(cpp_imputed)
cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
covars <- c("apgar1", "apgar5", "parity_cat", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

task <- sl3_Task$new(data.table::copy(cpp_imputed),
  covariates = covars,
  outcome = outcome
)

lrnr_glmnet <- make_learner(Lrnr_glmnet)
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnrs <- make_learner(Stack, lrnr_glm, lrnr_mean)

########################### coef screener ######################################
glm_screener <- make_learner(Lrnr_screener_coefs, lrnr_glm, max_screen = 2)
glm_screener_pipeline <- make_learner(Pipeline, glm_screener, lrnrs)
fit_glm_screener_pipeline <- glm_screener_pipeline$train(task)
preds_glm_screener_pipeline <- fit_glm_screener_pipeline$predict()
test_that("Lrnr_screener_coefs works selected max_screen no. covs", {
  glm_screener_fit <- glm_screener$train(task)
  selected <- glm_screener_fit$fit_object$selected
  expect_equal(length(selected), 2)
})

test_that("Lrnr_screener_coefs works selected min_screen no. covs", {
  glmnet_screener <- make_learner(Lrnr_screener_coefs, lrnr_glmnet, 
                                  min_screen = 3)
  glmnet_screener_fit <- glmnet_screener0$train(task)
  expect_equal(length(glmnet_screener_fit$fit_object$selected), 3)
})



###########################  correlation screener ##############################
# Correlation P-value Threshold Screener
screen_corP <- make_learner(Lrnr_screener_correlation, type = "threshold")
corP_pipeline <- make_learner(Pipeline, screen_corP, lrnrs)
fit_corP <- corP_pipeline$train(task)
preds_corP_screener <- fit_corP$predict()

# Correlation Rank Screener
screen_corRank <- make_learner(Lrnr_screener_correlation)
corRank_pipeline <- make_learner(Pipeline, screen_corRank, lrnrs)
fit_corRank <- corRank_pipeline$train(task)
preds_corRank_screener <- fit_corRank$predict()

test_that("Lrnr_screener_correlation errors when invalid args provided", {
  expect_error(make_learner(Lrnr_screener_correlation,
    num_screen = NULL,
    pvalue_threshold = 0.1, min_screen = NULL
  ))
  expect_error(make_learner(Lrnr_screener_correlation,
    type = "rank",
    num_screen = NULL
  ))
  expect_error(make_learner(Lrnr_screener_correlation,
    type = "threshold",
    pvalue_threshold = NULL
  ))
})

############################ augment screener ##################################
test_that("Lrnr_screener_augment adds covars to selected set", {
  screener_cor <- make_learner(Lrnr_screener_correlation,
    type = "rank",
    num_screen = 2
  )
  screener_augment <- Lrnr_screener_augment$new(screener_cor, covars)
  screener_fit <- screener_augment$train(task)
  expect_equal(length(screener_fit$fit_object$selected), length(covars))
  expect_equal(length(screener_fit$fit_object$screener_selected), 2)
})

###################### variable importance screener ############################

test_that("Lrnr_screener_importance test with randomForest", {

  # intialize
  lrnr_rf <- make_learner(Lrnr_randomForest)
  screen_randomForest <- make_learner(Lrnr_screener_importance, lrnr_rf,
    num_screen = 3
  )

  # screening fit & preds
  fit_randomForest <- screen_randomForest$train(task)
  selected <- fit_randomForest$fit_object$selected
  expect_equal(length(selected), 3)
  preds <- fit_randomForest$predict(task)
  expect_equal(nrow(preds), nrow(task$data))

  # pipeline fit & preds
  randomForest_pipeline <- make_learner(Pipeline, screen_randomForest, lrnrs)
  fit_randomForest_pipe <- randomForest_pipeline$train(task)
  preds_pipe <- fit_randomForest_pipe$predict(task)
  expect_equal(nrow(preds_pipe), nrow(task$data))
})

test_that("Lrnr_screener_importance test with ranger", {

  # induce error
  lrnr_ranger <- make_learner(Lrnr_ranger)
  fit <- lrnr_ranger$train(task)
  expect_error(fit$importance(task))

  # intialize correctly
  lrnr_ranger <- make_learner(Lrnr_ranger, importance = "impurity")
  screen_ranger <- make_learner(Lrnr_screener_importance, lrnr_ranger,
    num_screen = 3
  )

  # screening fit & preds
  fit_ranger <- screen_ranger$train(task)
  selected <- fit_ranger$fit_object$selected
  expect_equal(length(selected), 3)
  preds <- fit_ranger$predict(task)
  expect_equal(nrow(preds), nrow(task$data))

  # pipeline fit & preds
  ranger_pipeline <- make_learner(Pipeline, screen_ranger, lrnrs)
  fit_ranger_pipe <- ranger_pipeline$train(task)
  preds_pipe <- fit_ranger_pipe$predict(task)
  expect_equal(nrow(preds_pipe), nrow(task$data))
})

test_that("Lrnr_screener_importance test with xgboost", {

  # intialize
  lrnr_xgboost <- make_learner(Lrnr_xgboost, verbose = 0)
  screen_xgboost <- make_learner(Lrnr_screener_importance, lrnr_xgboost,
    num_screen = 3
  )

  # screening fit & preds
  fit_xgboost <- screen_xgboost$train(task)
  selected <- fit_xgboost$fit_object$selected
  expect_equal(length(selected), 3)
  preds <- fit_xgboost$predict(task)
  expect_equal(nrow(preds), nrow(task$data))

  # pipeline fit & preds
  xgboost_pipeline <- make_learner(Pipeline, screen_xgboost, lrnrs)
  fit_xgboost_pipe <- xgboost_pipeline$train(task)
  preds_pipe <- fit_xgboost_pipe$predict(task)
  expect_equal(nrow(preds_pipe), nrow(task$data))
})

test_that("Lrnr_screener_importance throws error if learner not supported", {
  expect_error(Lrnr_screener_importance$new(lrnr_glm))
})
