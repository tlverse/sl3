library(testthat)
context("test_screeners.R -- Screening Procedures")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)
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

# Coef screener
glmnet_screener <- make_learner(Lrnr_screener_coefs, lrnr_glmnet)
glmnet_screener_fit <- glmnet_screener$train(task)
glmnet_screener_fit$fit_object$selected

glm_screener <- make_learner(Lrnr_screener_coefs, lrnr_glm, max_retain = 2)
glm_screener_pipeline <- make_learner(Pipeline, glm_screener, lrnrs)
fit_glm_screener_pipeline <- glm_screener_pipeline$train(task)
preds_glm_screener_pipeline <- fit_glm_screener_pipeline$predict()
test_that("Lrnr_screener_coefs works selected max_retain no. covs", {
  glm_screener_fit <- glm_screener$train(task)
  selected <- glm_screener_fit$fit_object$selected
  expect_equal(length(selected), 2)
})

# Correlation P-value Screener
screen_corP <- make_learner(Lrnr_screener_corP)
corP_pipeline <- make_learner(Pipeline, screen_corP, lrnrs)
fit_corP <- corP_pipeline$train(task)
preds_corP_screener <- fit_corP$predict()

# Correlation Rank Screener
screen_corRank <- make_learner(Lrnr_screener_corRank)
corRank_pipeline <- make_learner(Pipeline, screen_corRank, lrnrs)
fit_corRank <- corRank_pipeline$train(task)
preds_corRank_screener <- fit_corRank$predict()

# Random Forest Screener
screen_randomForest <- make_learner(Lrnr_screener_randomForest)
randomForest_pipeline <- make_learner(Pipeline, screen_randomForest, lrnrs)
fit_randomForest <- randomForest_pipeline$train(task)
preds <- fit_randomForest$predict(task)

# Augment Screener
test_that("Lrnr_screener_augment adds covars to selected set", {
  screener_cor <- make_learner(Lrnr_screener_corRank, rank = 2)
  screener_augment <- Lrnr_screener_augment$new(screener_cor, covars)
  screener_fit <- screener_augment$train(task)
  expect_equal(length(screener_fit$fit_object$selected), length(covars))
  expect_equal(length(screener_fit$fit_object$screener_selected), 2)
})
