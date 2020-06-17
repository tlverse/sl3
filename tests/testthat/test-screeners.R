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
lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
lrnrs <- make_learner(Stack, lrnr_glm, lrnr_mean)

# Coefficient screener
glmnet_screener <- make_learner(Lrnr_screener_coefs, lrnr_glmnet)
debug_train(glmnet_screener)
glmnet_screener_fit <- glmnet_screener$train(task)
glmnet_screener_fit$fit_object$selected

test_that("argument checks throw warnings/errors", {
  expect_error(glmnet_screener$train(task, threshold = NULL, max_retain = NULL), 
               "threshold, max_retain or both must be provided")
  expect_error(glmnet_screener$train(task, min_retain = NULL), 
               "min_retain must be provided")
  expect_error(glmnet_screener$train(task, min_retain = 7), 
               "modify min_retain to be less than the number of covariates")
  expect_warning(glmnet_screener$train(task, min_retain = 9),
                 "max_retain is greater than the number of covariates, modifying max_retain to NULL")
})

glm_screener <- make_learner(Lrnr_screener_coefs, lrnr_glm)
debug_train(glm_screener)
glm_screener_fit <- glm_screener$train(task)
glm_screener_fit$fit_object$selected

# bayesglm_screener <- make_learner(Lrnr_screener_coefs, lrnr_bayesglm)
# debug_train(bayesglm_screener)
# bayesglm_screener_fit <- bayesglm_screener$train(task)
# bayesglm_screener_fit$fit_object$selected

# Correlation P-value Screener
screen_corP <- make_learner(Lrnr_screener_corP)
corP_pipeline <- make_learner(Pipeline, screen_corP, lrnrs)
fit_corP <- corP_pipeline$train(task)

# Correlation Rank Screener
screen_corRank <- make_learner(Lrnr_screener_corRank)
corRank_pipeline <- make_learner(Pipeline, screen_corRank, lrnrs)
fit_corRank <- corRank_pipeline$train(task)

# Random Forest Screener
screen_randomForest <- make_learner(Lrnr_screener_randomForest)
randomForest_pipeline <- make_learner(Pipeline, screen_randomForest, lrnrs)
fit_randomForest <- randomForest_pipeline$train(task)
