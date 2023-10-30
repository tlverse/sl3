context("test_screeners.R -- Screening Procedures")

library(data.table)
data(cpp_imputed)
setDT(cpp_imputed)
cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
covars <- c(
  "apgar1", "apgar5", "parity_cat", "gagebrth", "mage", "meducyrs",
  "sexn"
)
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
    min_screen = 2
  )
  glmnet_screener_fit <- glmnet_screener$train(task)
  expect_equal(length(glmnet_screener_fit$fit_object$selected), 2)
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

test_importance_screener <- function(learner) {
  if (learner == "Lrnr_ranger") {
    learner_obj <- make_learner(Lrnr_ranger, importance = "impurity")
  } else {
    learner_obj <- make_learner(learner)
  }
  print(sprintf(
    "Testing importance screener with Learner: %s",
    learner_obj$name
  ))

  importance_screener <- Lrnr_screener_importance$new(learner_obj,
    num_screen = 3
  )

  # screening fit & preds
  fit <- importance_screener$train(task)
  selected <- fit$fit_object$selected
  expect_equal(length(selected), 3)
  preds <- fit$predict(task)
  expect_equal(nrow(preds), nrow(task$data))

  # pipeline fit & preds
  importance_screener_pipeline <- make_learner(
    Pipeline, importance_screener,
    lrnrs
  )
  fit_pipe <- importance_screener_pipeline$train(task)
  preds_pipe <- fit_pipe$predict(task)
  expect_equal(nrow(preds_pipe), nrow(task$data))
}

test_that("Lrnr_screener_importance tests", {
  # get all learners supporting variable importance
  importance_learners <- sl3::sl3_list_learners("importance")

  # remove LightGBM on Windows
  if (Sys.info()["sysname"] == "Windows") {
    importance_learners <-
      importance_learners[!(importance_learners == "Lrnr_lightgbm")]
  }

  # test all learners supporting variable importance
  lapply(importance_learners, test_importance_screener)
})

test_that("Lrnr_screener_importance throws error if learner not supported", {
  expect_error(Lrnr_screener_importance$new(lrnr_glm))
})
