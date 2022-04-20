context("test-cv-selector.R -- Lrnr_cv_selector for CV-selector metalearner")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  load_all("./")
  devtools::check() # runs full check
  setwd("..")
  # INSTALL W/ devtools:
  install("sl3", build_vignettes = FALSE, dependencies = FALSE)
}

# set seed, load data, and make task
set.seed(32798)
library(data.table)
data(cpp_imputed)
setDT(cpp_imputed)
covs <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
cpp_imputed[, "bin_haz" := as.numeric(haz > mean(haz))]
binary_task <- sl3_Task$new(cpp_imputed,
  covariates = covs, outcome = "bin_haz",
  folds = 3
)
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_lasso <- make_learner(Lrnr_glmnet)

test_that("Lrnr_cv_selector selects the learner that minimizes cv risk", {
  lrnr_mean <- make_learner(Lrnr_mean)
  base_lrnrs <- make_learner(Stack, lrnr_glm, lrnr_lasso, lrnr_mean)
  metalearner <- make_learner(Lrnr_cv_selector)
  sl <- make_learner(Lrnr_sl, base_lrnrs, metalearner)
  task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")

  # train SL and predict
  fit <- sl$train(task)
  preds <- fit$predict()

  # extract learner that minimizes risk
  cv_stack_fit <- fit$fit_object$cv_fit
  stack_risks <- cv_stack_fit$cv_risk(loss_squared_error)
  cv_selector_index <- which.min(stack_risks$MSE)
  fit_lrnrs <-
    fit$fit_object$cv_fit$fit_object$full_fit$fit_object$learner_fits
  cv_selector_fit <- fit_lrnrs[[cv_selector_index]]

  # check predictions of manually extracted learner against the selector
  preds_manual <- cv_selector_fit$predict(task)
  expect_equal(preds, preds_manual)
})

test_that("For risk eval funs fit's cv_risk and calling cv_risk are same", {
  set.seed(458)
  risk_aucpr <- custom_ROCR_risk("aucpr")
  metalrnr <- Lrnr_ga$new(metalearner_logistic_binomial, risk_aucpr,
    verbose = F
  )
  eSL <- Lrnr_sl$new(list(lrnr_glm, lrnr_lasso), metalrnr)
  cv_selector_aucpr <- Lrnr_cv_selector$new(risk_aucpr, binary_task$folds)
  dSL <- Lrnr_sl$new(
    learners = list("glm" = lrnr_glm, "lasso" = lrnr_lasso, "eSL" = eSL),
    metalearner = cv_selector_aucpr
  )
  dSL_fit <- dSL$train(binary_task)
  dSL_tbl <- dSL_fit$cv_risk(risk_aucpr)
  expect_equal(
    as.numeric(dSL_fit$fit_object$cv_meta_fit$fit_object$cv_risk),
    as.numeric(dSL_tbl$aucpr)
  )
})

test_that("Lrnr_cv_selector errors if folds missing for risk eval funs", {
  risk_auc <- custom_ROCR_risk("auc")
  dSL <- Lrnr_sl$new(
    learners = list("glm" = lrnr_glm, "lasso" = lrnr_lasso),
    metalearner = Lrnr_cv_selector$new(risk_auc)
  )
  expect_error(dSL$train(binary_task))
})
