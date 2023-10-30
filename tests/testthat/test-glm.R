context("test-glm.R -- Lrnr_glm")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  devtools::check() # runs full check
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

# library(data.table) library(origami)
set.seed(1)

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

test_that("Lrnr_glm with intercept=FALSE works", {
  lrnr_glm <- make_learner(Lrnr_glm, intercept = FALSE)
  fit <- lrnr_glm$train(task)
  preds <- fit$predict(task)
  expect_equal(task$nrow, length(preds))
})

test_that("Lrnr_glm with formula works", {
  lrnr_glm <- Lrnr_glm$new(formula = as.formula("haz ~ apgar1:apgar5 + I(apgar1^2)"))
  fit <- lrnr_glm$train(task)
  sl3_pred <- fit$predict()
  glm_fit <- glm("haz ~ apgar1:apgar5 + I(apgar1^2)", data = task$data)
  glm_pred <- as.numeric(
    predict(glm_fit, newdata = task$data, type = "response")
  )
  expect_equal(sl3_pred, glm_pred)
})

test_that("Lrnr_glm with formula .^2 works", {
  lrnr_glm <- Lrnr_glm$new(formula = as.formula("~.^2"))
  fit <- lrnr_glm$train(task)
  sl3_pred <- fit$predict()
  glm_fit <- glm("haz ~ .^2", data = task$data)
  glm_pred <- as.numeric(
    predict(glm_fit, newdata = task$data, type = "response")
  )
  expect_equal(sl3_pred, glm_pred)
})

test_that("Lrnr_glm with formula errors when regressors are not task covariates", {
  lrnr_glm <- Lrnr_glm$new(formula = as.formula("haz ~ X"))
  expect_error(fit <- lrnr_glm$train(task))
})

test_that("Lrnr_glm with formula errors when response is not task outcome", {
  lrnr_glm <- Lrnr_glm$new(formula = as.formula("Y ~ apgar1:apgar5"))
  expect_error(fit <- lrnr_glm$train(task))
})
