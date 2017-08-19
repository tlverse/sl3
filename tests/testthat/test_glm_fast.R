context("Test Fast GLM")

if (FALSE) {
    setwd("..")
    setwd("..")
    getwd()
    library("devtools")
    document()
    load_all("./")  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
    devtools::check() # runs full check
    setwd("..")
    install("sl3", build_vignettes = FALSE, dependencies = FALSE)  # INSTALL W/ devtools:
}

library(testthat)
library(sl3)
# library(data.table)
# library(origami)
library(SuperLearner)
set.seed(1)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
task$nodes$covariates

test_that("Lrnr_glm and Learenr_GLMfast learners give the same predictions", {
  glm_learner <- Lrnr_glm$new()
  fglm_learner <- Lrnr_glm_fast$new()
  GLM_fit <- glm_learner$train(task)
  glm_preds <- GLM_fit$predict()
  fGLM_fit <- fglm_learner$train(task)
  fglm_preds <- fGLM_fit$predict()
  expect_true(data.table::is.data.table(fglm_preds))
  expect_true(all.equal(as.vector(glm_preds), as.vector(fglm_preds[["predictions"]])))
})

test_that("Lrnr_glm_fast trains on a subset of covariates (predictors)", {
  fglm_learner <- Lrnr_glm_fast$new(covariates = c("apgar1", "apgar5"))
  fGLM_fit <- fglm_learner$train(task)
  # print(fGLM_fit)
  # str(fGLM_fit$params)
  fglm_preds_2 <- fGLM_fit$predict()
  expect_true(data.table::is.data.table(fglm_preds_2))

  glm.fit <- glm(haz ~ apgar1 + apgar5, data = cpp, family = stats::gaussian())
  glm_preds_2 <- as.vector(predict(glm.fit))

  expect_true(sum(fglm_preds_2 - glm_preds_2) < 10^(-10), )
  expect_true(all.equal(as.vector(glm_preds_2), as.vector(fglm_preds_2[["predictions"]])))
})

test_that("Lrnr_glm_fast defines interactions", {
  fglm_learner <- Lrnr_glm_fast$new(covariates = c("apgar1", "apgar5"),
                                      interactions = list(c("apgar1", "apgar5")))
  fGLM_fit <- fglm_learner$train(task)
  # print(fGLM_fit)
  # str(fGLM_fit$params)
  fglm_preds_3 <- fGLM_fit$predict()
  expect_true(data.table::is.data.table(fglm_preds_3))

  glm.fit <- glm(haz ~ apgar1 + apgar5 + apgar1:apgar5, data = cpp, family = stats::gaussian())
  # print(glm.fit)
  glm_preds_3 <- as.vector(predict(glm.fit))

  expect_true(sum(fglm_preds_3 - glm_preds_3) < 10^(-10))
  expect_true(all.equal(as.vector(glm_preds_3), as.vector(fglm_preds_3[["predictions"]])))
})

test_that("Lrnr_glm_fast works with screener", {
  # example of learner chaining
  slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")

  ## FAILS, because screener currently renames the covariates
  # fglm_learner <- Lrnr_glm_fast$new(covariates = c("apgar1", "meducyrs"),
  #                                     interactions = list(c("apgar1", "meducyrs")))
  fglm_learner <- Lrnr_glm_fast$new()
  screen_and_glm <- Pipeline$new(slscreener, fglm_learner)
  sg_fit <- screen_and_glm$train(task)
  preds <- sg_fit$predict()
  # print(sg_fit)
})

test_that("Lrnr_glm_fast works with stacking", {
  glm_learner <- Lrnr_glm$new()
  fglm_learner <- Lrnr_glm_fast$new()
  screen_and_glm <- Pipeline$new(Lrnr_pkg_SuperLearner_screener$new("screen.glmnet"), fglm_learner)
  SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")

  # now lets stack some learners
  learner_stack <- Stack$new(glm_learner, fglm_learner, screen_and_glm, SL.glmnet_learner)
  stack_fit <- learner_stack$train(task)
  # print(stack_fit)
  preds <- stack_fit$predict()
  # print(head(preds))
})

test_that("Lrnr_glm_fast works with quasibinomial and continuous outcomes in (0,1)", {
  cpp_haz_01range <- cpp
  cpp_haz_01range[["haz_01range"]] <- rep_len(c(0.1,0.9), nrow(cpp))
  task_01range <- sl3_Task$new(cpp_haz_01range, covariates = covars, outcome = "haz_01range")

  fglm_learner <- Lrnr_glm_fast$new(family = "quasibinomial")
  fGLM_fit <- fglm_learner$train(task_01range)
  # print(fGLM_fit)

  fglm_learner <- Lrnr_glm_fast$new(family = "binomial")
  fGLM_fit <- fglm_learner$train(task_01range)
  # print(fGLM_fit)
})

test_that("Lrnr_glm_fast works with different families ('family = ...') and solvers ('method = ...')", {
  cpp_hazbin <- cpp
  cpp_hazbin[["haz_bin"]] <- rep_len(c(0L,1L), nrow(cpp))
  task_bin <- sl3_Task$new(cpp_hazbin, covariates = covars, outcome = "haz_bin")

  fglm_learner <- Lrnr_glm_fast$new(family = "quasibinomial")
  fGLM_fit <- fglm_learner$train(task_bin)
  # print(fGLM_fit)

  fglm_learner <- Lrnr_glm_fast$new(family = "binomial")
  fGLM_fit <- fglm_learner$train(task_bin)
  # print(fGLM_fit)

  fglm_learner <- Lrnr_glm_fast$new(family = "binomial", method = 'eigen')
  fGLM_fit <- fglm_learner$train(task_bin)
  # print(fGLM_fit)

  fglm_learner <- Lrnr_glm_fast$new(family = "binomial", method = 'Cholesky')
  fGLM_fit <- fglm_learner$train(task_bin)
  # print(fGLM_fit)

  fglm_learner <- Lrnr_glm_fast$new(family = "binomial", method = 'qr')
  fGLM_fit <- fglm_learner$train(task_bin)
  # print(fGLM_fit)
})

test_that("When speedglm fails (singlular X) the fallback glm works", {
  op <- options(sl3.verbose = TRUE)
  ## make a singular X for testing:
  set.seed(123456)
  dat_test <- data.frame(Y = rep(0L,100), X1 = rnorm(100), X2 = rnorm(100))
  dat_test <- cbind(dat_test, X3 = dat_test[["X1"]] + dat_test[["X2"]])
  task_all <- sl3_Task$new(dat_test, covariates = c("X1", "X2", "X3"), outcome = "Y")
  glm_lrnr <- Lrnr_glm$new()$train(task_all)
  fglm_lrnr <- Lrnr_glm_fast$new(method = "Cholesky")$train(task_all)
  glm_preds <- glm_lrnr$predict()
  fglm_preds <- fglm_lrnr$predict()
  expect_true(all.equal(as.vector(glm_preds), as.vector(fglm_preds[["predictions"]])))
  options(op)
})
