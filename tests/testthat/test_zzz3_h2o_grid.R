context("Test h2o grid")

if(FALSE) {
  setwd(".."); setwd(".."); getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  setwd("..");
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
  Sys.setenv(JAVA_HOME="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/")
}


library(testthat)
library(sl3)
library(h2o)
h2o::h2o.init(nthread = 1);
# library(data.table)
# library(origami)
library(SuperLearner)
set.seed(1)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"

task <- Learner_Task$new(cpp, covariates = covars, outcome = outcome)
task$nodes$covariates

options(sl3.verbose = TRUE)

test_that("Lrnr_h2o_grid works with naiveBays for categorical outcome", {
  h2o::h2o.no_progress()
  cpp_hazbin <- cpp
  cpp_hazbin[["haz_cat"]] <- as.factor(rep_len(c(0L,1L,2L), nrow(cpp)))
  task_bin <- Learner_Task$new(cpp_hazbin, covariates = covars, outcome = "haz_cat")

  bays_lrn <- Lrnr_h2o_grid$new(algorithm = "naivebayes")$train(task_bin)
  print(bays_lrn)
  bays_lrn_preds <- bays_lrn$predict()
  expect_true(ncol(bays_lrn_preds)==4)
})

test_that("Lrnr_h2o_grid learner works with a grid of regularized GLMs", {
  h2o::h2o.no_progress()
  h2o_glm_grid <- Lrnr_h2o_grid$new(algorithm = "glm",
                                       hyper_params =
                                        list(alpha = c(0, 0.5, 1)))
  h2o_glm_grid <- h2o_glm_grid$train(task)
  h2oGLM_preds <- h2o_glm_grid$predict()
})

test_that("Lrnr_h2o_grid learner works with a grid of GBMs", {
  h2o::h2o.no_progress()
  glm_fit <- GLM_Learner$new()$train(task)
  glm_preds <- glm_fit$predict()

  h2o_gbm_grid <- Lrnr_h2o_grid$new(algorithm = "gbm",
                                       hyper_params =
                                        list(ntrees = c(10, 20, 50)))
  h2o_gbm_grid <- h2o_gbm_grid$train(task)
  h2ogbm_preds <- h2o_gbm_grid$predict()
})

test_that("stack$predict plays nicely when Learner$predict() is a grid of predictions from several models", {
  h2o::h2o.no_progress()
  glm_learner <- GLM_Learner$new()
  fglm_learner <- GLMfast_Learner$new()

  screen_and_glm <- Pipeline$new(SL_Screener$new("screen.glmnet"), fglm_learner)

  h2o_gbm_grid <- Lrnr_h2o_grid$new(algorithm = "gbm",
                                       hyper_params =
                                        list(ntrees = c(10, 20)))

  SL.glmnet_learner <- SL_Learner$new(SL_wrapper = "SL.glmnet")

  # now lets stack some learners:
  learner_stack <- Stack$new(glm_learner, fglm_learner, screen_and_glm, h2o_gbm_grid, SL.glmnet_learner)
  stack_fit <- learner_stack$train(task)
  preds <- stack_fit$predict()

  expect_true(data.table::is.data.table(preds))
  expect_true(ncol(preds)==6L)
})

test_that("h2o.pca works with pipelines (not checking results)", {
  h2o::h2o.no_progress()

  ## regular GLM
  fglm_learner <- GLMfast_Learner$new()

  ## screen covars in X, then fit GLM on modified (subset of) X:
  screen_and_glm <- Pipeline$new(SL_Screener$new("screen.glmnet"), fglm_learner)

  ## apply PCA to X, then fit GLM on results of PCA
  pca_to_glm <- Pipeline$new(
                  Lrnr_h2o_grid$new(algorithm = "pca", k = 2, impute_missing = TRUE),
                  GLMfast_Learner$new())

  # stack above learners and fit them all:
  learner_stack <- Stack$new(fglm_learner, screen_and_glm, pca_to_glm)
  stack_fit <- learner_stack$train(task)
  preds <- stack_fit$predict()
})

h2o::h2o.shutdown(prompt = FALSE); Sys.sleep(3)