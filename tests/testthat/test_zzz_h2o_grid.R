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
h2o.init(nthread = 1); sleep(2)
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

options(sl3.verbose = FALSE)

test_that("h2o_grid_Learner learner works with GLM", {
  h2o::h2o.no_progress()
  glm_fit <- GLM_Learner$new()$train(task)
  glm_preds <- glm_fit$predict()

  h2o_glm_grid <- h2o_grid_Learner$new(algorithm = "glm",
                                       hyper_params =
                                        list(alpha = c(0, 0.5, 1)))
  h2o_glm_grid <- h2o_glm_grid$train(task)
  h2oGLM_preds <- h2o_glm_grid$predict()

  # expect_true(is.vector(h2oGLM_preds))
  # # print(sum(glm_preds-h2oGLM_preds))
  # expect_true(all.equal(as.vector(glm_preds), as.vector(h2oGLM_preds)))
})

test_that("h2o_grid_Learner learner works with GBM", {
  h2o::h2o.no_progress()
  glm_fit <- GLM_Learner$new()$train(task)
  glm_preds <- glm_fit$predict()

  h2o_gbm_grid <- h2o_grid_Learner$new(algorithm = "gbm",
                                       hyper_params =
                                        list(ntrees = c(10, 20, 50)))
  h2o_gbm_grid <- h2o_gbm_grid$train(task)
  h2ogbm_preds <- h2o_gbm_grid$predict()

  # expect_true(is.vector(h2oGLM_preds))
  # # print(sum(glm_preds-h2oGLM_preds))
  # expect_true(all.equal(as.vector(glm_preds), as.vector(h2oGLM_preds)))
})


h2o::h2o.shutdown(prompt = FALSE)
