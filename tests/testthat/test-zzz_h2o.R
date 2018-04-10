context("test-zzz_h2o.R -- h2o learners")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
  Sys.setenv(JAVA_HOME = "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/")
}


library(h2o)
h2o.init(nthread = 1)
Sys.sleep(3)
# library(data.table)
# library(origami)
# library(SuperLearner)
set.seed(1)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
cpp_imputed <- cpp_imputed[1:150, ]
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
task2 <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
task$nodes$covariates

test_learner <- function(learner, task, ...) {
  learner_obj <- learner$new(...)
  print(sprintf("Testing Learner: %s", learner_obj$name))
  # test learner training
  fit_obj <- learner_obj$train(task)
  test_that("Learner can be trained on data", expect_true(fit_obj$is_trained))
  # test learner prediction
  train_preds <- fit_obj$predict()
  test_that("Learner can generate training set predictions", expect_equal(
    sl3:::safe_dim(train_preds)[1],
    nrow(task$X)
  ))
  holdout_preds <- fit_obj$predict(task2)
  test_that("Learner can generate holdout set predictions", expect_equal(
    train_preds,
    holdout_preds
  ))
  # test learner chaining
  chained_task <- fit_obj$chain()
  test_that("Chaining returns a task", expect_true(is(
    chained_task,
    "sl3_Task"
  )))
  test_that("Chaining returns the correct number of rows", expect_equal(
    nrow(chained_task$X),
    nrow(task$X)
  ))
}

test_learner(Lrnr_h2o_glm, task)
test_learner(Lrnr_h2o_grid, task, algorithm = "glm")
test_learner(Lrnr_h2o_grid, task, algorithm = "gbm")
test_learner(Lrnr_h2o_grid, task, algorithm = "randomForest")
test_learner(Lrnr_h2o_grid, task, algorithm = "kmeans")
test_learner(Lrnr_h2o_grid, task, algorithm = "deeplearning")

## test h2o classifiers and mutator:
test_learner(Lrnr_h2o_classifier, task, algorithm = "naivebayes")
test_learner(Lrnr_h2o_mutator, task, algorithm = "pca", k = 3, impute_missing = TRUE)

test_that("Lrnr_glm and Lrnr_h2o_glm learners give the same predictions", {
  glm_learner <- Lrnr_glm$new()
  h2o_glm <- Lrnr_h2o_glm$new()
  GLM_fit <- glm_learner$train(task)
  glm_preds <- GLM_fit$predict()
  h2oGLM_fit <- h2o_glm$train(task)
  h2oGLM_preds <- h2oGLM_fit$predict()
  expect_true(data.table::is.data.table(h2oGLM_preds))
  # print(sum(glm_preds-h2oGLM_preds))
  expect_true(all.equal(as.vector(glm_preds), as.vector(h2oGLM_preds[[1]])))
})

test_that("Lrnr_h2o_glm trains based on a subset of covariates (predictors) and defines interactions", {
  h2o_glm <- Lrnr_h2o_glm$new(
    covariates = c("apgar1", "apgar5", "parity"),
    interactions = c("apgar1", "apgar5")
  )
  h2oGLM_fit <- h2o_glm$train(task)
  h2oGLM_preds_3 <- h2oGLM_fit$predict()
  expect_true(data.table::is.data.table(h2oGLM_preds_3))
  glm.fit <- glm(haz ~ apgar1 + apgar5 + parity + apgar1:apgar5, data = cpp_imputed, family = stats::gaussian())
  # print(glm.fit)
  glm_preds_3 <- as.vector(predict(glm.fit))
  expect_true(sum(h2oGLM_preds_3 - glm_preds_3) < 10^(-10))
  expect_true(all.equal(as.vector(glm_preds_3), as.vector(h2oGLM_preds_3[[1]])))
})

test_that("Lrnr_h2o_grid learner works with a grid of regularized GLMs, on a subset of covariates (predictors) and defines interactions", {
  h2o_glm_grid <- Lrnr_h2o_grid$new(
    algorithm = "glm",
    covariates = c("apgar1", "apgar5", "parity"),
    interactions = c("apgar1", "apgar5"),
    hyper_params = list(alpha = c(0, 0.5, 1)),
    lambda_search = TRUE
  )
  h2o_glm_grid <- h2o_glm_grid$train(task)
  h2oGLM_preds <- h2o_glm_grid$predict()
})

# test_that("Lrnr_h2o_glm works with screener", {
#     h2o::h2o.no_progress()
#     # example of learner chaining
#     slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")

#     ## FAILS, because screener currently renames the covariates h2o_glm <-
#     ## Lrnr_h2o_glm$new(covariates = c('apgar1', 'meducyrs'), interactions =
#     ## list(c('apgar1', 'meducyrs')))
#     h2o_glm <- Lrnr_h2o_glm$new()
#     screen_and_glm <- Pipeline$new(slscreener, h2o_glm)
#     sg_fit <- screen_and_glm$train(task)
#     # print(sg_fit)
# })

# test_that("Lrnr_h2o_glm works with stacking", {
#     h2o::h2o.no_progress()
#     glm_learner <- Lrnr_glm$new()
#     h2o_glm <- Lrnr_h2o_glm$new()
#     screen_and_glm <- Pipeline$new(Lrnr_pkg_SuperLearner_screener$new("screen.glmnet"),
#         h2o_glm)
#     SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")

#     # now lets stack some learners
#     learner_stack <- Stack$new(glm_learner, h2o_glm, screen_and_glm, SL.glmnet_learner)
#     stack_fit <- learner_stack$train(task)
#     # print(stack_fit)
#     preds <- stack_fit$predict()
#     expect_true(data.table::is.data.table(preds))
#     # print(head(preds))
# })

# ## quasibinomial is broken is all recent releases of h2o test_that('Lrnr_h2o_glm
# ## works with quasibinomial and continuous outcomes in (0,1)', {
# ## h2o::h2o.no_progress() cpp_haz_01range <- cpp cpp_haz_01range[['haz_01range']]
# ## <- rep_len(c(.1,.2,.3,.4,.5,.6,.7,.8,.9), nrow(cpp)) task_01range <-
# ## sl3_Task$new(cpp_haz_01range, covariates = covars, outcome = 'haz_01range')

# # h2o_glm <- Lrnr_h2o_glm$new(family = 'quasibinomial') h2oGLM_fit <-
# # h2o_glm$train(task_01range) preds_1 <- h2oGLM_fit$predict() # print(h2oGLM_fit)

# # fglm_learner <- Lrnr_glm_fast$new(family = 'quasibinomial') fglm_fit <-
# # fglm_learner$train(task_01range) fglm_preds_2 <- fglm_fit$predict()

# # h2o_glm <- Lrnr_h2o_glm$new(family = 'binomial') expect_error( h2oGLM_fit <-
# # h2o_glm$train(task_01range) ) })

# ## quasibinomial is broken is all recent releases of h2o
# test_that("Lrnr_h2o_glm works with binomial families for binary outcome and gives the same result as speedglm",
#     {
#         h2o::h2o.no_progress()
#         cpp_hazbin <- cpp
#         cpp_hazbin[["haz_bin"]] <- rep_len(c(0L, 1L), nrow(cpp))
#         task_bin <- sl3_Task$new(cpp_hazbin, covariates = covars, outcome = "haz_bin")

#         # h2o_glm <- Lrnr_h2o_glm$new(family = 'quasibinomial') h2oGLM_fit <-
#         # h2o_glm$train(task_bin) preds_1 <- h2oGLM_fit$predict() # print(h2oGLM_fit)

#         fglm_learner <- Lrnr_glm_fast$new(family = "quasibinomial")
#         fglm_fit <- fglm_learner$train(task_bin)
#         fglm_preds_2 <- fglm_fit$predict()

#         h2o_glm <- Lrnr_h2o_glm$new(family = "binomial")
#         h2oGLM_fit <- h2o_glm$train(task_bin)
#         preds_2 <- h2oGLM_fit$predict()

#         expect_true(all.equal(fglm_preds_2[[1]], preds_2[[1]]))
#         # print(h2oGLM_fit) expect_true(all.equal(preds_1, preds_2))
#     })

# test_that("Lrnr_h2o_glm works with different solvers", {
#     h2o::h2o.no_progress()
#     cpp_hazbin <- cpp
#     cpp_hazbin[["haz_bin"]] <- rep_len(c(0L, 1L), nrow(cpp))
#     task_bin <- sl3_Task$new(cpp_hazbin, covariates = covars, outcome = "haz_bin")

#     h2o_glm <- Lrnr_h2o_glm$new(family = "binomial", solver = "L_BFGS")
#     h2oGLM_fit <- h2o_glm$train(task_bin)
#     preds_1 <- h2oGLM_fit$predict()
#     # print(h2oGLM_fit)

#     h2o_glm <- Lrnr_h2o_glm$new(family = "binomial", solver = "IRLSM")
#     h2oGLM_fit <- h2o_glm$train(task_bin)
#     preds_1 <- h2oGLM_fit$predict()
#     # print(h2oGLM_fit)

#     h2o_glm <- Lrnr_h2o_glm$new(family = "binomial", solver = "COORDINATE_DESCENT")
#     h2oGLM_fit <- h2o_glm$train(task_bin)
#     preds_1 <- h2oGLM_fit$predict()
#     # print(h2oGLM_fit)

#     h2o_glm <- Lrnr_h2o_glm$new(family = "binomial", solver = "COORDINATE_DESCENT_NAIVE")
#     h2oGLM_fit <- h2o_glm$train(task_bin)
#     preds_1 <- h2oGLM_fit$predict()
#     # print(h2oGLM_fit)
# })

test_that("Lrnr_h2o_glm works with regularized regression and internal CV for lambda", {
  h2o::h2o.no_progress()
  cpp_hazbin <- cpp_imputed
  cpp_hazbin[["haz_bin"]] <- rep_len(c(0L, 1L), nrow(cpp_imputed))
  task_bin <- sl3_Task$new(cpp_hazbin, covariates = covars, outcome = "haz_bin")
  h2o_glm <- Lrnr_h2o_glm$new(
    family = "binomial", alpha = 0.5, lambda_search = TRUE,
    nlambdas = 20, nfolds = 5
  )
  h2oGLM_fit <- h2o_glm$train(task_bin)
  preds_1 <- h2oGLM_fit$predict()
  # print(h2oGLM_fit)
})

test_that("Lrnr_h2o_classifier works with naiveBays for categorical outcome", {
  h2o::h2o.no_progress()
  cpp_hazbin <- cpp_imputed
  cpp_hazbin[["haz_cat"]] <- as.factor(rep_len(c(0L, 1L, 2L), nrow(cpp_imputed)))
  task_bin <- sl3_Task$new(cpp_hazbin, covariates = covars, outcome = "haz_cat")

  bays_lrn <- Lrnr_h2o_classifier$new(algorithm = "naivebayes")$train(task_bin)
  print(bays_lrn)
  bays_lrn_preds <- bays_lrn$predict()
  expect_true(ncol(bays_lrn_preds) == 4)
})

test_that("Lrnr_h2o_classifier works with naiveBays for non-factor outcomes", {
  h2o::h2o.no_progress()
  cpp_hazbin <- cpp_imputed
  cpp_hazbin[["haz_cat"]] <- rep_len(c(0L, 1L, 2L), nrow(cpp_imputed))
  task_bin <- sl3_Task$new(cpp_hazbin, covariates = covars, outcome = "haz_cat")
  bays_lrn <- Lrnr_h2o_classifier$new(algorithm = "naivebayes")$train(task_bin)
  print(bays_lrn)
  bays_lrn_preds <- bays_lrn$predict()
  expect_true(ncol(bays_lrn_preds) == 4)
})

test_that("Lrnr_h2o_grid learner works with a grid of regularized GLMs", {
  h2o::h2o.no_progress()
  h2o_glm_grid <- Lrnr_h2o_grid$new(algorithm = "glm", hyper_params = list(alpha = c(
    0,
    0.5, 1
  )))
  h2o_glm_grid <- h2o_glm_grid$train(task)
  h2oGLM_preds <- h2o_glm_grid$predict()
})

test_that("Lrnr_h2o_grid learner works with a grid of GBMs", {
  h2o::h2o.no_progress()
  glm_preds <- Lrnr_glm$new()$train(task)$predict()
  glm_preds2 <- Lrnr_glm_fast$new()$train(task)$predict()

  h2o_gbm_grid <- Lrnr_h2o_grid$new(algorithm = "gbm", hyper_params = list(ntrees = c(
    10,
    20, 50
  )))
  h2o_gbm_grid <- h2o_gbm_grid$train(task)
  h2ogbm_preds <- h2o_gbm_grid$predict()
})

test_that("stack$predict plays nicely when Learner$predict() is a grid of predictions from several models", {
  h2o::h2o.no_progress()
  glm_learner <- Lrnr_glm$new()
  fglm_learner <- Lrnr_glm_fast$new()

  screen_and_glm <- Pipeline$new(
    Lrnr_pkg_SuperLearner_screener$new("screen.glmnet"),
    fglm_learner
  )

  h2o_gbm_grid <- Lrnr_h2o_grid$new(
    algorithm = "gbm",
    hyper_params = list(ntrees = c(10, 20))
  )

  SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")

  # now lets stack some learners:
  learner_stack <- Stack$new(
    glm_learner, fglm_learner, screen_and_glm, h2o_gbm_grid,
    SL.glmnet_learner
  )
  stack_fit <- learner_stack$train(task)
  preds <- stack_fit$predict()

  expect_true(data.table::is.data.table(preds))
  expect_true(ncol(preds) == 6L)
})

test_that("check Lrnr_h2o_mutator returns matrices of mutated predictors", {
  h2o::h2o.no_progress()
  ## regular GLM
  pca_lrnr <- Lrnr_h2o_mutator$new(
    algorithm = "pca", k = 4,
    impute_missing = TRUE
  )
  pca_fit <- pca_lrnr$train(task)
  preds <- pca_fit$predict()
  expect_true(ncol(preds) == 4L)
})

test_that("h2o.pca works with pipelines (not checking results)", {
  h2o::h2o.no_progress()
  ## regular GLM
  fglm_learner <- Lrnr_glm_fast$new()
  ## screen covars in X, then fit GLM on modified (subset of) X:
  screen_and_glm <- Pipeline$new(
    Lrnr_pkg_SuperLearner_screener$new("screen.glmnet"),
    fglm_learner
  )
  ## apply PCA to X, then fit GLM on results of PCA
  pca_to_glm <- Pipeline$new(
    Lrnr_h2o_mutator$new(algorithm = "pca", k = 3, impute_missing = TRUE),
    Lrnr_glm_fast$new()
  )

  # fit only pca pipeline
  # pca_to_glm_fit <- pca_to_glm$train(task)

  # stack above learners and fit them all:
  learner_stack <- Stack$new(fglm_learner, screen_and_glm, pca_to_glm)
  stack_fit <- learner_stack$train(task)
  preds <- stack_fit$predict()
})

h2o.shutdown(prompt = FALSE)
Sys.sleep(3)

# test_that("Lrnr_h2o_grid and Lrnr_h2o_glm will start a new instance of h2o if none was found", {
#     op <- options(sl3.verbose = TRUE)
#     h2o_glm <- Lrnr_h2o_glm$new()
#     expect_error(h2oGLM_fit <- h2o_glm$train(task))

#     h2o_glm_grid <- Lrnr_h2o_grid$new(algorithm = "glm",
#         covariates = c("apgar1", "apgar5", "parity"),
#         interactions = c("apgar1", "apgar5"),
#         hyper_params = list(alpha = c(0, 0.5)))
#     expect_error(h2o_glm_grid <- h2o_glm_grid$train(task))
#     options(op)
# })
