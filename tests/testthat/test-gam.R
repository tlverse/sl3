context("test-gam.R -- General testing for GAMs")

library(dplyr)
library(gam)

# define test dataset
data(mtcars)
task <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg")

task2 <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg")

interactions <- list(c("cyl", "disp"), c("hp", "drat"))
task_with_interactions <- task$add_interactions(interactions)
task2 <- task2$add_interactions(interactions)

test_learner <- function(learner, task, ...) {
  # test learner definition this requires that a learner can be instantiated with
  # only default arguments. Not sure if this is a reasonable requirement
  learner_obj <- learner$new(...)
  print(sprintf("Testing Learner: %s", learner_obj$name))
  # test learner training
  fit_obj <- learner_obj$train(task)
  test_that("Learner can be trained on data", expect_true(fit_obj$is_trained))

  # test learner prediction
  train_preds <- fit_obj$predict()
  test_that("Learner can generate training set predictions", expect_equal(
    sl3:::safe_dim(train_preds)[1],
    length(task$Y)
  ))

  holdout_preds <- fit_obj$predict(task2)
  test_that("Learner can generate holdout set predictions", expect_equal(
    train_preds,
    holdout_preds
  ))

  # test learner chaining
  chained_task <- fit_obj$chain()
  test_that("Chaining returns a task", expect_true(is(chained_task, "sl3_Task")))
  test_that("Chaining returns the correct number of rows", expect_equal(
    nrow(chained_task$X),
    nrow(task$X)
  ))
}

## test GAM learner:
op <- options(sl3.verbose = TRUE)
options(op)
test_learner(Lrnr_gam, task)
test_learner(Lrnr_gam, task2)

test_that("Lrnr_gam predictions approximately match those from gam::gam", {
  ## instantiate Lrnr_gam, train on task, and predict on task
  set.seed(73964)
  lrnr_gam <- Lrnr_gam$new(
    deg_plynm = 1,
    lim_cntns = 4
  )
  fit_lrnr_gam <- lrnr_gam$train(task)
  prd_lrnr_gam <- fit_lrnr_gam$predict()

  ## fit gam using the data from the task
  set.seed(73964)
  fit_gam <- gam(mpg ~ .,
    data = mtcars,
    control = gam::gam.control(maxit = 50, bf.maxit = 50)
  )
  prd_gam <- predict(fit_gam, mtcars) %>%
    as.numeric()

  ## test equivalence of prediction from Lrnr_gam and gam::gam
  expect_equal(prd_lrnr_gam, prd_gam, tol = 1e-6)
})

# library(rlang)
# library(SuperLearner)

# test_that("Lrnr_gam predictions match those from SL.gam in SuperLearner", {
### instantiate Lrnr_gam, train on task, and predict on task
# set.seed(73964)
# lrnr_gam <- Lrnr_gam$new(deg_plynm = 2,
# lim_cntns = 4)
# lrnr_sl_gam <- Lrnr_sl$new(learners = list(lrnr_gam),
# metalearner = Lrnr_nnls$new())
# fit_lrnr_sl_gam <- lrnr_sl_gam$train(task)
# prd_lrnr_sl_gam <- fit_lrnr_sl_gam$predict()

### fit gam using the data from the task
# set.seed(73964)
# SL_folds <- lapply(task$folds, function(x) {
# out <- mget("validation_set",
# new_environment(x)) %>%
# unlist(., use.names = FALSE)
# })
# fit_SL_gam <- SuperLearner(Y = as.numeric(task$Y),
# X = as.data.frame(task$X),
# SL.library = "SL.gam",
# cvControl = list(V = 10, validRows = SL_folds))
# prd_SL_gam <- predict(fit_SL_gam)$pred %>%
# as.numeric()

### test equivalence of prediction from Lrnr_gam and SL.gam
# expect_equal(prd_lrnr_sl_gam, prd_SL_gam)
# })
