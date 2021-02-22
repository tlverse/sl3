context("test-bayesglm -- Lrnr_bayesglm")

library(arm)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed,
                     covariates = covars,
                     outcome = outcome
)

test_that("Lrnr_bayesglm produces results matching those of arm::bayesglm", {
  # get predictions from Lrnr_* wrapper
  set.seed(1234)
  lrnr_bayesglm <- make_learner(Lrnr_bayesglm)
  fit <- lrnr_bayesglm$train(task)
  preds <- fit$predict(task)
  
  # get predictions from classic implementation
  set.seed(1234)
  fit_classic <- arm::bayesglm(
  haz ~ apgar1 + apgar5 + parity + gagebrth + mage + meducyrs + sexn,  data  =cpp_imputed
  )
  preds_classic <- predict(fit_classic,type = "response")
  
  # check equality of predictions
  expect_equal(preds, as.numeric(preds_classic))
})

test_that("Lrnr_bayesglm produces results matching those of legacy superLearner", {
  # get predictions from Lrnr_* wrapper
  set.seed(1234)
  lrnr_bayesglm_sl3 <- make_learner(Lrnr_bayesglm)
  fit_sl3 <- lrnr_bayesglm_sl3$train(task)
  preds_sl3 <- fit_sl3$predict(task)
  
  # get predictions from the legacy super learner
  set.seed(1234)
  lrnr_legacy_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
  fit_legacy <- lrnr_legacy_bayesglm$train(task)
  preds_legacy <- fit_legacy$predict(task)
  
  # check equality of predictions
  expect_equal(preds_sl3, as.numeric(preds_legacy))
})

test_that("Lrnr_bayesglm makes training set predictions for a continuous outcome", {
  con_covars <- c(
    "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
    "sexn"
  )
  con_outcome <- "haz"
  con_task <- sl3_Task$new(cpp_imputed,
                       covariates = con_covars,
                       outcome = con_outcome
  )
  lrnr_bayesglm <- make_learner(Lrnr_bayesglm)
  fit <- lrnr_bayesglm$train(con_task)
  preds <- fit$predict(con_task)
  
  expect_equal(sl3:::safe_dim(preds)[1],length(con_task$Y))
})

test_that("Lrnr_bayesglm makes training set predictions for a binary outcome", {
  bin_covars <- c(
    "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
    "sexn"
  )
  bin_outcome <- "smoked"
  bin_task <- sl3_Task$new(cpp_imputed,
                           covariates = bin_covars,
                           outcome = bin_outcome
  )
  lrnr_bayesglm <- Lrnr_bayesglm$new(family = binomial())
  fit <- lrnr_bayesglm$train(bin_task)
  preds <- fit$predict(bin_task)
  
  expect_equal(sl3:::safe_dim(preds)[1],length(bin_task$Y))
})

test_that("Lrnr_bayesglm makes out of sample predictions", {
  cpp_imputed_1 <- sample_n(cpp_imputed, 800, relpace = TRUE)
  cpp_imputed_2 <- sample_n(cpp_imputed, 200, replace = TRUE)
  
  task_1 <- sl3_Task$new(cpp_imputed_1,
                       covariates = covars,
                       outcome = outcome
  )
  
  task_2 <- sl3_Task$new(cpp_imputed_2,
                       covariates = covars,
                       outcome = outcome
  )
  
  lrnr_bayesglm <- make_learner(Lrnr_bayesglm)
  fit <- lrnr_bayesglm$train(task_1)
  preds_2 <- fit$predict(task_2)
  
  expect_equal(sl3:::safe_dim(preds_2)[1],length(task_2$Y))
})

test_that("Lrnr_bayesglm with intercept=FALSE works", {
  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
  lrnr_bayesglm <- make_learner(Lrnr_bayesglm, intercept = FALSE)
  fit <- lrnr_bayesglm$train(task)
  preds <- fit$predict(task)
  expect_equal(task$nrow, length(preds))
})

test_that("Lrnr_bayesglm generates predictions when specifying a custom prior distribution", {
  bin_covars <- c(
    "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
    "sexn"
  )
  bin_outcome <- "smoked"
  bin_task <- sl3_Task$new(cpp_imputed,
                           covariates = bin_covars,
                           outcome = bin_outcome
  )
  lrnr_bayesglm <- Lrnr_bayesglm$new(family=binomial(link="logit"), prior.scale = 2.5, prior.df=1)
  fit_bayesglm <- lrnr_bayesglm$train(bin_task)
  preds_bayesglm <- fit_bayesglm$predict(bin_task)
  
  expect_equal(bin_task$nrow, length(preds_bayesglm))
})

