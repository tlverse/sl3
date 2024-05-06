context("test-stack_broken.R -- Stack robustness to sub-learner errors")
library(R6)

data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

library(uuid)
Lrnr_broken <- R6Class(
  classname = "Lrnr_broken", inherit = Lrnr_base, portable = TRUE,
  public = list(
    initialize = function(pbreak = 1) {
      private$.params <- list(pbreak = pbreak)

      invisible(self)
    }
  ),
  private = list(
    .train = function(task) {
      if (rbinom(1, 1, self$params$pbreak)) {
        stop("this Learner often returns an error on training")
      }
    }
  )
)

broken_learner <- Lrnr_broken$new()
glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_glmnet$new()
broken_stack <- Stack$new(broken_learner, glm_learner, glm_learner)

test_that("Stack produces warning for learners that return errors", {
  expect_warning(broken_fit <- broken_stack$train(task))
})

test_that("Stack predicts on remaining good learners", {
  broken_fit <- suppressWarnings(broken_stack$train(task))
  predictions <- broken_fit$predict()
  expect_equal(length(predictions[[1]]), nrow(cpp_imputed))
  # expect_equal(names(predictions), "Lrnr_glm_TRUE")
})

test_that("Stack fails if all learners return errors", {
  all_broken_stack <- Stack$new(broken_learner, broken_learner)
  expect_error(suppressWarnings(all_broken_stack$train(task)))
})

test_that("Lrnr_cv on stack warns when learners that error on any fold", {
  broken_cv <- Lrnr_cv$new(broken_stack)
  expect_warning(broken_cv$train(task))
})

test_that("Lrnr_cv on stack drops all learners that error on any fold", {
  broken_cv <- Lrnr_cv$new(broken_stack)

  broken_cv_fit <- suppressWarnings(broken_cv$train(task))
  cv_preds <- broken_cv_fit$predict()
  expect_equal(length(cv_preds[[1]]), nrow(cpp_imputed))
  # expect_equal(names(cv_preds), "Lrnr_glm_TRUE")
})

test_that("Lrnr_sl propagates errors to full refit", {
  broken_sl <- Lrnr_sl$new(
    broken_stack,
    glm_learner
  )
  suppressWarnings({
    broken_sl_fit <- broken_sl$train(task)
  })
  expect_equal(sum(!is.na(coef(broken_sl_fit))), 2)
})

test_that("Lrnr_sl works even when a library learner breaks only sometimes", {
  set.seed(1)
  prob_broken_learner <- Lrnr_broken$new(pbreak = 0.1)
  prob_broken_sl <- Lrnr_sl$new(
    list(prob_broken_learner, glm_learner, glm_learner),
    glm_learner
  )
  suppressWarnings({
    prob_broken_sl_fit <- prob_broken_sl$train(task)
  })
  expect_equal(sum(!is.na(coef(prob_broken_sl_fit))), 2)
})
