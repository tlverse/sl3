context("test-stack.R -- Basic stack functionality")
library(origami)
library(SuperLearner)


data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

glm_learner <- Lrnr_glm$new()
glmnet_learner <- Lrnr_pkg_SuperLearner$new("SL.glmnet")
stack <- Stack$new(glm1=glm_learner, glm2=glmnet_learner)
stack2 <- Stack$new(stack)
test_that(
  "Stack$new copies original stack when learners is a Stack",
  expect_equivalent(stack$params$learners, stack2$params$learners)
)

stack_fit <- stack$train(task)
stack_preds <- stack_fit$predict()
test_that("Stack accepts custom names",
          expect_equal(names(stack_preds),c("glm1","glm2")))
# stack3 <- Stack$new(stack,glm_learner)
# stack3$params$learners
#
# test_that("Stack$new combines existing stacks into itself",
#           expect_length(stack3$params$learners,3))

# test that unique names are created when there's repetition
dens_hse_glm <- make_learner(Lrnr_density_semiparametric,
  mean_learner = make_learner(Lrnr_glm)
)

# check that stack gives unique names to input learners
stack_dens <- Stack$new(dens_hse_glm, dens_hse_glm)
stack_lrnr_names <- as.character(stack_dens$print())
test_that("Repetitive names of learners in stack differ after creation", {
  expect_false(stack_lrnr_names[1] == stack_lrnr_names[2])
})


# check that stack does not assume predict length
Lrnr_fixed_pred_length <- R6Class(
  classname = "Lrnr_broken", inherit = Lrnr_base, portable = TRUE,
  public = list(
    initialize = function() {
      invisible(self)
    }
  ),
  private = list(
    .train = function(task) {
      return(list())
    },
    .predict = function(task) {
      return(rep(1, 10))
    }
  )
)

test_that("Stack works with prediction lengths that don't match task length", {
  stack_fixed_len <- Stack$new(Lrnr_fixed_pred_length$new(), Lrnr_fixed_pred_length$new())
  fit <- stack_fixed_len$train(task)
  preds <- fit$predict()
})

# check that you can create a stack of one learner
stack_one <- Stack$new(glm_learner)
fit <- stack_one$train(task)
preds <- fit$predict(task)


# check that stacks can be a mix of pretrained and untrained learners
task_old <- task[1:10]
mean_lrnr <- Lrnr_mean$new()
old_fit <- mean_lrnr$train(task_old)
stack_old_and_new <- Stack$new(old_fit, mean_lrnr)
stack_fit <- stack_old_and_new$train(task)
# debug_predict(stack_fit)
preds <- stack_fit$predict()
old_mean <- mean(task_old$Y)
new_mean <- mean(task$Y)

test_that("A stack mixed from learners and fits does not retrain existing fits", {
  expect_equal(unlist(preds[1, 1, with = FALSE], use.names = FALSE), old_mean)
  expect_equal(unlist(preds[1, 2, with = FALSE], use.names = FALSE), new_mean)
})
