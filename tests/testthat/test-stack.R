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
stack <- Stack$new(glm_learner, glmnet_learner)
stack2 <- Stack$new(stack)
test_that(
  "Stack$new copies original stack when learners is a Stack",
  expect_equivalent(stack$params$learners, stack2$params$learners)
)

# stack3 <- Stack$new(stack,glm_learner)
# stack3$params$learners
#
# test_that("Stack$new combines existing stacks into itself",
#           expect_length(stack3$params$learners,3))

# test that unique names are created when there's repetition
dens_bin10_glm <- Lrnr_condensier$new(
  nbins = 10, bin_estimator = glm_learner,
  bin_method = "dhist"
)
# check that stack gives unique names to input learners
stack_dens <- Stack$new(dens_bin10_glm, dens_bin10_glm)
stack_lrnr_names <- as.character(stack_dens$print())
test_that("Repetitive names of learners in stack differ after creation", {
  expect_false(stack_lrnr_names[1] == stack_lrnr_names[2])
})

# check that you can create a stack of one learner
stack_one <- Stack$new(glm_learner)
fit <- stack_one$train(task)
preds <- fit$predict(task)
