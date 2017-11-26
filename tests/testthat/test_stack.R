library(testthat)
context("test_stack.R -- Basic stack functionality")

library(sl3)
library(origami)
library(SuperLearner)


data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
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
