if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

library(sl3)
library(data.table)
library(origami)
library(SuperLearner)
context("Overall Test")
# library(gridisl)
set.seed(1)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")



# todo: make a learner (or whatever) that preprocesses data, including
# factorizing 'discretish' variables, and makes missingness indicators
cpp[is.na(cpp)] <- 0
outcome <- "haz"

load_all() # for debug
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
task$nodes$covariates

# example of learner chaining
slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
glm_learner <- Lrnr_glm$new()
screen_and_glm <- Pipeline$new(slscreener, glm_learner)
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")
sg_fit <- screen_and_glm$train(task)
print(sg_fit)

# now lets stack some learners
learner_stack <- Stack$new(SL.glmnet_learner, glm_learner, screen_and_glm)
stack_fit <- learner_stack$train(task)
stack_fit$predict()
# stack_fit$predict()

# okay but what if we want CV fits/predictions (this part is still really rough)
cv_stack <- Lrnr_cv$new(learner_stack)
cv_fit <- cv_stack$train(task)
# cv_fit$predict()

# we can now fit a metalearner on the CV preds
glm_stack <- Pipeline$new(cv_stack, glm_learner)
ml_fit <- glm_stack$train(task)
ml_fit$predict()
print(ml_fit)

# convenience learner combining all this
sl <- Lrnr_sl$new(
  learners = list(SL.glmnet_learner, glm_learner, screen_and_glm),
  metalearner = glm_learner
)
sl_fit <- sl$train(task)
sl_fit
