## ----setup, echo=FALSE, results='hide'-----------------------------------
library(sl3)

## ----prelims, message = FALSE--------------------------------------------
set.seed(49753)

# packages we'll be using
library(data.table)
library(sl3)
library(origami)
library(SuperLearner)

# load example data set
data(cpp_imputed)

# here are the covariates we are interested in, and the outcome of course
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
            "sexn")
outcome <- "haz"

## ----sl3-task-create-----------------------------------------------------
task <- make_sl3_Task(data = cpp_imputed, covariates = covars, outcome = outcome)

## ----sl3-task-examine----------------------------------------------------
task

## ----sl3-make_learner----------------------------------------------------
# make learner object
lrnr_glm <- make_learner(Lrnr_glm)

## ----sl3-learner-train---------------------------------------------------
# fit learner to task data
lrnr_glm_fit <- lrnr_glm$train(task)

# verify that the learner is fit
lrnr_glm_fit$is_trained

## ----sl3-learner-predict-------------------------------------------------
# get learner predictions
preds <- lrnr_glm_fit$predict(task)
head(preds)

## ----sl3-list-learner----------------------------------------------------
sl3_list_properties()

sl3_list_learners(c("binomial", "offset"))

## ----SuperLearner Wrapper------------------------------------------------
lrnr_sl_glmnet <- make_learner(Lrnr_pkg_SuperLearner, "SL.glmnet")

## ----sl3-fit-screener, message=FALSE-------------------------------------
screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")
screen_fit <- screen_cor$train(task)
print(screen_fit)

## ----sl3-chain-screener--------------------------------------------------
screened_task <- screen_fit$chain()
print(screened_task)

## ----sl3-glm-on-screened-------------------------------------------------
screened_glm_fit <- lrnr_glm$train(screened_task)
screened_preds <- screened_glm_fit$predict()
head(screened_preds)

## ----sl3-define-pipeline-------------------------------------------------
sg_pipeline <- make_learner(Pipeline, screen_cor, lrnr_glm)
sg_pipeline_fit <- sg_pipeline$train(task)
sg_pipeline_preds <- sg_pipeline_fit$predict()
head(sg_pipeline_preds)

## ----sl3-stack-----------------------------------------------------------
stack <- make_learner(Stack, lrnr_glm, sg_pipeline)
stack_fit <- stack$train(task)
stack_preds <- stack_fit$predict()
head(stack_preds)

## ----sl3-cv-stack--------------------------------------------------------
cv_stack <- Lrnr_cv$new(stack)
cv_fit <- cv_stack$train(task)
cv_preds <- cv_fit$predict()

## ----sl3-cv-risk---------------------------------------------------------
risks <- cv_fit$cv_risk(loss_squared_error)
print(risks)

## ----sl3-metalearner-glm-------------------------------------------------
metalearner <- make_learner(Lrnr_nnls)
cv_task <- cv_fit$chain()
ml_fit <- metalearner$train(cv_task)

## ----sl3-define-SuperLearner---------------------------------------------
sl_pipeline <- make_learner(Pipeline, stack_fit, ml_fit)
sl_preds <- sl_pipeline$predict()
head(sl_preds)

## ----sl3-Lrnr_sl---------------------------------------------------------
sl <- Lrnr_sl$new(learners = stack,
                  metalearner = metalearner)
sl_fit <- sl$train(task)
lrnr_sl_preds <- sl_fit$predict()
head(lrnr_sl_preds)

## ----sl3-delayed-sl------------------------------------------------------
lrnr_rf <- make_learner(Lrnr_randomForest)
lrnr_glmnet <- make_learner(Lrnr_glmnet)
sl <- Lrnr_sl$new(learners = list(lrnr_glm, lrnr_rf, lrnr_glmnet),
                  metalearner = metalearner)

## ----sl3-delayed-plot----------------------------------------------------
delayed_sl_fit <- delayed_learner_train(sl, task)
plot(delayed_sl_fit)

## ----sessionInfo, echo=FALSE---------------------------------------------
sessionInfo()

