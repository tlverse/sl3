if (FALSE) {
    setwd("..")
    setwd("..")
    getwd()
    library("devtools")
    document()
    load_all("./")  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
    setwd("..")
    install("sl3", build_vignettes = FALSE, dependencies = FALSE)  # INSTALL W/ devtools:
}

library(sl3)
# library(data.table)
library(origami)
library(SuperLearner)
context("Overall Test")
set.seed(1)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")



# todo: make a learner (or whatever) that preprocesses data, including factorizing 'discretish' variables, and
# makes missingness indicators
cpp[is.na(cpp)] <- 0
outcome <- "haz"


task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
task$nodes$covariates

# example of learner chaining
slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
glm_learner <- Lrnr_glm$new()
screen_and_glm <- Pipeline$new(slscreener, glm_learner)
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")
sg_fit <- screen_and_glm$train(task)
# print(sg_fit)
