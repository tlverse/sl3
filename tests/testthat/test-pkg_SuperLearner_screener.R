context("test-pkg_SuperLearner_screener.R -- SL.screen wrapper")

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

# library(data.table)
library(origami)
library(SuperLearner)

set.seed(1)

data(cpp_imputed)

# make a factor covariate
setDT(cpp_imputed)
cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
levels(cpp_imputed$parity_cat) <- c("0","bad level 1","bad.2","also_bad","4+")
covars <- c("apgar1", "apgar5", "parity_cat", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"



task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
task$nodes$covariates

# example of learner chaining
slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
glm_learner <- Lrnr_glm$new()
screen_and_glm <- Pipeline$new(slscreener, glm_learner)
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")
sg_fit <- screen_and_glm$train(task)
