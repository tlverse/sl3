library(testthat)
context("test_screeners.R -- Screening Procedures")

options(sl3.verbose = TRUE)
library(sl3)
library(origami)
library(SuperLearner)
library(data.table)
data(cpp_imputed)
setDT(cpp_imputed)
cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
covars <- c("apgar1", "apgar5", "parity_cat", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

task <- sl3_Task$new(data.table::copy(cpp_imputed), covariates = covars, 
                     outcome = outcome)
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnrs <- make_learner(Stack, lrnr_glm, lrnr_mean)

# Correlation P-value Screener                     
screen_corP <- make_learner(Lrnr_screener_corP)
corP_pipeline <- make_learner(Pipeline, screen_corP, lrnrs)
fit_corP <- corP_pipeline$train(task)

# Correlation Rank Screener                     
screen_corRank <- make_learner(Lrnr_screener_corRank)
corRank_pipeline <- make_learner(Pipeline, screen_corRank, lrnrs)
fit_corRank <- corRank_pipeline$train(task)

# Random Forest Screener                     
screen_randomForest <- make_learner(Lrnr_screener_randomForest)
randomForest_pipeline <- make_learner(Pipeline, screen_randomForest, lrnrs)
fit_randomForest <- randomForest_pipeline$train(task)

