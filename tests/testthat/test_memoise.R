# disabled until memoise is reimplemented either outside of or as a part of delayed
# library(sl3)
# library(testthat)
# library(origami)
# library(SuperLearner)
# 
# 
# data(cpp)
# cpp <- cpp[!is.na(cpp[, "haz"]), ]
# covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
# cpp[is.na(cpp)] <- 0
# outcome <- "haz"
# task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
# task_2 <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
# glm_memoised <- Lrnr_glm$new(memoise_learner=T)
# glm_memoised_2 <-Lrnr_glm$new(memoise_learner=T)
# memoised_fit <- glm_memoised$train(task)
# 
# glm_unmemoised <- Lrnr_glm$new(memoise_learner=F)
# 
# test_that("memoised learner returns fit from cache", {
#   memoised_fit_2 <- glm_memoised$train(task)
#   expect_equal(memoised_fit,memoised_fit_2)
#   })
# 
# test_that("a different task results in a new fit", {
#   memoised_fit_3 <- glm_memoised$train(task_2)
#   expect_false(identical(memoised_fit,memoised_fit_3))
#   })
# 
# test_that("a different learner instance results in a new fit", {
#   memoised_fit_4 <- glm_memoised_2$train(task)
#   expect_false(identical(memoised_fit,memoised_fit_4))
# })
# 
# test_that("memoisation can be disabled", {
#   unmemoised_fit_1 <- glm_unmemoised$train(task)
#   unmemoised_fit_2 <- glm_unmemoised$train(task)
#   expect_false(identical(unmemoised_fit_1,unmemoised_fit_2))
# })
