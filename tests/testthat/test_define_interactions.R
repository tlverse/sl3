library(sl3)
library(testthat)
context("Lrnr_sl Test")

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)

make_inter <- Lrnr_define_interactions$new(interactions=list(c("apgar1","parity"),c("apgar5","parity")))
task_with_interactions <- make_inter$train(task)$chain()
X_interact <- task_with_interactions$X

test_that("Lrnr_define_interactions adds interactions",
          {
           expect_equal(X_interact$apgar1*X_interact$parity, X_interact$apgar1_parity)
           expect_equal(X_interact$apgar5*X_interact$parity, X_interact$apgar5_parity)
          })