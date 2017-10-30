library(sl3)
library(testthat)
context("test_define_interactions.R -- Lrnr_define_interactions")

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

make_inter <- Lrnr_define_interactions$new(interactions=list(c("apgar1","parity"),c("apgar5","parity")))
task_with_interactions <- make_inter$train(task)$chain()
X_interact <- task_with_interactions$X

test_that("Lrnr_define_interactions adds interactions",
          {
           expect_equal(X_interact$apgar1*X_interact$parity, X_interact$apgar1_parity)
           expect_equal(X_interact$apgar5*X_interact$parity, X_interact$apgar5_parity)
          })