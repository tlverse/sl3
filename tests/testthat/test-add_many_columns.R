context("test-add-many-columns.R -- Verify fix for data.table set issue")
# see https://github.com/Rdatatable/data.table/issues/1831

library(data.table)
library(testthat)

test_that("columns were added successful", {
  data(cpp_imputed)
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  outcome <- "haz"

  task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

  for (i in 1:1e4) {
    new_data <- data.table(A = rnorm(task$nrow))
    z <- task$add_columns(new_data)
  }
  expect_gt(ncol(task$internal_data$raw_data), 1e4)
})
