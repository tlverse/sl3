context("test-shared-data.R -- chain two tasks with shared underlying data")


data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

# ref_a <- setDT(cpp_imputed)
# ref_b <- setDT(cpp_imputed)
#
# new_data_a <- data.frame(A=1)
# setDT(new_data_a)
#
# new_data_b <- data.frame(B=2)
# setDT(new_data_b)
#
# set(ref_a, j=names(new_data_a), value=new_data_a)
# set(ref_b, j=names(new_data_b), value=new_data_b)

task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
task_2 <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

glm <- make_learner(Lrnr_glm)
fit <- glm$train(task)

test_that("making two tasks from the same data and chaining lworks", {
  chained <- fit$chain(task)
  chained_2 <- fit$chain(task_2)
  expect_is(chained, "sl3_Task")
  expect_is(chained_2, "sl3_Task")
})
