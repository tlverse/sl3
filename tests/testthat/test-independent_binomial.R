context("test-independent_binomial.R -- Lrnr_independent_binomial")

library(dplyr)

data(cpp)

cpp <- cpp %>%
  select(c(bmi, agedays, feeding)) %>%
  mutate(feeding = as.factor(feeding)) %>%
  na.omit

task <- make_sl3_Task(cpp, covariates = c("agedays", "bmi"), outcome = "feeding")

test_that("Lrnr_independent_binomial produces predictions with retained factor levels", {
  # get predictions from Lrnr_* wrapper
  lrnr_indbinomial <- make_learner(Lrnr_independent_binomial)
  fit <- lrnr_indbinomial$train(task)
  preds <- fit$predict(task)
  
  original_names <- levels(cpp$feeding)
  predicted_names <- names(preds[[1]][[1]])
  
  # check equality of predictions
  expect_equal(original_names, predicted_names)
})
