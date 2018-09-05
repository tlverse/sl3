context("test-define_interactions.R -- Lrnr_define_interactions")

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
interactions = list(c("apgar1", "parity"), c("apgar5", "parity"))
make_inter <- Lrnr_define_interactions$new(interactions = list(c("apgar1", "parity"), c("apgar5", "parity")), warn_on_existing = FALSE)
fit_inter <- make_inter$train(task)
task_with_interactions <- fit_inter$base_chain()
X_interact <- task_with_interactions$X

test_that("Lrnr_define_interactions adds interactions", {
  expect_equal(X_interact$apgar1 * X_interact$parity, X_interact$apgar1_parity)
  expect_equal(X_interact$apgar5 * X_interact$parity, X_interact$apgar5_parity)
})

test_that("A pipeline with Lrnr_define_interactions can be chained", {
  lrnr_glm <- Lrnr_glm$new()
  pipeline_glm <- make_learner(Pipeline, make_inter, lrnr_glm)
  fit <- pipeline_glm$train(task)
  result <- fit$chain()
  expect_is(result, "sl3_Task")
})
