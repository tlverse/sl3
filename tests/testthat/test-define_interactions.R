context("test-define_interactions.R -- Lrnr_define_interactions")

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
interactions <- list(c("apgar1", "parity"), c("apgar5", "parity"))
make_inter <- Lrnr_define_interactions$new(interactions = interactions, warn_on_existing = FALSE)
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

test_that("Lrnr_define_interactions adds categorical/mixed-type interactions", {
  covars_factors <- c(covars, "feeding", "mrace")
  task <- suppressWarnings(
    sl3_Task$new(cpp_imputed, covariates = covars_factors, outcome = outcome)
  )
  interactions <- list(
    c("apgar1", "feeding"), c("feeding", "mrace"),
    c("apgar1", "apgar5"), c("apgar1", "feeding", "mrace")
  )
  make_inter <- Lrnr_define_interactions$new(interactions)
  fit_inter <- make_inter$train(task)
  task_with_interactions <- fit_inter$base_chain()
  X_interact <- task_with_interactions$X

  lrnr_glm <- Lrnr_glm$new()
  pipeline_glm <- make_learner(Pipeline, make_inter, lrnr_glm)
  fit <- pipeline_glm$train(task)
  coef_glm <- coef(
    glm(haz ~ ., data = fit$learner_fits$Lrnr_glm_TRUE$training_task$data)
  )
  coef_sl3 <- coef(fit$learner_fits$Lrnr_glm_TRUE)
  expect_equal(mean(coef_glm - coef_sl3, na.rm = T), 0)
})

test_that("Lrnr_define_interactions warns on existing interactions", {
  interactions <- list("apgar1", c("apgar1", "apgar5"))
  make_inter <- Lrnr_define_interactions$new(interactions)
  fit_inter <- make_inter$train(task)
  expect_warning(fit_inter$base_chain())
})
