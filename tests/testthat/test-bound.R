context("test-Lrnr_bound.R -- bound predictions")
library(origami)

g0 <- function(W) {
  W1 <- W[, 1]
  scale_factor <- 0.8
  A <- plogis(scale_factor * W1)
}

gen_data <- function(n = 1000, p = 4) {
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W) <- paste("W", seq_len(p), sep = "")
  g0W <- g0(W)
  A <- rbinom(n, 1, g0W)

  u <- runif(n)
  df <- data.frame(W, A)

  df$g0W <- g0(W)

  return(df)
}

set.seed(1234)
data <- gen_data(1000)


Wnodes <- grep("^W", names(data), value = TRUE)
Anode <- "A"
task <- sl3_Task$new(data, covariates = Wnodes, outcome = Anode)
learners <- list(
  xgb = make_learner(Lrnr_xgboost, verbose = 0),
  glm_fast = make_learner(Lrnr_glm_fast),
  mean = make_learner(Lrnr_mean)
)

# define Super Learner
binom_sl <- make_learner(Lrnr_sl, learners)
sl_fit <- binom_sl$train(task)
preds <- sl_fit$predict()

lrnr_bound <- Lrnr_bound$new(bound = .1)
sl_pipeline_bounded <- make_learner(Pipeline, sl_fit, lrnr_bound)
sl_fit_bounded <- sl_pipeline_bounded$train(task)
bounded_preds <- sl_fit_bounded$predict()

test_that("Lrnr_bound is bounding predictions", {
  expect_gte(min(bounded_preds), 0.1)
  expect_lte(max(bounded_preds), 0.9)
})
