context("test-density-pooled_hazards: Lrnr_pooled_hazards + Lrnr_density_discretize")

test_that("Negative log likelihood for pooled_hazards and haldensify match", {
  # define test dataset
  set.seed(74294)
  n <- 100
  x <- rnorm(n)
  epsilon <- rnorm(n)
  y <- 3 * x + epsilon
  data <- data.table(x = x, y = y)
  task <- sl3_Task$new(data, covariates = c("x"), outcome = "y")

  # instantiate learners
  hal <- Lrnr_hal9001$new(
    lambda = exp(seq(-1, -13, length = 100)),
    max_degree = 6,
    smoothness_orders = 0
  )
  haldensify <- Lrnr_haldensify$new(
    grid_type = "equal_range",
    n_bins = 5,
    lambda_seq = exp(seq(-1, -13, length = 100)),
    max_degree = 6,
    smoothness_orders = 0,
    trim_dens = 0
  )
  hazard_learner <- Lrnr_pooled_hazards$new(hal)
  density_learner <- Lrnr_density_discretize$new(
    hazard_learner,
    type = "equal_range",
    n_bins = 5
  )

  # fit discrete density model to pooled hazards data
  set.seed(74294)
  fit_density <- density_learner$train(task)
  pred_density <- fit_density$predict()

  # fit haldensify for comparison
  set.seed(74294)
  suppressWarnings({
    fit_haldensify <- haldensify$train(task)
  })
  pred_haldensify <- fit_haldensify$predict()

  # compare density estimates
  true_density <- dnorm(x = y, mean = 3 * x)
  nll_ph <- sum(-1 * true_density * log(pred_density))
  nll_haldensify <- sum(-1 * true_density * log(pred_haldensify))
  expect_equal(nll_ph, nll_haldensify, scale = nll_ph, tol = 2)
})
