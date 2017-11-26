context("test_densitySL.R -- Lrnr_condensier in a SuperLearner")

require(condensier)
require(origami)
require(sl3)

test_that("Super Learner for densities works", {
  ## Simulation code for the original density_dat
  # require("simcausal")
  # D <- DAG.empty()
  # D <- D +
  #   node("W1", distr = "rbern", prob = 0.5) +
  #   node("W2", distr = "rbern", prob = 0.3) +
  #   node("W3", distr = "rbern", prob = 0.3) +
  #   node("sA.mu", distr = "rconst", const = (0.98 * W1 + 0.58 * W2 + 0.33 * W3)) +
  #   node("sA", distr = "rnorm", mean = sA.mu, sd = 1) +
  #   node("like.sA", distr = "rconst", const = dnorm(sA,  mean = sA.mu, sd = 1))
  # D <- set.DAG(D, n.test = 10, vecfun = "dnorm")
  # density_dat <- sim(D, n = 1000, rndseed = 12345)
  # save(density_dat, compress = TRUE, compression_level = 9, file = "density_dat.rda")

  op <- options(sl3.verbose = FALSE)
  data(density_dat)
  ## Define 3 density estimators (candidate learners) using sl3 package:
  task_1 <- sl3_Task$new(
    density_dat, covariates = c("W1", "W2", "W3"),
    outcome = "sA",
    folds = make_folds(
      V = 5,
      cluster_ids = density_dat$ID
    )
  )
  newdata <- density_dat[1:5, c("W1", "W2", "W3", "sA")]
  new_task <- sl3_Task$new(
    newdata, covariates = c("W1", "W2", "W3"),
    outcome = "sA"
  )

  ## Define some learners:
  lrn1 <- Lrnr_condensier$new(nbins = 5, bin_method = "equal.len", pool = FALSE)
  lrn2 <- Lrnr_condensier$new(nbins = 5, bin_method = "equal.len", pool = TRUE)
  lrn3 <- Lrnr_condensier$new(nbins = 7, bin_method = "equal.mass", pool = TRUE)
  lrn4 <- Lrnr_condensier$new(
    nbins = 5, bin_method = "equal.len", pool = TRUE,
    condensier::speedglmR6$new()
    # bin_estimator = Lrnr_xgboost$new(nrounds = 20,
    # objective = 'reg:logistic')
  )
  sl <- Lrnr_sl$new(
    learners = list(lrn1, lrn2, lrn3, lrn4),
    metalearner = Lrnr_solnp_density$new()
  )
  sl_fit <- sl$train(task_1)
  is(sl_fit, "Lrnr_base")
  ## obtain likelihood predictions from SL fit for new data:
  sl_preds <- sl_fit$predict(new_task)
  options(op)
})
