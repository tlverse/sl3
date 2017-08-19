library(condensier)
library(sl3)
library(data.table)
library(simcausal)

op <- options(sl3.verbose = FALSE)

D <- DAG.empty()
D <-
D + node("W1", distr = "rbern", prob = 0.5) +
  node("W2", distr = "rbern", prob = 0.3) +
  node("W3", distr = "rbern", prob = 0.3) +
  node("sA.mu", distr = "rconst", const = (0.98 * W1 + 0.58 * W2 + 0.33 * W3)) +
  node("sA", distr = "rnorm", mean = sA.mu, sd = 1) +
  node("like.sA", distr = "rconst", const = dnorm(sA,  mean = sA.mu, sd = 1))

D <- set.DAG(D, n.test = 10, vecfun = "dnorm")
datO <- sim(D, n = 1000, rndseed = 12345)

test_that("Super Learner for densities works", {
  ## Define 3 density estimators (candidate learners) using sl3 package:
  task <- sl3_Task$new(datO, covariates=c("W1", "W2", "W3"),outcome="sA")
  newdata <- datO[1:5, c("W1", "W2", "W3", "sA")]
  new_task <- sl3_Task$new(newdata, covariates=c("W1", "W2", "W3"),outcome="sA" )

  ## Define some learners:
  lrn1 <- Lrnr_condensier$new(task, nbins = 5, bin_method = "equal.len", pool = TRUE)
  lrn2 <- Lrnr_condensier$new(task, nbins = 5, bin_method = "equal.mass", pool = TRUE)

  ## Fit a Super Learner (all of the above steps + find the optimal convex combination of densities)
  sl <- Lrnr_sl$new(learners = list(lrn1, lrn2),
                    metalearner = Lrnr_solnp_density$new())
  sl_fit <- sl$train(task)

  ## obtain likelihood predictions from SL fit for new data:
  sl_preds <- sl_fit$predict(new_task)
})

options(op)