context("solnp_density.R -- Lrnr_solnp_density")

library(Rsolnp)

data(cpp_imputed)
setDT(cpp_imputed)

covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

test_that("Lrnr_solnp_density as a meta-learner coefficients to sum to 1", {
  set.seed(1234)
  task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
  
  hse_lrnr <- Lrnr_density_hse$new()
  semi_lrnr <- Lrnr_density_semiparametric$new()
  stack <- Stack$new(hse_lrnr, semi_lrnr)
  sl <- Lrnr_sl$new(learners = stack, metalearner = Lrnr_solnp_density$new())
  
  sl_fit <- sl$train(task)
  expect_equal(sum(coef(sl_fit)), 1)
})

test_that("Lrnr_solnp_density coefficients match Rsolnp coefficients", {
  task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
  set.seed(123)
  
  # sl3
  solnp_lrnr <- Lrnr_solnp_density$new()
  fit <- solnp_lrnr$train(task)
  
  # Rsolnp
  loss_func <- function(alphas) {
    sum(-log(as.vector(as.matrix(task$X) %*% alphas)))
  }
  eq_fun <- function(alphas) {
    sum(alphas)
  }
  
  set.seed(123)
  solnp_fit <- Rsolnp::solnp(
    stats::runif(ncol(task$X)), loss_func,
    eqfun = eq_fun, eqB = 1,
    LB = rep(0L, ncol(task$X))
  )

  expect_equal(as.numeric(coef(fit)), solnp_fit$pars)
  expect_equal(sum(coef(fit)), 1)
})
