context("test_haldensify.R -- Lrnr_haldensify")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  devtools::check() # runs full check
  setwd("..")
  install("sl3",
    build_vignettes = FALSE,
    dependencies = FALSE
  ) # INSTALL W/ devtools:
}

library(testthat)
library(sl3)
library(hal9001)
library(haldensify)

data(cpp_imputed)
covars <- c(
  "parity", "sexn"
)
outcome <- "haz"
task <- cpp_imputed %>%
  dplyr::filter(agedays == 1) %>%
  sl3_Task$new(covariates = covars, outcome = outcome)

hal_dens <- Lrnr_haldensify$new(
  grid_type = "equal_range", n_bins = 3,
  lambda_seq = exp(seq(-1, -13, length = 100))
)
hal_dens_2 <- Lrnr_haldensify$new(
  grid_type = "equal_range", n_bins = 2,
  lambda_seq = exp(seq(-1, -13, length = 100))
)

sl3_dens <- Lrnr_sl$new(
  learners = list(hal_dens, hal_dens_2),
  metalearner = Lrnr_solnp_density$new()
)

test_that("Lrnr_haldensify produces prediction similar to haldensify", {
  set.seed(67391)
  hal_dens_fit <- hal_dens$train(task)
  hal_dens_preds <- hal_dens_fit$predict()

  # set.seed(67391)
  # sl3_dens_fit <- sl3_dens$train(task)
  # sl3_dens_preds <- sl3_dens_fit$predict()

  set.seed(67391)
  haldensify_fit <- haldensify::haldensify(
    A = task$Y, W = as.matrix(task$X),
    grid_type = "equal_range",
    n_bins = 3,
    lambda_seq = exp(seq(-1, -13,
      length = 100
    ))
  )
  haldensify_preds <- predict(haldensify_fit,
    new_A = task$Y,
    new_W = as.matrix(task$X)
  )

  # check that predicted conditional density estimates match
  expect_equal(hal_dens_preds, haldensify_preds, tolerance = 1e-15)
})
