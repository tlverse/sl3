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

library(hal9001)
library(haldensify)
library(origami)

data(cpp_imputed)
covars <- c(
  "parity", "sexn"
)
outcome <- "haz"
task <- cpp_imputed %>%
  dplyr::filter(agedays == 1) %>%
  sl3_Task$new(
    covariates = covars,
    outcome = outcome
  )

hal_dens <- Lrnr_haldensify$new(
  lambda_seq = exp(seq(-1, -13, length = 100))
)

test_that("Lrnr_haldensify produces prediction similar to haldensify", {
  set.seed(67391)
  hal_dens_fit <- hal_dens$train(task)
  hal_dens_preds <- hal_dens_fit$predict()

  set.seed(67391)
  haldensify_fit <- haldensify::haldensify(
    A = as.numeric(task$Y),
    W = as.matrix(task$X),
    lambda_seq = exp(seq(-1, -13,
      length = 100
    ))
  )
  haldensify_preds <- predict(haldensify_fit,
    new_A = as.numeric(task$Y),
    new_W = as.matrix(task$X)
  )

  # check that predicted conditional density estimates match
  expect_equal(hal_dens_preds, haldensify_preds, tolerance = 1e-15)
})

# test_that("Ensembling with Lrnr_haldensify produces sane predictions", {
## just another HAL to ensemble with
# hal_dens_more_lambda <- Lrnr_haldensify$new(
# lambda_seq = exp(seq(-1, -13, length = 200))
# )

## ensembled conditional density estimation
# sl3_dens <- Lrnr_sl$new(
# learners = list(hal_dens, hal_dens_more_lambda),
# metalearner = Lrnr_solnp_density$new()
# )

# set.seed(67391)
# sl3_dens_fit <- sl3_dens$train(task)
# sl3_dens_preds <- sl3_dens_fit$predict() %>%
# unlist(use.names = FALSE)

## check that predicted conditional density estimates match
# expect_equal(sl3_dens_preds, haldensify_preds, tolerance = 1e-2)
# })
