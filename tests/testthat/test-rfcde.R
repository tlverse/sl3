context("test_rfcde.R -- Lrnr_rfcde")

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

library(RFCDE)

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

test_that("Lrnr_rfcde produces predictions similar to those from RFCDE", {
  set.seed(67391)
  rfcde_lrn <- Lrnr_rfcde$new(
    z_grid = seq(0, 10, length.out = 100)
  )
  rfcde_lrn_fit <- rfcde_lrn$train(task)
  rfcde_lrn_fit_preds <- rfcde_lrn_fit$predict() %>% as.numeric()

  set.seed(67391)
  rfcde_fit <- RFCDE::RFCDE(
    z_train = as.numeric(task$Y),
    x_train = as.matrix(task$X),
    n_trees = 1000,
    node_size = 5,
    n_basis = 31,
    basis_system = "cosine",
    min_loss_delta = 0,
    fit_oob = FALSE
  )
  rfcde_fit_preds <- predict(rfcde_fit,
    newdata = as.matrix(task$X),
    z_grid = seq(0, 10, length.out = 100),
    bandwidth = "auto"
  ) %>%
    as.numeric()

  # check that predicted conditional density estimates match within tolerance
  expect_equal(rfcde_lrn_fit_preds,
    expected = rfcde_fit_preds,
    tolerance = 1e-3,
    scale = 1
  )
})
