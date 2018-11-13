library(tidyverse)
library(condensier)
library(hal9001)
library(sl3)

# data simulation
sim_data_set <- function(n_obs = 1000, w_prob = 0.5, shift_delta = 0.5) {
  w <- rbinom(n = n_obs, size = 1, prob = w_prob)
  ipc_delta <- rbinom(n = n_obs, size = 1, prob = plogis(w))
  a <- rnorm(n = n_obs, mean = 2 * w, sd = 1)
  y <- a + w + rnorm(n_obs, mean = 0, sd = 1)
  data_in <- as.data.frame(cbind(y, a, ipc_delta, w, 1 / plogis(w))) %>%
    dplyr::filter(ipc_delta == 1) %>%
    dplyr::select(-ipc_delta) %>%
    as.data.table()
  setnames(data_in, c("Y", "A", "W", "Weights"))
  return(data_in)
}

# simulate data and set up task
data_in <- sim_data_set()
sl_task <- sl3_Task$new(data_in, covariates = "W", outcome = "A")

# set up learner with HAL density
lrn <- Lrnr_condensier$new(nbins = 10, bin_method = "equal.len", pool = TRUE, 
                           bin_estimator = Lrnr_hal9001_density$new())

# train and predict from learner
trained_lrn <- lrn$train(sl_task)
predictions <- trained_lrn$predict()
predictions

