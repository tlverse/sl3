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

# make separate tasks with W = 0 and W = 1
n_samp <- 5000
A_supp <- seq(-6, 6, length = n_samp)
W0 <- rep(0, n_samp)
data_W0 <- as.data.table(cbind(A_supp, W0))
setnames(data_W0, c("A", "W"))
task_W0 <- sl3_Task$new(data_W0, covariates = "W", outcome = "A")

predictions_W0 <- trained_lrn$predict(task_W0)
hist_W0 <- predictions_W0 %>%
  ggplot(aes(x = likelihood)) + geom_histogram()


W1 <- rep(1, n_samp)
data_W1 <- as.data.table(cbind(A_supp, W1))
setnames(data_W1, c("A", "W"))
task_W1 <- sl3_Task$new(data_W1, covariates = "W", outcome = "A")

predictions_W1 <- trained_lrn$predict(task_W1)
hist_W1 <- predictions_W1 %>%
  ggplot(aes(x = likelihood)) + geom_histogram()
ggsave(hist_W1, filename = "~/hal_condensier_example.pdf")

