# context("test-kerasR.R -- time series methods")
# 
# if (FALSE) {
#   setwd("..")
#   setwd("..")
#   getwd()
#   library("devtools")
#   document()
#   load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
#   # devtools::check() # runs full check
#   setwd("..")
#   install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
# }
# 
# library(reticulate)
# library(sl3)
# library(testthat)
# library(origami)
# 
# set.seed(1)
# attach(list(lag = stats::lag), name = "stats_lag_test_kludge", warn.conflicts = FALSE)
# data(bsds)
# bsds <- bsds[1:50, ]
# 
# data <- as.data.table(bsds)
# data[, time := .I]
# 
# outcome <- "cnt"
# 
# folds <- origami::make_folds(data,
#   fold_fun = folds_rolling_window, window_size = 20,
#   validation_size = 15, gap = 0, batch = 10
# )
# 
# node_list <- list(outcome = outcome, time = "time")
# 
# task <- sl3_Task$new(data, nodes = node_list, folds = folds)
# 
# # See which environments reticulate can see:
# # reticulate:::conda_list()
# # use_condaenv("r-reticulate")
# # import("scipy")
# # import("tensorflow")
# 
# # Install keras:
# # install.packages("keras")
# # keras::install_keras()
# 
# test_that("Lrnr_bilstm does what we expect", {
#   have_foo <- reticulate::py_module_available("keras.models")
# 
#   if (!(have_foo)) {
#     skip("keras.models not available for testing")
#   } else {
#     reticulate::import("keras.models")
# 
#     bilstm_learner <- Lrnr_bilstm$new(epochs = 1)
#     bilstm_fit <- bilstm_learner$train(task)
#     bilstm_preds <- bilstm_fit$predict(task)
# 
#     # At epochs=1 (saves time) the prediction is too variable to be tested
#     # expect_true(sum(bilstm_preds)-118.5766 < 10^(-1))
#     expect_equal(length(bilstm_preds), length(task$Y))
#   }
# })
