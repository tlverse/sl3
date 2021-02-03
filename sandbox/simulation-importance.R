source(here::here("sandbox", "simulation-importance-utils.R"))

###################### data generated under simple linear model ################
DGP_linear <- function(n){
  X1 <- runif(n, 0, 1) 
  X2 <- rnorm(n) 
  X3 <- rbinom(n, 1, 0.5)
  X4 <- rnorm(n, 0, .5) 
  X5 <- rbinom(n, 1, 0.25)
  Y <- X1 + X2 + X3 + rnorm(n)
  data.table(X1, X2, X3, X4, X5, Y)
}

RNGkind(sample.kind = "Rejection")
set.seed(4917)
Bseeds <- sample(1:2^15, 2)
run_simulation_sequence(bootstrap_seeds = Bseeds, gen_data = DGP_linear, 
                        lrnr = Lrnr_glm$new(), N = 1e6, 
                        n_sequence = c(50, 100, 500, 1000, 5000),
                        loss = loss_squared_error, outcome = "Y", 
                        covariates = NULL, cores = 1,
                        save_path = "~/Desktop/sl3_importance_testing/")

##################### data generated under mlbench.friedman1 ###################

# The mlbench::mlbench.friedman1() function is the regression problem Friedman 1,
# as described in Friedman (1991) and Breiman (1996). Inputs are 10 independent 
# variables uniformly distributed on the interval [0, 1], only 5 out of the 10 
# covariates are actually used. The user specifies the number n of samples to be 
# generated, and the standard deviation of a standard Gaussian noise sequence 
# added to the simulated Y.

library(mlbench)
DGP_friedman <- function(n, sd = 1){
  data_list <- mlbench.friedman1(n, sd)
  data.table(data_list$x, Y = data_list$y)
}

# lrn_glm <- Lrnr_glm$new()
# lrn_spline <- Lrnr_polspline$new()
# grid_params <- list(alpha = seq(0, 1, 0.2))
# grid <- expand.grid(grid_params, KEEP.OUT.ATTRS=F)
# glmnets <- apply(grid, 1, function(par) do.call(Lrnr_glmnet$new, as.list(par)))
# rf <- Lrnr_ranger$new()
# xgb <- Lrnr_xgboost$new(max_depth=4, eta=0.1, nrounds=50,
#                         colsample_bytree=0.8, early_stopping_rounds=10)
# lrnrs <- c(glmnets, lrn_glm, lrn_spline, rf, xgb)
# names(lrnrs) <- c("enet_0", "enet_0.2", "enet_0.4", "enet_0.6", "enet_0.8", 
#                   "enet_1", "glm", "polspline", "ranger","xgboost")
# lrnr <- Lrnr_sl$new(lrnrs, full_fit = TRUE)
RNGkind(sample.kind = "Rejection")
set.seed(4917)
Bseeds <- sample(1:2^15, 2)
run_simulation_sequence(bootstrap_seeds = Bseeds, gen_data = DGP_friedman, 
                        lrnr = Lrnr_glm$new(), n_sequence = c(50, 100),
                        save_path = "~/Desktop/sl3_importance_testing/")