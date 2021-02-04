source(here::here("sandbox", "simulation-importance-utils.R"))
#devtools::source_url("https://raw.githubusercontent.com/tlverse/sl3/importance-simulations/sandbox/simulation-importance-utils.R")

###################### data generated under simple linear model ################
DGP_linear <- function(n, sd = 1){
  X1 <- runif(n, 0, 1) 
  X2 <- rnorm(n) 
  X3 <- rbinom(n, 1, 0.5)
  X4 <- rnorm(n, 0, .5) 
  X5 <- rbinom(n, 1, 0.25)
  Y <- X1 + X2 + X3 + rnorm(n, sd)
  data.table(X1, X2, X3, X4, X5, Y)
}

RNGkind(sample.kind = "Rejection")
set.seed(4917)
Bseeds <- sample(1:2^15, 1000)
lrnr <- Lrnr_glm$new()
relevant_covariates_rank <- list("X1"=c(1,2,3), "X2"=c(1,2,3), "X3"=c(1,2,3))
irrelevant_covariates <- c("X4", "X5")
run_simulation_sequence(bootstrap_seeds = Bseeds, gen_data = DGP_linear, 
                        relevant_covariates_rank = relevant_covariates_rank,
                        irrelevant_covariates = irrelevant_covariates, lrnr = lrnr, 
                        cores = 22, save_path = "~/results/DGP_linear/")

##################### data generated under mlbench.friedman1 ###################

# The mlbench::mlbench.friedman1() function is the regression problem Friedman 1,
# as described in Friedman (1991) and Breiman (1996). Inputs are 10 independent 
# variables uniformly distributed on the interval [0, 1], only 5 out of the 10 
# covariates are actually used. The user specifies the number n of samples to be 
# generated, and the standard deviation of a standard Gaussian noise sequence 
# added to the simulated Y. 

# Found here: https://cran.r-project.org/web/packages/datarobot/vignettes/VariableImportance.html
# "Based on the structure of this simulator, we may classify the covariates into 
# an influential subset (x1-x5), and an irrelevant subset (x6-x10). Also, from 
# the model coefficients and the functions involved, we expect x4 to be the 
# most important variable, probably followed by x1 and x2, both comparably 
# important, with x5 probably less important. The influence of x3 is somewhat 
# more difficult to assess due to its quadratic dependence, but it seems likely 
# that this nonlinearity will suppress this variable’s influence since the total 
# range of this term is from 0 to 5, the same as the x5 term.

library(mlbench)
DGP_friedman <- function(n, sd = 1){
  data_list <- mlbench.friedman1(n, sd)
  data.table(data_list$x, Y = data_list$y)
}

RNGkind(sample.kind = "Rejection")
set.seed(4917)
Bseeds <- sample(1:2^15, 1000)
relevant_covariates_rank <- list("V4"=1, "V2"=c(2,3), "V1"=c(2,3), 
                                 "V3"=c(4,5), "V5"=c(4,5))
irrelevant_covariates <- paste0("V", 6:10)

lrn_glm <- Lrnr_glm$new()
lrn_spline <- Lrnr_polspline$new()
lrn_lasso <- Lrnr_glmnet$new()
rf <- Lrnr_ranger$new()
xgb <- Lrnr_xgboost$new()
lrnrs <- c(lrn_lasso, lrn_glm, lrn_spline, rf, xgb)
names(lrnrs) <- c("lasso", "glm", "polspline", "ranger", "xgboost")
lrnr <- Lrnr_sl$new(lrnrs, full_fit = TRUE)

run_simulation_sequence(bootstrap_seeds = Bseeds, gen_data = DGP_friedman, 
                        lrnr = lrnr, cores = 22, save_path = "~/results/DGP_friedman/",
                        relevant_covariates_rank = relevant_covariates_rank, 
                        irrelevant_covariates = irrelevant_covariates)
