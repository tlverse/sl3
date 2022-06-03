

context("test grf: Generalized Random Forests")

if (FALSE) {
  setwd("..")
  getwd()
  library("devtools")
  document()
  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  load_all("./")
  setwd("..")
  # INSTALL W/ devtools:
  install("sl3", build_vignettes = FALSE, dependencies = FALSE)
}

# Preliminaries
library(grf)
set.seed(791)

# Generate some data
n <- 500
p <- 10
X <- matrix(rnorm(n * p), n, p)
A <- rbinom(n,1,0.15)     # NK: added a randomised A
X<- cbind(A,X)            # NK:  made A  part of X

# NK: added A
X.test <- matrix(0, 101, p)
X.test[, 1] <- seq(-2, 2, length.out = 101)
Y <- X[, 1] * rnorm(n) + 0.5*A               #NK: added a treatment effect
data <- data.frame(list(Y = Y, X = X))  
names(data)[2]<-"A"
# Make sl3 Task
covars <- names(data)[2:ncol(data)]   # NK: note this now has A in it, but should be OK
#treatment <- names(data)[2]           # NK: added location of treatment = not sure this is good/necessary
outcome <- names(data)[1]
task <- sl3_Task$new(data, covariates = covars, outcome = outcome)  # NK: but how to add A here?
                                                                     # NK: i think Ivana said for now just make A part of X. OK!
                                  # is this going to be a problem for the predictions, that A is part of X? or it will be just ignored (I think)
test_that("Lrnr_grfcate predictions match those of grf::causal_forest", {
  seed_int <- 496L
  set.seed(seed_int)
  # GRF learner class
  grfcate_learner <- Lrnr_grfcate$new(seed = seed_int)
  grfcate_fit <- grfcate_learner$train(task)
  grfcate_pred <- grfcate_fit$predict(task)
  
  set.seed(seed_int)
  # GRF package                         # what arguments are passed?
  grfcate_pkg <- grf::causal_forest(
    X = X, Y = Y, seed = seed_int,     # what to do about A here - we need it
    num.threads = 1L
  )
  grfcate_pkg_pred_out <- predict(
    grfcate_pkg,
    #quantiles = grf_fit$params$quantiles_pred
  )
  grfcate_pkg_pred <- as.numeric(grfcate_pkg_pred_out$predictions)
  
  # test equivalence
  expect_equal(grfcate_pred, grfcate_pkg_pred)
})