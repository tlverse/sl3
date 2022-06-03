

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
A <- rbinom(n,1,0.15)     #  added a randomised A
X<- cbind(A,X)            #  made A the first column of X

X.test <- matrix(0, 101, p)
X.test[, 1] <- seq(-2, 2, length.out = 101)
Y <- X[, 1] * rnorm(n) + 0.5*A           # added a constant treatment effect
data <- data.frame(list(Y = Y, X = X))  
names(data)[2]<-"A"                      # this is probably not necessary
# Make sl3 Task
covars <- names(data)[2:ncol(data)]     #  A is in covars
outcome <- names(data)[1]
task <- sl3_Task$new(data, covariates = covars, outcome = outcome)  
                                                                     
                                  
test_that("Lrnr_grfcate predictions match those of grf::causal_forest", {
  seed_int <- 496L
  set.seed(seed_int)
  # GRF learner class
  grfcate_learner <- Lrnr_grfcate$new(seed = seed_int,num.threads = 1L)  # perhaps this num.threads argument caused the discrepancy
  # sl3_debug_mode()
  # debug_train(grfcate_learner)
  grfcate_fit <- grfcate_learner$train(task)
  grfcate_pred <- grfcate_fit$predict(task)
  
  set.seed(seed_int)
  # GRF package                         
  grfcate_pkg <- grf::causal_forest(
    X = X[,-1], W=X[,1], Y = Y, seed = seed_int,     
    num.threads = 1L
  )
  grfcate_pkg_pred_out <- predict(
    grfcate_pkg,
   
  )
  grfcate_pkg_pred <- as.numeric(grfcate_pkg_pred_out$predictions)
  
  

  # test equivalence
  expect_equal(grfcate_pred, grfcate_pkg_pred)
})