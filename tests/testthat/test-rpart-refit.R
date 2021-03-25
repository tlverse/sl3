context("test-Lrnr_rpart.R -- General testing for Rpart")

library(sl3)
library(testthat)
library(rpart)

gendata <- function(n=1000,p=5){
  x <- matrix(rnorm(n*p),nrow=n)
  ey <- x[,1] + x[,2] 
  y <- ey + rnorm(n,0,0.1)
  
  data.frame(x,ey, y)
}

data <- gendata()
covariates <- setdiff(names(data),c("y","ey"))
task <- make_sl3_Task(data, covariates = covariates, outcome="y")
lrnr_rpart <- make_learner(Lrnr_rpart)
fit <- lrnr_rpart$train(task)
preds <- fit$predict()

# check fit tree names
fit$fit_object$terms

# predict on external data
external <- gendata(300)
ext_task <- make_sl3_Task(external, covariates = covariates, outcome="y")
ext_preds <- fit$predict(ext_task)
mean((external$ey-ext_preds)^2)
