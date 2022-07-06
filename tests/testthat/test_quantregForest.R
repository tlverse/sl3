context("test-quantregForest.R -- Lrnr_quantregForest")

library(origami)
library(data.table)

gen_data_DGP0 = function(n,p=1,f_x=NULL,sd=0.5){
  #Generate covariates
  X<-matrix(runif(n*p,min=-1,max=1), nrow = n)
  colnames(X) <- paste("W", seq_len(p), sep = "")
  
  #Generate the outcome
  Y<-rnorm(n, mean=X[,1], sd=sd)
  
  data<-cbind.data.frame(ID=seq(1:n), X,Y=Y)
  return(data)
}

#Generate the data and task:
data_train <- gen_data_DGP0(n=100)
data_test  <- gen_data_DGP0(n=100)
task_train <- sl3_Task$new(data_train, outcome = "Y", 
                           covariates = c("W1"), id = "ID")
task_test <- sl3_Task$new(data_test, outcome = "Y",
                          covariates = c("W1"), id = "ID")

test_that("Test that Lrnr_quantregForest works", {
  lrnr_qf <- Lrnr_quantregForest$new()
  lrnr_qf_fit <- lrnr_qf$train(task_train)
  preds <- lrnr_qf_fit$predict(task_test)
  
  expect_equal(dim(preds)[2], 2)
})
