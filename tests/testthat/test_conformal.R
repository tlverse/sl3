context("test-conformal.R -- Lrnr_conformal_inference")

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

#Generate the data:
data_train <- gen_data_DGP0(n=200)
data_test  <- gen_data_DGP0(n=200)

task_train <- sl3_Task$new(data_train, outcome = "Y", 
                           covariates = c("W1"), id = "ID")
task_test  <- sl3_Task$new(data_test, outcome = "Y", 
                           covariates = c("W1"), id = "ID")

test_that("Test that Lrnr_conformal_inference works", {
  lrnr_conf <- Lrnr_conformal_quantile$new(task=task_train)
  lrnr_conf$train()
  preds<- lrnr_conf$predict(task=task_test)
  
  expect_equal(dim(preds)[2], 2)
})
