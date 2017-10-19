context("test_arima.R -- time series methods")

if (FALSE) {
    setwd("..")
    setwd("..")
    getwd()
    library("devtools")
    document()
    load_all("./")  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
    # devtools::check() # runs full check
    setwd("..")
    install("sl3", build_vignettes = FALSE, dependencies = FALSE)  # INSTALL W/ devtools:
}

library(testthat)
library(sl3)
set.seed(1)

data(bsds)
covars <- c("cnt")
outcome <- "cnt"

task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome)

test_that("Lrnr_arima gives expected values with auto.arima", {
    arima_learner <- Lrnr_arima$new(n.ahead=1)
    arima_fit <- arima_learner$train(task)
    arima_preds <- arima_fit$predict(task)
    
    arima_fit_2 <- forecast::auto.arima(bsds$cnt)
    arima_preds_2 <- predict(arima_fit_2)
    arima_preds_2 <- as.numeric(arima_preds_2$pred)
    arima_preds_2  <- structure(arima_preds_2, names=1)
    
    expect_true(sum(arima_preds - arima_preds_2) < 10^(-10))
    expect_true(all.equal(arima_preds_2, arima_preds))
})

test_that("Lrnr_arima gives expected values with arima order set", {
    arima_learner <- Lrnr_arima$new(order = c(3, 1, 6), n.ahead=1)
    arima_fit <- arima_learner$train(task)
    arima_preds <- arima_fit$predict(task)
    
    arima_fit_2 <- arima(bsds$cnt, order = c(3, 1, 6))
    arima_preds_2 <- predict(arima_fit_2)
    arima_preds_2 <- as.numeric(arima_preds_2$pred)
    arima_preds_2  <- structure(arima_preds_2, names=1)
    
    expect_true(sum(arima_preds - arima_preds_2) < 10^(-10))
    expect_true(all.equal(arima_preds_2, arima_preds))
})

test_that("Lrnr_arima with further forecasts", {
    arima_learner <- Lrnr_arima$new(n.ahead = 5)
    arima_fit <- arima_learner$train(task)
    arima_preds <- arima_fit$predict(task)
    
    arima_fit_2 <- forecast::auto.arima(bsds$cnt)
    arima_preds_2 <- predict(arima_fit_2, n.ahead = 5)
    arima_preds_2 <- as.numeric(arima_preds_2$pred)
    arima_preds_2  <- structure(arima_preds_2, names=1:5)
    
    expect_true(sum(arima_preds - arima_preds_2) < 10^(-10))
    expect_true(all.equal(arima_preds_2, arima_preds))
})

test_that("Lrnr_tsDyn with multiple different models, univariate", {
  
  #AR(m) model
  tsDyn_learner <- Lrnr_tsDyn$new(learner="linear", m=1, n.ahead=5)
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)
  
  fit_2 <- tsDyn::linear(bsds$cnt, m=1)
  fit_2_preds <- predict(fit_2,n.ahead=5)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds  <- structure(fit_2_preds, names=1:5)
  
  expect_true(all.equal(fit_1_preds, fit_2_preds))
  
  #self exciting threshold autoregressive model
  tsDyn_learner <- Lrnr_tsDyn$new(learner="setar", m=1, model="TAR", n.ahead=5)
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)
  
  fit_2 <- tsDyn::setar(bsds$cnt, m=1)
  fit_2_preds <- predict(fit_2,n.ahead=5)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds  <- structure(fit_2_preds, names=1:5)
  
  expect_true(all.equal(fit_1_preds, fit_2_preds))
  
  #Logistic Smooth Transition autoregressive model
  tsDyn_learner <- Lrnr_tsDyn$new(learner="lstar", m=1, n.ahead=5)
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)
  
  fit_2 <- tsDyn::lstar(bsds$cnt, m=1)
  fit_2_preds <- predict(fit_2,n.ahead=5)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds  <- structure(fit_2_preds, names=1:5)
  
  expect_true(all.equal(fit_1_preds, fit_2_preds))
  
  #STAR model fitting with automatic selection of the number of regimes
  #tsDyn_learner <- Lrnr_tsDyn$new(learner="star", m=1, n.ahead=5)
  #fit_1 <- tsDyn_learner$train(task)
  #fit_1_preds <- fit_1$predict(task)
  
  #fit_2 <- tsDyn::star(bsds$cnt, m=1)
  #fit_2_preds <- predict(fit_2,n.ahead=5)
  #fit_2_preds <- as.numeric(fit_2_preds)
  #fit_2_preds  <- structure(fit_2_preds, names=1:5)
  
  #expect_true(all.equal(fit_1_preds, fit_2_preds))
  
  #Additive nonlinear autoregressive model
  # tsDyn_learner <- Lrnr_tsDyn$new(learner="aar", m=1, n.ahead=5)
  # fit_1 <- tsDyn_learner$train(task)
  # fit_1_preds <- fit_1$predict(task)
  
  # fit_2 <- tsDyn::aar(bsds$cnt, m=1)
  # fit_2_preds <- predict(fit_2,n.ahead=5)
  # fit_2_preds <- as.numeric(fit_2_preds)
  # fit_2_preds  <- structure(fit_2_preds, names=1:5)
  # 
  # expect_true(all.equal(fit_1_preds, fit_2_preds))  
})

test_that("Lrnr_tsDyn with multiple different models, multivariate", {
  
  #Estimate multivariate threshold VAR
  
  #Define new data:
  covars <- c("temp", "atemp")
  outcome <- c("temp", "atemp")
  
  task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome)
  
  tsDyn_learner <- Lrnr_tsDyn$new(learner="lineVar", lag=2, n.ahead=5)
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)
  
  fit_2 <- tsDyn::lineVar(task$X, lag=2)
  fit_2_preds <- predict(fit_2,n.ahead=5)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds  <- structure(fit_2_preds, names=1:5)
  
  expect_true(all.equal(fit_1_preds, fit_2_preds))   
  
  #Estimation of Vector error correction model (VECM)
  tsDyn_learner <- Lrnr_tsDyn$new(learner="VECM", lag=2, n.ahead=5, type="linear")
  params<-tsDyn_learner$params
  fit_1 <- tsDyn_learner$train(task)
  fit_1_preds <- fit_1$predict(task)
  
  fit_2 <- tsDyn::VECM(task$X, lag=2)
  fit_2_preds <- predict(fit_2,n.ahead=5)
  fit_2_preds <- as.numeric(fit_2_preds)
  fit_2_preds  <- structure(fit_2_preds, names=1:5)
  
  expect_true(all.equal(fit_1_preds, fit_2_preds))  
  
  #Multivariate Threshold autoregressive model (TVAR)
  #tsDyn_learner <- Lrnr_tsDyn$new(learner="TVAR", lag=2, model="TAR", thDelay=1, trim=0.1)
  #params<-tsDyn_learner$params
  #fit_1 <- tsDyn_learner$train(task)
  
  #fit_2 <- tsDyn::TVAR(task$X, lag=2)
  
})
