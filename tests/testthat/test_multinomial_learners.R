context("multinomial SL")
library(origami)

Qbar0 <- function(A, W) {
    
    W1 <- W[, 1]
    W2 <- W[, 2]
    W3 <- W[, 3]
    W4 <- W[, 4]
    Qbar <- (1/2)*(plogis(-5*(A==2)*(W1+0.5)+5*(A==3)*(W1-0.5))+plogis(W2*W3))
    return(Qbar)
}

g0 <- function(W) {
    W1 <- W[, 1]
    W2 <- W[, 2]
    W3 <- W[, 3]
    W4 <- W[, 4]
    
    # rep(0.5, nrow(W))
    scale_factor <- 0.8
    A1 <- plogis(scale_factor * W1)
    A2 <- plogis(scale_factor * W2)
    A3 <- plogis(scale_factor * W3)
    A <- cbind(A1, A2, A3)
    
    # make sure A sums to 1
    A <- normalize_rows(A)
}

gen_data <- function(n = 1000, p = 4) {
    W <- matrix(rnorm(n * p), nrow = n)
    colnames(W) <- paste("W", seq_len(p), sep = "")
    g0W <- g0(W)
    A <- factor(apply(g0W, 1, function(pAi) which(rmultinom(1, 1, pAi) == 1)))
    A_vals <- levels(A)
    
    u <- runif(n)
    Y <- as.numeric(u < Qbar0(A, W))
    Q0aW <- sapply(A_vals, Qbar0, W)
    d0 <- max.col(Q0aW)
    Yd0 <- as.numeric(u < Qbar0(d0, W))
    df=data.frame(W, A, Y, d0, Yd0)
       
    df$g0W=g0(W)
    df$Q0aW=Q0aW
    
    return(df)
}

data <- gen_data(1000)


Wnodes <- grep("^W", names(data), value = TRUE)
Anode <- "A"
Ynode <- "Y"

task <- sl3_Task$new(data, covariates = Wnodes, outcome=Anode)

rf <- make_learner(Lrnr_randomForest)
xgb <- make_learner(Lrnr_xgboost, nrounds=20)
lrnr_glmnet <- make_learner(Lrnr_glmnet)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glm_fast <- make_learner(Lrnr_glm_fast)
lrnr_multinom_gf <- make_learner(Lrnr_independent_binomial, lrnr_glm_fast)
fit <- lrnr_multinom_gf$train(task)
ct <- fit$chain()
mn_metalearner <- make_learner(Lrnr_solnp, loss_function = mn_loglik, learner_function = mn_logit)
stack <- make_learner(Stack, list(rf, xgb, lrnr_mean, lrnr_glmnet, lrnr_multinom_gf))
sf <- stack$train(task)
ct <- sf$chain()


mf <- mn_metalearner$base_train(ct)


mn_sl <- make_learner(Lrnr_sl, stack, mn_metalearner)
sl_fit <- mn_sl$train(task)
sl_fit$fit_object$cv_meta_fit$fit_object$coef
sl_fit$base_predict()
sl_fit$cv_risk(mn_loglik)
