library(origami)

Qbar0 <- function(A, W) {
    
    W1 <- W[, 1]
    W2 <- W[, 2]
    W3 <- W[, 3]
    W4 <- W[, 4]
    Qbar <- (1/2)*(plogis(A*(W1-0.5))+plogis(W2*W3))
    return(Qbar)
}

g0 <- function(W) {
    W1 <- W[, 1]
    W2 <- W[, 2]
    W3 <- W[, 3]
    W4 <- W[, 4]
    
    # rep(0.5, nrow(W))
    scale_factor <- 0.8
    A  <- plogis(scale_factor * W1)
}

gen_data <- function(n = 1000, p = 4) {
    W <- matrix(rnorm(n * p), nrow = n)
    colnames(W) <- paste("W", seq_len(p), sep = "")
    g0W <- g0(W)
    A <- rbinom(n, 1, g0W)
    
    u <- runif(n)
    Y <- as.numeric(u < Qbar0(A, W))
    Q0aW <- sapply(c(0,1), Qbar0, W)
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
fit <- lrnr_glm_fast$train(task)

lrnr_glm_fast_true_covars <- make_learner(Lrnr_glm_fast, covariates="W1")
logit_metalearner <- make_learner(Lrnr_solnp, loss_function = loss_squared_error, learner_function = metalearner_linear)
stack <- make_learner(Stack, rf, xgb, lrnr_mean, lrnr_glmnet, lrnr_glm_fast, lrnr_glm_fast_true_covars)
cv_stack <- make_learner(Lrnr_cv, stack, folds = task$folds)
cv_stack_fit <- cv_stack$train(task)
cv_task <- cv_stack_fit$chain()
# debugonce(logit_metalearner$.__enclos_env__$private$.train)
ml_fit <- logit_metalearner$base_train(cv_task)

binom_sl <- make_learner(Lrnr_sl, stack, logit_metalearner)

sl_fit <- binom_sl$train(task)
sl_fit$fit_object$cv_meta_fit$fit_object$coef
sl_fit$base_predict()
sl_fit$cv_risk(loss_squared_error)
