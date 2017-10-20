context("test_multinomial_learners.R -- multinomial learners in a Super Learner")
library(origami)

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
    
    df=data.frame(W, A)
       
    df$g0W=g0(W)
    
    return(df)
}

set.seed(1234)
data <- gen_data(1000)


Wnodes <- grep("^W", names(data), value = TRUE)
Anode <- "A"
task <- sl3_Task$new(data, covariates = Wnodes, outcome=Anode)

#define learners
learners <- list(
    rf <- make_learner(Lrnr_randomForest),
    xgb <- make_learner(Lrnr_xgboost),
    glmnet <- make_learner(Lrnr_glmnet),
    multinom_gf <- make_learner(Lrnr_independent_binomial, make_learner(Lrnr_glm_fast)),
    mean <- make_learner(Lrnr_mean)
)

#define metalearner
mn_metalearner <- make_learner(Lrnr_solnp, loss_function = loss_loglik_multinomial, learner_function = metalearner_linear_multinomial)

#define Super Learner
mn_sl <- make_learner(Lrnr_sl, learners, mn_metalearner)

test_that("Lrnr_sl multinomial integration test", {
  sl_fit <- mn_sl$train(task)
  coef(sl_fit)
  preds <- sl_fit$base_predict()
  sl_fit$cv_risk(loss_loglik_multinomial)
})