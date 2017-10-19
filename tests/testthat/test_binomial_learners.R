context("test_binomial_learners.R -- binomial learners in a Super Learner")
library(origami)


g0 <- function(W) {
    W1 <- W[, 1]
    scale_factor <- 0.8
    A  <- plogis(scale_factor * W1)
}

gen_data <- function(n = 1000, p = 4) {
    W <- matrix(rnorm(n * p), nrow = n)
    colnames(W) <- paste("W", seq_len(p), sep = "")
    g0W <- g0(W)
    A <- rbinom(n, 1, g0W)
    
    u <- runif(n)
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
  rf = make_learner(Lrnr_randomForest),
  xgb = make_learner(Lrnr_xgboost),
  glmnet = make_learner(Lrnr_glmnet),
  glm_fast = make_learner(Lrnr_glm_fast),
  glm_fast_true_covars = make_learner(Lrnr_glm_fast, covariates="W1"),
  mean = make_learner(Lrnr_mean)
)

#define metalearner
logit_metalearner <- make_learner(Lrnr_solnp, 
                                  loss_function = loss_loglik_binomial, 
                                  learner_function = metalearner_logistic_binomial)

#define Super Learner
binom_sl <- make_learner(Lrnr_sl, learners, logit_metalearner)

# test_that("Binomial Lrnr_sl components for debugging", {
#   stack <- make_learner(Stack, learners)
#   stack_fit <- stack$train(task)
# 
#   stack_chained <- stack_fit$chain()
# 
#   cv_stack <- make_learner(Lrnr_cv, stack)
#   cv_stack_fit <- cv_stack$train(task)
#   cv_stack_chained <- cv_stack_fit$chain()
#   meta_fit <- logit_metalearner$base_train(cv_stack_chained)
#   coef(meta_fit)
# })

test_that("Lrnr_sl binomial integration test", {
  #fit and generate predictions
  sl_fit <- binom_sl$train(task)
  coef(sl_fit$fit_object$cv_meta_fit)
  preds <- sl_fit$predict()
  sl_fit$cv_risk(loss_loglik_binomial)
})