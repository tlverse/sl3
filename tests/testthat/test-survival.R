context("test-survival.R -- Pooled hazards model")
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

  df <- data.frame(W, A)

  df$g0W <- g0(W)

  return(df)
}

set.seed(1234)
data <- gen_data(1000)


Wnodes <- grep("^W", names(data), value = TRUE)
Anode <- "A"

task <- sl3_Task$new(data, covariates = Wnodes, outcome = Anode)
lrnr_ph <- make_learner(Lrnr_pooled_hazards, binomial_learner=make_learner(Lrnr_xgboost))
fit <- lrnr_ph$train(task)
preds <- unpack_predictions(fit$base_predict())
p0 <- g0(as.matrix(task$X))
mean(rowSums(p0*log(preds)))

lrnr_glmnet <- make_learner(Lrnr_glmnet)
glmnet_fit <- lrnr_glmnet$train(task)
glmnet_preds <- unpack_predictions(glmnet_fit$predict(task))
mean(rowSums(p0*log(glmnet_preds)))

lrnr_mean <- make_learner(Lrnr_mean)
mean_fit <- lrnr_mean$train(task)
mean_preds <- unpack_predictions(mean_fit$predict(task))
mean(rowSums(p0*log(mean_preds)))
