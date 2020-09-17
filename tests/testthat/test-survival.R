context("test-survival.R -- Pooled hazards model")
options(sl3.verbose = FALSE)
library(data.table)
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

  df <- data.table(W, A)
  g0W <- g0(W)
  return(list(data = df, truth = g0W))
}

set.seed(1234)
sim_results <- gen_data(1000)
dat <- sim_results$data
dat <- dat[, A := as.numeric(A)]
g0W <- sim_results$truth

Wnodes <- grep("^W", names(dat), value = TRUE)
Anode <- "A"

task <- sl3_Task$new(dat, covariates = Wnodes, outcome = Anode)
hazards_task <- pooled_hazard_task(task)
lrnr_ph <- make_learner(Lrnr_pooled_hazards,
  binomial_learner = make_learner(Lrnr_xgboost)
)
fit <- lrnr_ph$train(task)
preds <- unpack_predictions(fit$base_predict())
p0 <- g0(as.matrix(task$X))
mean(rowSums(p0 * log(preds)))

lrnr_xgb <- make_learner(Lrnr_xgboost)
xgb_fit <- lrnr_xgb$train(task)
xgb_preds <- unpack_predictions(xgb_fit$predict(task))
mean(rowSums(p0 * log(xgb_preds)))

lrnr_mean <- make_learner(Lrnr_mean)
mean_fit <- lrnr_mean$train(task)
mean_preds <- unpack_predictions(mean_fit$predict(task))
mean(rowSums(p0 * log(mean_preds)))
