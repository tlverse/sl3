loss_squared_error <- function(pred, truth) {
  (pred - truth)^2
}

## negative log-likelihood loss
## truth is missing and never actually used
loss_loglike <- function(pred, truth) {
  -log(pred)
}

## negative log-likelihood loss
loss_binom_loglik <- function(pred, truth) {
  -1*ifelse(truth==1, log(pred), log(1-pred))
}

metalearner_binom_logit <- function(alpha, X){
  plogis(qlogis(X) %*% alpha)
}

metalearner_linear <- function(alpha, X){
  X %*% alpha
}

#' @importFrom stats weighted.mean
risk <- function(pred, truth, loss = loss_squared_error, weights = NULL) {
  if (is.null(weights)) {
      weights <- rep(1, length(truth))
  }
  risk <- weighted.mean(loss(truth, pred), weights)
}
