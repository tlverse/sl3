# squared error loss
loss_squared_error <- function(pred, truth) {
  (pred - truth)^2
}

# negative log-likelihood loss
# assumes pred is p(Y=truth)
# therefore, truth is not actually used
loss_loglik_true_cat <- function(pred, truth) {
  -log(pred)
}

# negative log-likelihood loss
# for binomial outcome
loss_loglik_binomial <- function(pred, truth) {
  -1*ifelse(truth==1, log(pred), log(1-pred))
}

# negative log-likelihood loss
# for multinomial outcome
# assumes predicted probabilities are "packed" into a single vector
loss_loglik_multinomial <- function(pred, truth) {
  #make index matrix
  index_mat <- cbind(seq_along(truth), truth)
  unpacked <- unpack_predictions(pred)
  class_liks <- log(unpacked[index_mat])
  return(-1 * class_liks)
}

# estimate risk for a given loss function
#' @importFrom stats weighted.mean
risk <- function(pred, truth, loss = loss_squared_error, weights = NULL) {
  if (is.null(weights)) {
      weights <- rep(1, length(truth))
  }
  risk <- weighted.mean(loss(truth, pred), weights)
}
