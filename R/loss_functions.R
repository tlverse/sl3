#' Loss Function Definitions
#'
#' Loss functions for use in evaluating learner fits
#'
#' @param pred A vector of predicted values
#' @param truth A vector of true values
#'
#' @return A vector of loss values
#'
#' @name loss_functions
#

# SQUARED ERROR LOSS
#
#' @rdname loss_functions
#'
#' @export
#
loss_squared_error <- function(pred, truth) {
  out <- (pred - truth) ^ 2
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS
#
# assumes pred is p(Y = truth); therefore, truth is not actually used
#
#' @rdname loss_functions
#'
#' @export
#
loss_loglik_true_cat <- function(pred, truth) {
  out <- -log(pred)
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS FOR BINOMIAL OUTCOMES
#
#' @rdname loss_functions
#'
#' @export
#
loss_loglik_binomial <- function(pred, truth) {
  out <- -1 * ifelse(truth == 1, log(pred), log(1 - pred))
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS FOR MULTINOMIAL OUTCOMES
#
# Assumes predicted probabilities are "packed" into a single vector
#
#' @rdname loss_functions
#'
#' @export
#
loss_loglik_multinomial <- function(pred, truth) {
  # make index matrix
  index_mat <- cbind(seq_along(truth), truth)
  unpacked <- unpack_predictions(pred)
  class_liks <- log(unpacked[index_mat])
  return(-1 * class_liks)
}

#' Risk Esimation
#'
#' Estimates a risk for a given set of predictions and loss function.
#'
#' @param pred A vector of predicted values.
#' @param truth A vector of true values.
#' @param loss A loss function. For options, see \link{loss_functions}.
#' @param weights A vector of weights.
#'
#' @importFrom stats weighted.mean
#'
#' @export
#
risk <- function(pred, truth, loss = loss_squared_error, weights = NULL) {
  if (is.null(weights)) {
      weights <- rep(1, length(truth))
  }
  risk <- weighted.mean(loss(truth, pred), weights)
  return(risk)
}

