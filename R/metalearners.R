# from SuperLearner package
trim_logit <- function (x, trim = 1e-05)  {
  x[x < trim] <- trim
  x[x > (1 - trim)] <- (1 - trim)
  foo <- log(x / (1 - x))
  return(foo)
}

#' Combine predictions from multiple learners
#'
#' @param alpha a vector of combination coefficients
#' @param X a matrix of predictions
#' @param trim a value use to trim predictions away from 0 and 1.
#'
#' @name metalearners
#' @rdname metalearners
#'
#' @importFrom stats plogis qlogis
#'
#' @export
#
metalearner_logistic_binomial <- function(alpha, X, trim) {
  plogis(trim_logit(X) %*% alpha)
}

#' @rdname metalearners
#'
#' @export
#
metalearner_linear <- function(alpha, X) {
  X %*% alpha
}

#' @rdname metalearners
#'
#' @export
#
metalearner_linear_multinomial <- function(alpha, X) {
  unpacked <- lapply(as.data.frame(X), unpack_predictions)
  multiplied <- mapply(`*`, unpacked, alpha, SIMPLIFY = FALSE)
  Y_pred <- Reduce(`+`, multiplied)
  # normalize so class predictions sum to 1
  Y_pred <- normalize_rows(Y_pred)
  return(pack_predictions(Y_pred))
}

