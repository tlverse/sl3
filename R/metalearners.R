
#from SuperLearner package
trim_logit <- function (x, trim = 1e-05) 
{
  x[x < trim] <- trim
  x[x > (1 - trim)] <- (1 - trim)
  foo <- log(x/(1 - x))
  return(foo)
}

#' @importFrom stats plogis qlogis
metalearner_logistic_binomial <- function(alpha, X, trim){
  plogis(trim_logit(X) %*% alpha)
}

metalearner_linear <- function(alpha, X){
  X %*% alpha
}

metalearner_linear_multinomial <- function(alpha, x) {
  
  unpacked <- lapply(as.data.frame(x),unpack_predictions)
  multiplied <- mapply(`*`, unpacked, alpha, SIMPLIFY = FALSE)
  Y_pred <- Reduce(`+`, multiplied)
  
  # normalize so class predictions sum to 1
  Y_pred <- normalize_rows(Y_pred)
  
  return(pack_predictions(Y_pred))
}