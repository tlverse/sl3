#' Inverse CDF Sampling
#'
#' @param cdf A list with x and y representing the cdf
#' @param pdf A list with x and y representing the pdf
#' @param n_samples If \code{true}, remove entries after failure time for each
#'  observation.
#'
#' @importFrom stats runif
#'
#' @export
inverse_sample <- function(n_samples, cdf = NULL, pdf = NULL) {
  if (is.null(cdf) && is.null(pdf)) {
    stop("CDF and PDF cannot both be NULL.")
  } else if (is.null(cdf)) {
    cdf$x <- pdf$x
    cdf$y <- vector(mode = "numeric", length = length(pdf$y))
    cdf$y[1] <- pdf$y[1]
    for (i in 2:length(pdf$y)) {
      cdf$y[i] <- cdf$y[i - 1] + pdf$y[i]
    }
  }

  U <- stats::runif(n_samples, 0, cdf$y[length(cdf$y)])
  X <- vector(mode = "numeric", length = n_samples)
  for (i in seq_len(n_samples)) {
    for (j in 1:length(cdf$x)) {
      if (cdf$y[j] > U[i]) {
        X[i] <- cdf$x[j]
        break
      }
    }
  }
  return(X)
}
