#' Harmonic Regression
#'
#' @description This learner fits first harmonics in a Fourier expansion to one
#' or more time series. Fourier decomposition relies on
#' \code{\link[forecast]{fourier}}, and the time series is fit using
#' \code{\link[forecast]{tslm}}
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom stats arima
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{Kparam}}{Maximum order of the fourier terms. Passed to
#'     \code{\link[forecast]{fourier}}.}
#'   \item{\code{n.ahead=NULL}}{ The forecast horizon. If not specified, returns
#'     forecast of size \code{task$X}.}
#'   \item{\code{freq}}{The frequency of the time series.}
#'   \item{\code{...}}{Not used.}
#' }
#
Lrnr_HarmonicReg <- R6Class(
  classname = "Lrnr_HarmonicReg",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(Kparam, n.ahead = NULL, freq, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("timeseries", "continuous"),

    .train = function(task) {
      params <- self$params
      Kparam <- params[["Kparam"]]
      freq <- params[["freq"]]
      task_ts <- ts(task$X, frequency = freq)

      if (length(freq) != length(Kparam)) {
        stop("Number of periods does not match number of orders")
      } else if (any(2 * Kparam > freq)) {
        stop("K must be not be greater than period/2")
      }

      fourier_fit <- forecast::fourier(task_ts, K = Kparam)
      fit_object <- forecast::tslm(task_ts~fourier_fit)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      params <- self$params
      n.ahead <- params[["n.ahead"]]
      freq <- params[["freq"]]
      Kparam <- params[["Kparam"]]

      if (is.null(n.ahead)) {
        n.ahead <- nrow(task$X)
      }

      task_ts <- ts(task$X, frequency = freq)
      fourier_fit <- data.frame(forecast::fourier(
        task_ts, K = Kparam,
        h = n.ahead
      ))
      predictions <- forecast::forecast(private$.fit_object, fourier_fit)

      # Create output as in glm
      predictions <- as.numeric(predictions$mean)
      predictions <- structure(predictions, names = seq_len(n.ahead))
      return(predictions)
    },

    .required_packages = c("forecast")
  )
)
