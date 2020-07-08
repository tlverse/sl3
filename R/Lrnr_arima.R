#' Univariate ARIMA Models
#'
#' This learner supports autoregressive integrated moving average model for
#' univariate time-series.
#'
#' @docType class
#'
#' @family Learners
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
#' @section Parameters:
#' \describe{
#'   \item{\code{order=NULL}}{A specification of the non-seasonal part of the
#'    ARIMA model: the three integer components (p, d, q) are the AR order, the
#'    degree of differencing, and the MA order.}
#'   \item{\code{seasonal=list(order=c(0,0,0) period=NA)}}{ A specification of
#'    the seasonal part of the ARIMA model, plus the period (which defaults to
#'    frequency(x)). This should be a list with components order and period, but
#'    a specification of just a numeric vector of length 3 will be turned into a
#'    suitable list with the specification as the order.}
#'   \item{\code{n.ahead=NULL}}{ The forecast horizon. If not specified, returns
#'    forecast of size \code{task$X}.}
#' }
#
Lrnr_arima <- R6Class(
  classname = "Lrnr_arima",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(order = NULL,
                              seasonal = list(order = c(0L, 0L, 0L), period = NA),
                              n.ahead = NULL, ...) {
      super$initialize(params = args_to_list(), ...)
      if (!is.null(n.ahead)) {
        warning("n.ahead paramater is specified- obtaining an ensemble will fail. 
                Please only use for obtaining individual learner forcasts.")
      }
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous"),
    .train = function(task) {
      params <- self$params
      ord <- params[["order"]]
      season <- params[["seasonal"]]

      # Add option to include external regressors:
      if (length(task$X) > 0) {
        # Support for a single time-series
        if (is.numeric(ord)) {
          fit_object <- stats::arima(task$Y, order = ord, seasonal = season, xreg = as.matrix(task$X))
        } else {
          fit_object <- forecast::auto.arima(task$Y, xreg = as.matrix(task$X))
        }
      } else {
        # Support for a single time-series
        if (is.numeric(ord)) {
          fit_object <- stats::arima(task$Y, order = ord, seasonal = season)
        } else {
          fit_object <- forecast::auto.arima(task$Y)
        }
      }
      return(fit_object)
    },
    .predict = function(task = NULL) {
      h <- ts_get_pred_horizon(self$training_task, task)

      # Include external regressors:
      if (length(task$X) > 0) {
        predictions <- predict(
          private$.fit_object,
          newdata = task$Y, newxreg = as.matrix(task$X),
          type = "response", n.ahead = h
        )
      } else {
        predictions <- predict(
          private$.fit_object,
          newdata = task$Y,
          type = "response", n.ahead = h
        )
      }

      preds <- as.numeric(predictions$pred)
      requested_preds <- ts_get_requested_preds(self$training_task, task, preds)

      return(requested_preds)
    },
    .required_packages = c("forecast")
  )
)
