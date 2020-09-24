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
#'   \item{\code{order=NULL}}{An optional specification of the non-seasonal
#'    part of the ARIMA model: the three integer components (p, d, q) are the
#'    AR order, the degree of differencing, and the MA order. If order is
#'    specified, then \code{\link[stats]{arima}} will be called; otherwise,
#'    \code{\link[forecast]{auto.arima}} will be used to fit the "best" ARIMA
#'    model according to AIC (default), AICc or BIC. The information criterion
#'    to be used in \code{\link[forecast]{auto.arima}} model selection can be
#'    modified by specifying \code{ic} argument.}
#'  \item{\code{...}}{Other parameters passed to \code{\link[stats]{arima}} or
#'    \code{\link[forecast]{auto.arima}} function, depending on whether or not
#'    \code{order} argument is provided.}
#' }
#
Lrnr_arima <- R6Class(
  classname = "Lrnr_arima",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(order = NULL,
                          ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous"),

    .train = function(task) {
      params <- self$params

      # option to include external regressors
      if (length(task$X) > 0) {
        params$xreg <- as.matrix(task$X)
      }

      if (is.numeric(params$order)) {
        params$x <- task$Y
        fit_object <- call_with_args(stats::arima, params)
      } else {
        params$y <- task$Y
        fit_object <- call_with_args(forecast::auto.arima, params)
      }

      return(fit_object)
    },

    .predict = function(task = NULL) {
      h <- ts_get_pred_horizon(self$training_task, task)

      # include external regressors
      if (length(task$X) > 0) {
        newxreg <- as.matrix(task$X)
      } else {
        newxreg <- NULL
      }

      raw_preds <- predict(private$.fit_object,
        newdata = task$Y, n.ahead = h,
        newxreg = newxreg, type = "response"
      )
      preds <- as.numeric(raw_preds$pred)

      requested_preds <- ts_get_requested_preds(self$training_task, task, preds)
      return(requested_preds)
    },
    .required_packages = c("forecast")
  )
)
