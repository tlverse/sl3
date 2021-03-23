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
#' @importFrom stats arima predict
#' @importFrom caret findLinearCombos
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
#'  - \code{order}: An optional specification of the non-seasonal
#'    part of the ARIMA model; the three integer components (p, d, q) are the
#'    AR order, the degree of differencing, and the MA order. If order is
#'    specified, then \code{\link[stats]{arima}} will be called; otherwise,
#'    \code{\link[forecast]{auto.arima}} will be used to fit the "best" ARIMA
#'    model according to AIC (default), AIC or BIC. The information criterion
#'    to be used in \code{\link[forecast]{auto.arima}} model selection can be
#'    modified by specifying \code{ic} argument.
#'  - \code{num_screen = 5}: The top n number of "most impotant" variables to
#'      retain.
#'  - \code{...}: Other parameters passed to \code{\link[stats]{arima}} or
#'    \code{\link[forecast]{auto.arima}} function, depending on whether or not
#'    \code{order} argument is provided.
#'
#' @examples
#' library(origami)
#' data(bsds)
#'
#' folds <- make_folds(bsds,
#'   fold_fun = folds_rolling_window, window_size = 500,
#'   validation_size = 100, gap = 0, batch = 50
#' )
#'
#' task <- sl3_Task$new(
#'   data = bsds,
#'   folds = folds,
#'   covariates = c(
#'     "weekday", "temp"
#'   ),
#'   outcome = "cnt"
#' )
#'
#' arima_lrnr <- make_learner(Lrnr_arima)
#'
#' train_task <- training(task, fold = task$folds[[1]])
#' valid_task <- validation(task, fold = task$folds[[1]])
#'
#' arima_fit <- arima_lrnr$train(train_task)
#' arima_preds <- arima_fit$predict(valid_task)
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

        # determines if the matrix is full rank & then identifies the sets of
        # columns that are involved in the dependencies.
        rm_idx <- caret::findLinearCombos(task$X)$remove

        if (length(rm_idx) > 0) {
          params$xreg <- as.matrix(task$X[, -rm_idx, with = FALSE])
          print(paste(c(
            "ARIMA requires matrix of external regressors to not be rank ",
            "deficient. The following covariates were removed to counter the ",
            "linear combinations:", names(task$X)[rm_idx]
          ), collapse = " "))
        } else {
          params$xreg <- as.matrix(task$X)
        }
      }

      if (is.numeric(params$order)) {
        params$x <- task$Y
        fit_object <- call_with_args(stats::arima, params)
      } else {
        params$y <- task$Y
        fit_object <- call_with_args(
          forecast::auto.arima, params,
          ignore = "order"
        )
      }

      return(fit_object)
    },

    .predict = function(task = NULL) {
      fit_object <- private$.fit_object
      h <- ts_get_pred_horizon(self$training_task, task)

      # include external regressors 'newxreg' if 'xreg' was used for training
      if (length(task$X) > 0) {
        xreg <- fit_object$xreg
        if (!is.null(xreg)) {
          newxreg <- as.matrix(task$X)
          # ensure 'xreg' and 'newxreg' have same number & order of columns
          newxreg <- newxreg[, match(colnames(xreg), colnames(newxreg))]
        } else {
          warning(
            "Cannot include external regressors for prediction, ",
            "since they were not used for training."
          )
          newxreg <- NULL
        }
      } else {
        newxreg <- NULL
      }

      raw_preds <- stats::predict(fit_object,
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
