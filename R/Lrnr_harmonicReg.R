#' Harmonic Regression
#'
#' @description This learner fits first harmonics in a Fourier expansion to one
#'  or more time series. Fourier decomposition relies on
#'  \code{\link[forecast]{fourier}}, and the time series is fit using
#'  \code{\link[forecast]{tslm}}. For further details on working with harmonic
#'  regression for time-series with package \pkg{forecast}, consider consulting
#'  \insertCite{forecast;textual}{sl3}) and
#'  \insertCite{hyndman2008forecast-jss;textual}{sl3}).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats arima
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'  - \code{K}: Maximum order of the fourier terms. Passed to
#'      \code{\link[forecast]{fourier}}.
#'  - \code{freq}: The frequency of the time series.
#'  - \code{...}: Other parameters passed to \code{\link[forecast]{fourier}}.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' library(origami)
#' library(data.table)
#' data(bsds)
#'
#' # make folds appropriate for time-series cross-validation
#' folds <- make_folds(bsds,
#'   fold_fun = folds_rolling_window, window_size = 500,
#'   validation_size = 100, gap = 0, batch = 50
#' )
#'
#' # build task by passing in external folds structure
#' task <- sl3_Task$new(
#'   data = bsds,
#'   folds = folds,
#'   covariates = c(
#'     "weekday", "temp"
#'   ),
#'   outcome = "cnt"
#' )
#'
#' # create tasks for taining and validation
#' train_task <- training(task, fold = task$folds[[1]])
#' valid_task <- validation(task, fold = task$folds[[1]])
#'
#' # instantiate learner, then fit and predict
#' HarReg_learner <- Lrnr_HarmonicReg$new(K = 7, freq = 105)
#' HarReg_fit <- HarReg_learner$train(train_task)
#' HarReg_preds <- HarReg_fit$predict(valid_task)
Lrnr_HarmonicReg <- R6Class(
  classname = "Lrnr_HarmonicReg",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(K, freq, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous"),
    .train = function(task) {
      args <- self$params
      args$x <- ts(task$Y, frequency = args$freq)

      # Checks
      if (length(args$freq) != length(args$K)) {
        stop("Number of periods does not match number of orders")
      } else if (any(2 * args$K > args$freq)) {
        stop("K must be not be greater than period/2")
      }

      # Passes a warning for an extra argument: that's ok
      # forecast::fourier doesn't take freq as an argument anymore
      fourier_fit <- call_with_args(
        forecast::fourier, args,
        ignore = "freq"
      )
      fit_object <- forecast::tslm(args$x ~ fourier_fit)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      h <- ts_get_pred_horizon(self$training_task, task)
      x <- ts(task$Y, frequency = self$params$freq)
      fourier_fit <- data.frame(
        forecast::fourier(
          x,
          K = self$params$K,
          h = h
        )
      )
      raw_preds <- forecast::forecast(private$.fit_object, fourier_fit)
      preds <- as.numeric(raw_preds$mean)
      requested_preds <- ts_get_requested_preds(
        self$training_task, task, preds
      )
      return(requested_preds)
    },
    .required_packages = c("forecast")
  )
)
