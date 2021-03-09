#' Exponential Smoothing state space model
#'
#' This learner supports exponential smoothing models using
#' \code{\link[forecast]{ets}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
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
#'  - \code{model="ZZZ"}: Three-character string identifying method. In all
#'     cases, "N"=none, "A"=additive, "M"=multiplicative, and "Z"=automatically
#'     selected. The first letter denotes the error type, second letter denotes
#'     the trend type, third letter denotes the season type. For example, "ANN"
#'     is simple exponential smoothing with additive errors, "MAM" is
#'     multiplicative Holt-Winters' methods with multiplicative errors, etc.
#'  - \code{damped=NULL}: If TRUE, use a damped trend (either additive or
#'     multiplicative). If NULL, both damped and non-damped trends will be tried
#'     and the best model (according to the information criterion ic) returned.
#'  - \code{alpha=NULL}: Value of alpha. If NULL, it is estimated.
#'  - \code{beta=NULL}: Value of beta. If NULL, it is estimated.
#'  - \code{gamma=NULL}: Value of gamma. If NULL, it is estimated.
#'  - \code{phi=NULL}: Value of phi. If NULL, it is estimated.
#'  - \code{lambda=NULL}: Box-Cox transformation parameter. Ignored if
#'    \code{NULL}. When lambda is specified, \code{additive.only} is set to
#'    \code{TRUE}.
#'  - \code{additive.only=FALSE}: If \code{TRUE}, will only consider
#'     additive models.
#'  - \code{biasadj=FALSE}: Use adjusted back-transformed mean for Box-Cox
#'     transformations.
#'  - \code{lower=c(rep(1e-04, 3), 0.8)}: Lower bounds for the parameters
#'     (alpha, beta, gamma, phi).
#'  - \code{upper=c(rep(0.9999,3), 0.98)}: Upper bounds for the parameters
#'     (alpha, beta, gamma, phi)
#'  - \code{opt.crit="lik"}: Optimization criterion.
#'  - \code{nmse=3}: Number of steps for average multistep MSE (1 <= nmse
#'     <= 30).
#'  - \code{bounds="both"}" Type of parameter space to impose: "usual"
#'     indicates all parameters must lie between specified lower and upper
#'     bounds; "admissible" indicates parameters must lie in the admissible
#'     space; "both" (default) takes the intersection of these regions.
#'  - \code{ic="aic"}: Information criterion to be used in model
#'     selection.
#'  - \code{restrict=TRUE}: If TRUE, models with infinite variance will not
#'     be allowed.
#'  - \code{allow.multiplicative.trend=FALSE}: If TRUE, models with
#'     multiplicative trend are allowed when searching for a model.
#'  - \code{use.initial.values=FALSE}: If \code{TRUE} and model is of class
#'     "ets", then the initial values in the model are also not re-estimated.
#'  - \code{n.ahead}: The forecast horizon. If not specified, returns
#'     forecast of size \code{task$X}.
#'  - \code{freq=1}: the number of observations per unit of time.
#'  - \code{...}: Other parameters passed to \code{\link[forecast]{ets}.}
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
#' expSmooth_lrnr <- make_learner(Lrnr_expSmooth)
#'
#' train_task <- training(task, fold = task$folds[[1]])
#' valid_task <- validation(task, fold = task$folds[[1]])
#'
#' expSmooth_fit <- expSmooth_lrnr$train(train_task)
#' expSmooth_preds <- expSmooth_fit$predict(valid_task)
Lrnr_expSmooth <- R6Class(
  classname = "Lrnr_expSmooth",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(model = "ZZZ", damped = NULL, alpha = NULL,
                          beta = NULL, gamma = NULL, phi = NULL, lambda = NULL,
                          additive.only = FALSE, biasadj = FALSE,
                          lower = c(rep(1e-04, 3), 0.8),
                          upper = c(rep(0.9999, 3), 0.98),
                          opt.crit = "lik", nmse = 3, bounds = "both",
                          ic = "aic", restrict = TRUE,
                          allow.multiplicative.trend = FALSE,
                          use.initial.values = FALSE, freq = 1, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c("timeseries", "continuous"),

    .train = function(task) {
      args <- self$params
      args$y <- ts(task$Y, frequency = args$freq)
      fit_object <- call_with_args(forecast::ets, args)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      h <- ts_get_pred_horizon(self$training_task, task)
      raw_preds <- forecast::forecast(private$.fit_object, h = h)
      preds <- as.numeric(raw_preds$mean)
      requested_preds <- ts_get_requested_preds(self$training_task, task, preds)
      return(requested_preds)
    },
    .required_packages = c("forecast")
  )
)
