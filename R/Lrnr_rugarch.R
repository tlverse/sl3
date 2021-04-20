#' Univariate GARCH Models
#'
#' @description This learner supports autoregressive fractionally integrated moving
#' average  and various flavors of generalized autoregressive conditional
#' heteroskedasticity models for univariate time-series. All the models
#' are fit using \code{\link[rugarch]{ugarchfit}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#'  - \code{variance.model}: List containing variance model specification.
#'     This includes model, GARCH order, submodel, external regressors and
#'     variance tageting. Refer to \code{\link{ugarchspec}} for more information.
#'  - \code{mean.model}: List containing the mean model specification. This
#'     includes ARMA model, whether the mean should be included, and external
#'     regressors among others.
#'  - \code{distribution.model}:Conditional density to use for the
#'     innovations.
#'  - \code{start.pars}:List of staring parameters for the
#'     optimization routine.
#'  - \code{fixed.pars}:List of parameters which are to be kept
#'     fixed during the optimization.
#'  - \code{...}: Other parameters passed to \code{\link[rugarch]{ugarchfit}}.
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
#'
#' #
Lrnr_rugarch <- R6Class(
  classname = "Lrnr_rugarch", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(variance.model =
                            list(
                              model = "sGARCH", garchOrder = c(1, 1),
                              submodel = NULL, external.regressors = NULL,
                              variance.targeting = FALSE
                            ),
                          mean.model =
                            list(
                              armaOrder = c(1, 1), include.mean = TRUE,
                              archm = FALSE, archpow = 1, arfima = FALSE,
                              external.regressors = NULL, archex = FALSE
                            ),
                          distribution.model = "norm", start.pars = list(),
                          fixed.pars = list(), ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous"),
    .train = function(task) {
      args <- self$params
      # Support for a single time-series
      spec_object <- call_with_args(rugarch::ugarchspec, args)
      # Perhaps might not want to store all the info. TODO
      fit_object <- rugarch::ugarchfit(spec_object, task$X)
      return(fit_object)
    },

    # Only simple forecast, do not implement CV based forecast here
    .predict = function(task = NULL) {
      fit_object <- private$.fit_object
      h <- ts_get_pred_horizon(self$training_task, task)

      # Give the same output as GLM
      predictions <- rugarch::ugarchforecast(
        private$.fit_object,
        data = task$X,
        n.ahead = h
      )

      preds <- as.numeric(predictions@forecast$seriesFor)
      requested_preds <- ts_get_requested_preds(self$training_task, task, preds)

      return(requested_preds)
    },
    .required_packages = c("rugarch")
  )
)
