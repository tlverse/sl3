#' Nonlinear Time Series Analysis
#'
#' This learner supports various forms of nonlinear autoregression, including
#' additive AR, neural nets, SETAR and LSTAR models, threshold VAR and VECM.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#'   \item{\code{learner}}{Available built-in time series models. Currently
#'     available can be listed with availableModels() function.}
#'   \item{\code{m = 1}}{embedding dimension.}
#'   \item{\code{...}}{Additional learner-specific arguments.}
#' }
#
Lrnr_tsDyn <- R6Class(
  classname = "Lrnr_tsDyn",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner, m = 1, ...) {
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),

  private = list(
    .properties = c("timeseries", "continuous"),

    .train = function(task) {
      args <- self$params
      learner <- args$learner
      learner_fun <- get(
        learner,
        mode = "function",
        envir = asNamespace("tsDyn")
      )

      if (length(task$X) > 0) {
        # TODO: add option for external regressors
        args$data <- args$x <- as.matrix(task$Y)
      } else {
        args$data <- args$x <- as.matrix(task$Y)
      }

      fit_object <- call_with_args(learner_fun, args)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      params <- self$params
      h <- ts_get_pred_horizon(self$training_task, task)
      learner <- params[["learner"]]


      if (learner == "TVAR") {
        stop("No forecast for multivariate Threshold VAR model implemented.")
      }

      predictions <- predict(private$.fit_object, n.ahead = h)
      predictions <- as.numeric(predictions)

      requested_preds <- ts_get_requested_preds(self$training_task, task, predictions)

      return(requested_preds)
    },
    .required_packages = c("tsDyn", "mgcv")
  )
)
