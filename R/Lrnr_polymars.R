#' Polyspline - multivariate adaptive polynomial spline regression (polymars)
#'   and polychotomous regression and multiple classification (polyclass)
#'
#' This learner provides fitting procedures for an adaptive regression procedure
#' using piecewise linear splines to model the response, using the function
#' \code{\link[polspline]{polymars}} or \code{\link[polspline]{polyclass}} from
#' the \code{polspline} package as appropriate.
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
#'   \item{\code{cv}}{The number of cross-validation folds to use in evaluating
#'     the sequence of fit models. Only passed to
#'     \code{\link[polspline]{polyclass}}.
#'   }
#'   \item{\code{...}}{Other parameters passed to either
#'     \code{\link[polspline]{polymars}} or \code{\link[polspline]{polyclass}}.
#'     See their documentation for details.
#'   }
#' }
#
Lrnr_polspline <- R6Class(
  classname = "Lrnr_polspline", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(cv = 2, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical"),

    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      args$X <- task$X
      args$Y <- outcome_type$format(task$Y)

      if (outcome_type$type == "continuous") {
        ...
      } else if (outcome_type$type %in% c("binomial", "categorical")) {
        ...
      } else {
        stop("Lrnr_polspline does not support the designated outcome type.")
      }

      fit_object <- call_with_args(ranger::ranger, args)
      return(fit_object)
    },

    .predict = function(task) {
      predictions <- stats::predict(
        private$.fit_object,
        data = task$X,
        type = "response",
        num.threads = self$params$num.threads
      )
      # extract numeric predictions from custom class ranger.prediction
      preds <- predictions[[1]]
      return(preds)
    },
    .required_packages = c("ranger")
  )
)

