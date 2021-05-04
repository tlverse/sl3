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
#' @importFrom stats predict
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
#'   \item{\code{cv}}{The number of cross-validation folds to be used in
#'     evaluating the sequence of fit models. This is only passed to
#'     \code{\link[polspline]{polyclass}} for binary/categorical outcomes.
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
    initialize = function(cv = 5, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      if (outcome_type$type == "continuous") {
        if (task$has_node("weights")) {
          args$weights <- task$weights
        }
        args$predictors <- task$X
        args$responses <- outcome_type$format(task$Y)
        fit_object <- call_with_args(polspline::polymars, args)
      } else if (outcome_type$type %in% c("binomial", "categorical")) {
        if (task$has_node("weights")) {
          args$weight <- task$weights
        }
        args$cov <- task$X
        args$data <- outcome_type$format(task$Y)
        fit_object <- call_with_args(polspline::polyclass, args)
      } else {
        stop("Lrnr_polspline does not support the designated outcome type.")
      }
      return(fit_object)
    },
    .predict = function(task) {
      outcome_type <- self$get_outcome_type(task)
      if (outcome_type$type == "continuous") {
        preds <- stats::predict(object = private$.fit_object, x = task$X)
      } else if (outcome_type$type %in% c("binomial", "categorical")) {
        preds <- polspline::ppolyclass(
          fit = private$.fit_object,
          cov = task$X
        )[, 2]
      }
      return(preds)
    },
    .required_packages = c("polspline")
  )
)
