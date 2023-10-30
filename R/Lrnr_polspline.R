#' Polyspline - multivariate adaptive polynomial spline regression (polymars)
#'   and polychotomous regression and multiple classification (polyclass)
#'
#' This learner provides fitting procedures for an adaptive regression procedure
#' using piecewise linear splines to model the response, using the the
#' \pkg{polspline} package' functions \code{\link[polspline]{polymars}} (for
#' continuous outcome prediction) or \code{\link[polspline]{polyclass}} (for
#' binary or categorical outcome prediction).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict family
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
#'    - \code{...}: Other parameters passed to
#'        \code{\link[polspline]{polymars}}, \code{\link[polspline]{polyclass}},
#'        or additional arguments defined in \code{\link{Lrnr_base}} (such as
#'        \code{params} like \code{formula}). See their documentation for
#'        details.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#' polspline_lrnr <- Lrnr_caret$new(method = "rf")
#' set.seed(693)
#' polspline_lrnr_fit <- polspline_lrnr$train(task)
#' polspline_lrnr_predictions <- polspline_lrnr_fit$predict()
Lrnr_polspline <- R6Class(
  classname = "Lrnr_polspline", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
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
        args$verbose <- getOption("sl3.verbose")
        fit_object <- call_with_args(polspline::polymars, args)
      } else if (outcome_type$type %in% c("binomial", "categorical")) {
        if (task$has_node("weights")) {
          args$weight <- task$weights
        }
        args$cov <- task$X
        args$data <- outcome_type$format(task$Y)
        args$silent <- getOption("sl3.verbose")
        fit_object <- call_with_args(polspline::polyclass, args)
      } else {
        stop("Lrnr_polspline does not support the designated outcome type.")
      }
      return(fit_object)
    },
    .predict = function(task) {
      if (private$.training_outcome_type$type == "continuous") {
        predictions <- stats::predict(
          object = private$.fit_object, x = task$X
        )
      } else if (private$.training_outcome_type$type == "binomial") {
        predictions <- polspline::ppolyclass(
          fit = private$.fit_object, cov = task$X
        )[, 2]
      } else if (private$.training_outcome_type$type == "categorical") {
        predictions <- pack_predictions(polspline::ppolyclass(
          fit = private$.fit_object, cov = task$X
        ))
      }
      return(predictions)
    },
    .required_packages = c("polspline")
  )
)
