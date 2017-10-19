#' SuperLearner Wrapper Interface
#'
#' This learner provides an interface to the wrapper functions provided by the
#' \code{SuperLearner} package. Such wrapper functions are used to draw on
#' learning algorithms provided by a wide variety of R packages, towards the
#' goal of fitting a \code{SuperLearner} ensemble model. Consult documentation
#' of the \code{SuperLearner} package for details on these wrapper functions.
#'
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field SL_wrapper A \code{SuperLearner} wrapper, used as an interface to
#'  algorithms to be fit in a \code{SuperLearner} ensemble model.
#' @field ... Additional arguments. Currently unused.
#'
#' @importFrom R6 R6Class
#' @importFrom stats gaussian predict
#'
#' @export
#
Lrnr_pkg_SuperLearner <- R6Class(classname = "Lrnr_pkg_SuperLearner",
                                 inherit = Lrnr_base, portable = TRUE,
                                 class = TRUE,
  public = list(
    initialize = function(SL_wrapper, ...) {
      wrapper_fun = get(SL_wrapper)
      params = list(wrapper_name = SL_wrapper, wrapper_fun = wrapper_fun, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("binomial", "continuous", "weights", "ids"),
    .train = function(task) {
      args <- self$params
      wrapper = args$wrapper_fun
      # to minimize prediction costs (since we throw out predictions from here
      # anyways), newX is just a single row
      newX = task$X[1, ]
      
      
      
      outcome_type <- self$get_outcome_type(task)
      args$family <- get_glm_family(args$family, outcome_type)

      fit_object <- wrapper(task$Y, task$X, newX, family = args$family,
                            obsWeights = task$weights, id = task$id)$fit
      return(fit_object)
    },
    .predict = function(task) {
      args <- self$params
      outcome_type <- private$.training_outcome_type
      args$family <- get_glm_family(args$family, outcome_type)
      
      predictions = stats::predict(private$.fit_object, newdata = task$X,
                                   family = args$family)
      return(predictions)
    },
    .required_packages = c("SuperLearner")
  )
)

