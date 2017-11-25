#' Use SuperLearner Wrappers, Screeners, and Methods, in sl3
#'
#' @description These learners provide an interface to the wrapper functions,
#'  screening algorithms, and combination methods provided by the
#'  \code{SuperLearner} package. These components add support for a range of
#'  algorithms not currently implemented natively in \code{sl3}.
#'
#' @description \code{Lrnr_pkg_SuperLearner} - Interface for \code{SuperLearner}
#'  wrapper functions. Use \code{SuperLearner::listWrappers("SL")} for a list.
#'
#' @docType class
#'
#' @rdname SuperLearner_interface
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
#'   \item{\code{SL_wrapper}}{The wrapper function to use.}
#'   \item{\code{...}}{Currently not used.}
#' }
#'
#' @template common_parameters
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

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      fit_object <- wrapper(task$Y, task$X, newX, family = args$family,
                            obsWeights = task$weights, id = task$id)$fit
      return(fit_object)
    },

    .predict = function(task) {
      args <- self$params
      outcome_type <- private$.training_outcome_type

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      predictions = stats::predict(private$.fit_object, newdata = task$X,
                                   family = args$family)
      return(predictions)
    },
    .required_packages = c("SuperLearner")
  )
)

