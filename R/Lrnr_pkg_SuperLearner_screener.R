#' SuperLearner Screener Interface
#'
#' This learner provides an interface to the screener functions provided by the
#' \code{SuperLearner} package. Such screening functions are used to reduce the
#' number of covariates in the process of fitting a \code{SuperLearner} ensemble
#' model. Consult documentation of the \code{SuperLearner} package for details
#' on these screener functions.
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
#' @importFrom stats gaussian
#'
#' @export
#
Lrnr_pkg_SuperLearner_screener <- R6Class(classname = "Lrnr_pkg_SuperLearner_screener",
                                          inherit = Lrnr_base, portable = TRUE,
                                          class = TRUE,
  public = list(
    initialize = function(SL_wrapper, ...) {
      if (SL_wrapper == "All") {
        wrapper_fun = NULL
      } else {
        wrapper_fun = get(SL_wrapper)
      }
      params = list(wrapper_name = SL_wrapper, wrapper_fun = wrapper_fun, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("binomial", "continuous", "weights", "ids"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      
            
      if(is.null(args$family)){
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }

      
      wrapper = args$wrapper_fun
      if (is.null(wrapper)) {
        selected <- task$nodes$covariates
      } else {
        selected <- wrapper(task$Y, task$X, family = args$family,
                            obsWeights = task$weights, id = task$id)
      }
      fit_object = list(selected = task$nodes$covariates[selected])
      return(fit_object)
    },
    .predict = function(task) {
      task$X[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    .required_packages = c("SuperLearner")
  )
)

