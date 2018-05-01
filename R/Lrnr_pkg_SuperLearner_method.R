#' \code{Lrnr_pkg_SuperLearner_method} -- Interface for \code{SuperLearner}
#' combination methods.
#'
#' Use \code{SuperLearner::listWrappers("method")} for a list of options.
#'
#' @rdname SuperLearner_interface
#'
#' @export
#
Lrnr_pkg_SuperLearner_method <- R6Class(
  classname =
    "Lrnr_pkg_SuperLearner_method",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(SL_wrapper, ...) {
      params <- list(SL_wrapper = SL_wrapper, ...)
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("binomial", "continuous", "weights"),

    .train = function(task) {
      method <- self$params$SL_wrapper
      X <- as.matrix(task$X)
      Y <- task$Y

      fit_object <- method$computeCoef(
        X, Y, names(X),
        verbose = FALSE,
        obsWeights = task$weights
      )
      return(fit_object)
    },

    .predict = function(task) {
      coef <- private$.fit_object$coef
      X <- as.matrix(task$X)
      method <- self$params$SL_wrapper
      predictions <- method$computePred(X, coef)
      return(predictions)
    },
    .required_packages = c("SuperLearner")
  )
)

# sl3_learner_registry$register_learner(Lrnr_pkg_SuperLearner)
