#' Methods for SuperLearner Objects
#'
#' Support for \code{SuperLearner} wrappers, obviously not the most efficient
#' approach, we should reimplement as many as possible.
#'
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#
Lrnr_pkg_SuperLearner_method <- R6Class(classname = "Lrnr_pkg_SuperLearner_method",
                                        inherit = Lrnr_base, portable = TRUE,
                                        class = TRUE,
  public = list(
    initialize = function(method, ...) {
      params = list(method = method, ...)
      super$initialize(params=params, ...)
    }
  ),
  private = list(
    .train = function(task) {
      method <- self$params$method
      X <- as.matrix(task$X)
      Y <- task$Y

      fit_object <- method$computeCoef(X, Y, names(X), verbose = FALSE,
                                       obsWeights = task$weights)
      return(fit_object)
    },
    .predict = function(task) {
      coef <- private$.fit_object$coef
      X <- as.matrix(task$X)
      method <- self$params$method
      predictions = method$computePred(X, coef)
      return(predictions)
    },
    .required_packages = c("SuperLearner")
  )
)

# sl3_learner_registry$register_learner(Lrnr_pkg_SuperLearner)
