#' GLM Fits
#'
#' This learner provides fitting procedures for generalized linear models by way
#' of a wrapper relying on \code{stats::glm}.
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
#' @field family A \code{family} object from package \code{stats} describing the
#'  error family of the model to be fit. See the documentation for the package
#'  \code{stats} for details, or consult \code{stats::family} directly.
#' @field ... Additional arguments.
#'
#' @importFrom R6 R6Class
#' @importFrom stats glm predict family
#'
#' @export
#
Lrnr_glm <- R6Class(classname = "Lrnr_glm", inherit = Lrnr_base,
                    portable = TRUE, class = TRUE,
  public = list(
    initialize = function(family = gaussian(), ...) {
      params <- list(family = family, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .train = function(task) {
      params <- self$params
      family <- params[["family"]]
      if (is.character(family)) {
        family <- get(family, mode = "function", envir = parent.frame())
        family <- stats::family()
      }
      # TODO: if possible have this use task$Xmat with glm.fit or speedglm
      Y <- task$Y
      fit_object <- stats::glm(Y ~ ., data = task$X, family = family,
                               weights = task$weights)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      predictions <- stats::predict(private$.fit_object, newdata = task$X,
                                    type = "response")
      return(predictions)
    }
  ),
)

