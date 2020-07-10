#' Cross-Validated Selector
#'
#' This meta-learner identifies the cross-validated selector (i.e. discrete
#' super learner) for any loss function.
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
#'   \item{\code{loss_function=loss_squared_error}}{A function(pred, truth)
#'     that takes prediction and truth vectors and returns a loss vector. See
#'     \link{loss_functions} for options.}
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#
Lrnr_cv_selector <- R6Class(
  classname = "Lrnr_cv_selector",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(loss_function = loss_squared_error,
                              ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights"
    ),

    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      loss_function <- params$loss_function
      outcome_type <- self$get_outcome_type(task)

      # specify data
      X <- as.matrix(task$X)
      Y <- outcome_type$format(task$Y)
      weights <- task$weights

      risk <- function(preds) {
        loss <- loss_function(preds, Y)
        risk <- weighted.mean(loss, weights)
        return(risk)
      }
      risks <- apply(X, 2, risk)

      fit_object <- list()
      fit_object$name <- colnames(task$X)[which.min(risks)]
      coef <- risks
      coef[which.min(risks)] <- 1
      coef[-(which.min(risks))] <- 0
      fit_object$coefficients <- as.numeric(coef)

      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- as.matrix(task$X)
      predictions <- X[, self$fit_object$name]
      return(predictions)
    }
  )
)
