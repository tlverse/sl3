#' Learner that chains into a revere task
#'
#' A wrapper around a revere generator that produces a revere task on chain
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom origami training validation fold_index cross_validate
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
#'   \item{\code{revere_function}}{The revere generator function to wrap}
#' }
#
Lrnr_revere_task <- R6Class(
  classname = "Lrnr_revere_task",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(revere_function, ...) {
      params <- list(revere_function = revere_function, ...)
      super$initialize(params = params, ...)
    },
    predict_fold = function(task, fold_number = "validation") {
      stop("this learner is meant for chaining only")
    },
    chain_fold = function(task, fold_number = "validation") {
      if (task$uuid == self$training_task$uuid) {
        revere_task <- self$fit_object$revere_task
      } else {
        revere_task <- sl3_revere_Task$new(self$params$revere_function, task)
      }

      return(revere_task$revere_fold_task(fold_number))
    }
  ),
  active = list(
    name = function() {
      name <- paste("CV", self$params$learner$name, sep = "_")
    }
  ),
  private = list(
    .properties = c("wrapper", "cv"),
    .train = function(task, trained_sublearners) {
      fit_object <- list(
        revere_task = sl3_revere_Task$new(self$params$revere_function, task)
      )
      return(fit_object)
    },
    .predict = function(task) {
      stop("this learner is meant for chaining only")
      return(predictions)
    },
    .chain = function(task) {
      if (task$uuid == self$training_task$uuid) {
        return(self$fit_object$revere_task)
      } else {
        return(sl3_revere_Task$new(self$params$revere_function, task))
      }
    },
    .required_packages = c("origami")
  )
)
