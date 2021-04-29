#' Ranger - A Fast Implementation of Random Forests
#'
#' This learner provides fitting procedures for a fast implementation of Random
#' Forests, particularly suited for high dimensional data, using the
#' \code{ranger} package, using the function \code{\link[ranger]{ranger}}.
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
#'   \item{\code{num.trees = 500}}{Number of trees to be used in growing the
#'     forest.}
#'   \item{\code{write.forest = TRUE}}{If \code{TRUE}, forest is stored, which
#'     is required for prediction. Set to \code{FALSE} to reduce memory usage if
#'     no prediction is intended.}
#'   \item{\code{num.threads = 1}}{Number of threads.}
#'   \item{\code{...}}{Other parameters passed to \code{\link[ranger]{ranger}}.
#'     See its documentation for details.}
#' }
#
Lrnr_ranger <- R6Class(
  classname = "Lrnr_ranger", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(num.trees = 500,
                          write.forest = TRUE,
                          num.threads = 1,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    importance = function(...) {
      if (self$fit_object$importance.mode == "none") {
        stop(
          "This learner was instantiated with default argument, ",
          "importance=none. Modify this argument to measure importance."
        )
      }
      self$assert_trained()

      # initiate argument list for ranger::importance
      args <- list(...)
      args$x <- self$fit_object

      # calculate importance metrics
      importance_result <- call_with_args(ranger::importance, args, keep_all = T)

      # sort according to decreasing importance
      return(importance_result[order(importance_result, decreasing = TRUE)])
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical", "importance", "weights"),

    .train = function(task) {
      args <- self$params

      # format data
      if (task$outcome_type$type == "binomial") {
        args$data <- cbind(factor(task$Y), task$X)
      } else if (task == "categorical") {
        args$data <- cbind(task$outcome_type$format(task$Y), task$X)
      } else {
        args$data <- cbind(task$Y, task$X)
      }
      colnames(args$data)[1] <- task$nodes$outcome
      args$dependent.variable.name <- task$nodes$outcome

      # specify tree type
      if (is.null(args$classification) & task$outcome_type$type == "binomial") {
        args$classification <- TRUE
      }
      args$probability <- task$outcome_type$type == "categorical"

      if (task$has_node("weights")) {
        args$case.weights <- task$weights
      }

      if (is.null(args$verbose)) {
        args$verbose <- getOption("sl3.verbose")
      }

      fit_object <- call_with_args(ranger::ranger, args)
      return(fit_object)
    },

    .predict = function(task) {

      # extract numeric predictions from custom class ranger.prediction
      predictions <- stats::predict(
        private$.fit_object,
        data = task$X,
        type = "response",
        num.threads = self$params$num.threads
      )

      predictions <- predictions[[1]]

      if (task$outcome_type$type == "categorical") {
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("ranger")
  )
)
