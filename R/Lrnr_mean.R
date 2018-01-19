#' Fitting Intercept Models
#'
#' This learner provides fitting procedures for intercept models. Such models
#' predict the outcome variable simply as the mean of the outcome vector.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
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
#'   \item{\code{...}}{Not used.}
#' }
#'
#' @template common_parameters
#
Lrnr_mean <- R6Class(
  classname = "Lrnr_mean", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(params = params, ...)
    },

    print = function() {
      print(self$name)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights", "offset"),

    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)
      y <- outcome_type$format(task$Y)
      weights <- task$weights

      if (task$has_node("offset")) {
        offset <- task$offset
        if (outcome_type$type == "categorical") {
          # todo: fix 
          stop("offsets not yet supported for outcome_type='categorical'")
        }
      } else {
        offset <- rep(0, task$nrow)
      }
      
      if (outcome_type$type == "categorical") {
        y_levels <- outcome_type$levels
        means <- sapply(
          y_levels,
          function(level) weighted.mean(y == level, weights)
        )
        fit_object <- list(mean = pack_predictions(matrix(means, nrow = 1)))
      } else {
        fit_object <- list(mean = weighted.mean(y - offset, weights))
      }
      
      fit_object$training_offset <- task$has_node("offset")
      
      return(fit_object)
    },

    .predict = function(task = NULL) {
      predictions <- rep(private$.fit_object$mean, task$nrow)
      
      if (self$fit_object$training_offset) {
        offset <- task$offset_transformed(NULL, for_prediction = TRUE)
        predictions <- predictions + offset
      } 
      return(predictions)
    }
  )
)
