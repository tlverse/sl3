#' Intercept Model Fits
#'
#' This learner provides fitting procedures for intercept models. Such models
#' predict the outcome variable simply as the mean of the outcome vector.
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
#' @field ... Additional arguments.
#'
#' @importFrom R6 R6Class
#'
#' @export
#
Lrnr_mean <- R6Class(classname = "Lrnr_mean", inherit = Lrnr_base,
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
    .properties = c("continuous", "binomial", "categorical", "weights"),
    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)
      y <- task$format_Y(outcome_type)
      weights <- task$weights
      
      if(outcome_type == "categorical"){
        y_levels <- levels(y)
        means <- sapply(y_levels, function(level)weighted.mean(y==level, weights))
        fit_object <- list(mean = pack_predictions(matrix(means, nrow=1)))
        
      } else {
        fit_object <- list(mean = weighted.mean(y, weights))  
      }
      
      return(fit_object)
    },
    .predict = function(task = NULL) {
      predictions <- rep(private$.fit_object$mean, task$nrow)
      return(predictions)
    }
  ),
)

