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
    .properties = c("continuous", "binomial", "categorical", "weights","offset"),
    
    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)
      y <- outcome_type$format(task$Y)
      weights <- task$weights
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      
      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }
      
      if (outcome_type$type == "categorical") {
        y_levels <- outcome_type$levels
        means <- sapply(
          y_levels,
          function(level) weighted.mean(y == level, weights)
        )
        fit_object <- list(mean = pack_predictions(matrix(means, nrow = 1)))
      } else {
        if (task$has_node("offset")) {
          fit_object = glm(Y~1, data = data, offset = task$offset, family = args$family, 
                           weights = weights)
        } else {
          fit_object = glm(Y~1, data = data, family = args$family, weights = weights)
        }
      }
      fit_object$linkinv_fun = args$family$linkinv
      return(fit_object)
    },
    
    .predict = function(task = NULL) {
      if (outcome_type$type == "categorical") {
        predictions <- rep(private$.fit_object$mean, task$nrow)
      } else {
        if (task$has_node("offset")) {
          data = as.data.frame(task$X)
          predictions <- plogis(stats::predict(fit, data = data) + task$offset)
        } else {
          predictions <- plogis(stats::predict(fit, data = data))
        }
      }
    }
    return(predictions)
  )
)
