#' Bound Predictions
#'
#' This learner bounds predictions. Intended for use in a pipeline.
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
#'   \item{\code{bound = .005}}{Either a length two vector of c(lower,upper) or a 
#'   lower bound, where the upper is then 1 - lower}
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#
Lrnr_bound <- R6Class(
  classname = "Lrnr_bound",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(bound = 0.005,
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
      fit_object <- list()
      return(fit_object)
    },
    
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- as.matrix(task$X)
      bounds <- self$params$bound
      
      bound <- function(x, bounds) {
        lower <- bounds[[1]]
        if (length(bounds) > 1) {
          upper <- bounds[[2]]
        } else {
          upper <- 1 - lower
        }
        pmin(pmax(x, lower), upper)
      }
      
      predictions <- bound(X, bounds)
      return(predictions)
    }
  )
)