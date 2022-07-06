#' Quantile Regression Forests
#'
#' This learner implements Quantile Regression Forests algorithm, using the \code{quantregForest} package.
#' Quantile Regression Forests infer conditional qunatile functions from the data.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'   \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{x}}{A matrix containing the predictor variables.}
#'   \item{\code{y}}{The response variable.}
#'   \item{\code{nthreads}}{The number of threads to use for parallel computation.}
#'   \item{\code{keep.inbag}}{Keep information which observations are in and out-of-bag?
#'   For out-of-bag predictions, this argument needs to be set to TRUE.}
#' }
#'
#' @template common_parameters
#'
#' @examples
#' 
#' #Get the data and make a sl3 task:
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed[1:100,], covariates = covs, outcome = "haz")
#' 
#' #Initialize, train and predict:
#' lrnr_qf <- Lrnr_quantregForest$new()
#' lrnr_qf_fit <- lrnr_qf$train(task)
#' preds <- lrnr_qf_fit$predict(task)
#' 

Lrnr_quantregForest <- R6Class(
  classname = "Lrnr_quantregForest",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(nthreads=1, keep.inbag=TRUE,
                          alpha=c(0.05,0.95),
                          ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("continuous", "quantile"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      
      # specify data
      args$x <- as.data.frame(task$X)
      args$y <- outcome_type$format(task$Y)
      
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }
      
      if (task$has_node("offset")) {
        args$offset <- task$offset
      }
      
      fit_object <- call_with_args(
        quantregForest::quantregForest, args
      )
      
      return(fit_object)
    },
    .predict = function(task) {
      outcome_type <- private$.training_outcome_type
      alpha <- self$params$alpha
      
      predictions <- predict(
        private$.fit_object,
        data = task$X,
        what=alpha
      )
      return(predictions)
    },
    .required_packages = c("quantregForest")
  )
)
