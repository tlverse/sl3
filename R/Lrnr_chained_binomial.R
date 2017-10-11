#' Classification from Binomial Regression
#'
#' This learner provides converts a binomial learner into a multinomial learner using independent binomials. 
#' The procedure is modelled on \link{https://en.wikipedia.org/wiki/Multinomial_logistic_regression#As_a_set_of_independent_binary_regressions}
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field ... Additional arguments. Currently unused.
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#
Lrnr_chained_binomial <- R6Class(classname = "Lrnr_chained_binomial",
                             inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params = list(...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("categorical"),
    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)
      X <- as.matrix(task$X)
      Y <- task$format_Y(outcome_type)
      weights <- task$weights
      
      family<-switch(outcome_type, 
                     continuous="gaussian",
                     binomial="binomial",
                     categorical="multinomial"
                     )
      
      fit_object <- glmnet::cv.glmnet(x = X, y = Y, weights = weights, 
                                 lambda = NULL, type.measure = "deviance", nfolds = 10, 
                                 family = family, alpha = 1, nlambda = 100)
      
      return(fit_object)
    },
    .predict = function(task) {
      
      outcome_type <- private$.training_outcome_type
      predictions = stats::predict(private$.fit_object, newx = as.matrix(task$X), type="response", s = "lambda.min")

      if(outcome_type == "categorical"){
        # predictions is a 3-dim matrix, convert to 2-dim matrix
        dim(predictions) <- dim(predictions)[1:2]
        
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("randomForest")
  )
)

