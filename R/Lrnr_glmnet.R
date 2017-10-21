#' Random Forest Models
#'
#' This learner provides fitting procedures for elastic net models, using the
#' \code{glmnet} package. For details on the fitting procedure, consult
#' the documentation of the \code{glmnet} package.
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
#' @field ... Additional arguments. Currently unused.
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#
Lrnr_glmnet <- R6Class(classname = "Lrnr_glmnet",
                             inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(lambda = NULL, type.measure = "deviance", nfolds = 10,
                                 alpha = 1, nlambda = 100, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      
      if(is.null(args$family)){
        args$family <- outcome_type$glm_family()
      }
      
      if (args$family %in% "quasibinomial") {
        args$family <- "gaussian"
        warning("Lrnr_glmnet doesn't understand outcome_type='quasibinomial'; fitting glmnet with family='gaussian' instead.")
      }

      # specify data
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)

      if(task$has_node("weights")){
        args$weights <- task$weights
      }

      if(task$has_node("offset")){
        args$offset <- task$offset
      }

      fit_object <- call_with_args(glmnet::cv.glmnet, args, names(formals(glmnet::glmnet)))
      fit_object$glmnet.fit$call <- NULL

      return(fit_object)
    },
    .predict = function(task) {
      outcome_type <- private$.training_outcome_type
      predictions <- stats::predict(private$.fit_object, newx = as.matrix(task$X), type="response", s = "lambda.min")

      if(outcome_type$type == "categorical"){
        # predictions is a 3-dim matrix, convert to 2-dim matrix
        dim(predictions) <- dim(predictions)[1:2]

        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("glmnet")
  )
)

