#' GLMs with Elastic Net Regularization
#'
#' This learner provides fitting procedures for elastic net models, using the
#' \code{glmnet} package, using \code{\link[glmnet]{cv.glmnet}} to select an
#' appropriate value of lambda.
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
#'   \item{\code{lambda=NULL}}{A vector of lambda values to compare}
#'   \item{\code{type.measure="deviance"}}{The loss to use when selecting
#'     lambda. Options documented in \code{\link[glmnet]{cv.glmnet}}.}
#'   \item{\code{nfolds=10}}{Number of folds to use for internal
#'     cross-validation.}
#'   \item{\code{alpha=1}}{The elastic net parameter. 0 is Ridge Regression, 1
#'     is Lasso. Intermediate values are a combination. Documented in
#'     \code{\link[glmnet]{glmnet}}.}
#'   \item{\code{nlambda=100}}{The number of lambda values to compare. Comparing
#'     less values will speed up computation, but may decrease statistical
#'     performance. Documented in \code{\link[glmnet]{cv.glmnet}}.}
#'   \item{\code{use_min=TRUE}}{If TRUE, use lambda=cv_fit$lambda.min for prediction,
#'     otherwise use lambda=cv_fit$lambda.1se.
#'     the distinction is clarified in \code{\link[glmnet]{cv.glmnet}}.}
#'   \item{\code{...}}{Other parameters to be passed to
#'     \code{\link[glmnet]{cv.glmnet}} and \code{\link[glmnet]{glmnet}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_glmnet <- R6Class(
  classname = "Lrnr_glmnet",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(lambda = NULL, type.measure = "deviance", nfolds = 10,
                          alpha = 1, nlambda = 100, use_min = TRUE, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),

    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family()
      }

      if (args$family %in% "quasibinomial") {
        args$family <- "gaussian"
        warning(paste(
          "Lrnr_glmnet doesn't understand outcome_type =",
          "'quasibinomial'; fitting glmnet with family='gaussian'",
          "instead."
        ))
      }

      # specify data
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(
        glmnet::cv.glmnet, args,
        names(formals(glmnet::glmnet)), ignore="use_min"
      )
      fit_object$glmnet.fit$call <- NULL
      return(fit_object)
    },

    .predict = function(task) {
      outcome_type <- private$.training_outcome_type
      if (self$params$use_min) {
        lambda <- "lambda.min"
      } else {
        lambda <- "lambda.1se"
      }
      predictions <- stats::predict(
        private$.fit_object,
        newx = as.matrix(task$X), type = "response",
        s = lambda
      )

      if (outcome_type$type == "categorical") {
        cat_names <- dimnames(predictions)[[2]]
        # predictions is a 3-dim matrix, convert to 2-dim matrix
        dim(predictions) <- dim(predictions)[1:2]
        colnames(predictions) <- cat_names
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("glmnet")
  )
)
