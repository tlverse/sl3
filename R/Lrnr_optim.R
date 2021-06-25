#' Optimize Metalearner according to Loss Function using optim
#'
#' This meta-learner provides fitting procedures for any pairing of loss
#' function and metalearner function, subject to constraints. The
#' optimization problem is solved by making use of \code{optim}, For further
#' details, consult the documentation of \code{\link{optim}}.
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
#'   \item{\code{learner_function=metalearner_linear}}{A function(alpha, X) that
#'     takes a vector of covariates and a matrix of data and combines them into
#'     a vector of predictions. See \link{metalearners} for options.}
#'   \item{\code{loss_function=loss_squared_error}}{A function(pred, truth) that
#'     takes prediction and truth vectors and returns a loss vector. See
#'     \link{loss_functions} for options.}
#'   \item{\code{intercept=FALSE}}{If true, X includes an intercept term.}
#'   \item{\code{init_0=FALSE}}{If true, alpha is initialized to all 0's, useful
#'     for TMLE. Otherwise, it is initialized to equal weights summing to 1,
#'     useful for Super Learner.}
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#
Lrnr_optim <- R6Class(
  classname = "Lrnr_optim",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner_function = metalearner_linear,
                          loss_function = loss_squared_error,
                          intercept = FALSE, init_0 = FALSE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights",
      "offset"
    ),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      learner_function <- params$learner_function
      loss_function <- params$loss_function

      if (params$intercept) {
        X <- as.matrix(task$X_intercept)
      } else {
        X <- as.matrix(task$X)
      }
      Y <- task$Y

      if (task$has_node("offset")) {
        offset <- task$offset
      } else {
        offset <- NULL
      }
      weights <- task$weights
      risk <- function(alphas) {
        if (!is.null(offset)) {
          preds <- learner_function(alphas, X, offset)
        } else {
          preds <- learner_function(alphas, X)
        }
        eval_result <- loss_function(preds, Y)
        if (!is.null(attr(eval_result, "risk"))) {
          risk <- eval_result
        } else {
          losses <- eval_result
          risk <- weighted.mean(losses, weights)
        }
        return(risk)
      }
      p <- ncol(X)

      if (params$init_0) {
        init_alphas <- rep(0, p)
      } else {
        init_alphas <- rep(1 / p, p)
      }
      fit_object <- optim(init_alphas, risk, method = "BFGS")
      coefs <- fit_object$par
      names(coefs) <- colnames(task$X)

      fit_object$coefficients <- coefs
      fit_object$training_offset <- task$has_node("offset")
      fit_object$name <- "optim"
      return(fit_object)
    },
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")

      if (self$params$intercept) {
        X <- as.matrix(task$X_intercept)
      } else {
        X <- as.matrix(task$X)
      }
      alphas <- self$fit_object$coefficients

      if (self$fit_object$training_offset) {
        predictions <- self$params$learner_function(alphas, X, task$offset)
      } else {
        predictions <- self$params$learner_function(alphas, X)
      }

      if (ncol(predictions) == 1) {
        predictions <- as.vector(predictions)
      }
      return(predictions)
    }
  )
)
