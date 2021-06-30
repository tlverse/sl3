#' Nonlinear Optimization via Augmented Lagrange
#'
#' This meta-learner provides fitting procedures for any pairing of loss or risk
#' function and metalearner function, subject to constraints. The optimization
#' problem is solved by making use of \code{\link[Rsolnp]{solnp}}, using
#' Lagrange multipliers. For further details, consult the documentation of the
#' \code{Rsolnp} package.
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
#'   \item{\code{learner_function=metalearner_linear}}{A function(alpha, X) that
#'     takes a vector of covariates and a matrix of data and combines them into
#'     a vector of predictions. See \link{metalearners} for options.}
#'   \item{\code{eval_function=loss_squared_error}}{A function(pred, truth) that
#'     takes prediction and truth vectors and returns a loss vector or a risk
#'     scalar. See \link{loss_functions} and \link{risk_functions} for options
#'     and more detail.}
#'   \item{\code{make_sparse=TRUE}}{If TRUE, zeros out small alpha values.}
#'   \item{\code{convex_combination=TRUE}}{If \code{TRUE}, constrain alpha to
#'     sum to 1.}
#'   \item{\code{init_0=FALSE}}{If TRUE, alpha is initialized to all 0's, useful
#'     for TMLE. Otherwise, it is initialized to equal weights summing to 1,
#'     useful for Super Learner.}
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#
Lrnr_solnp <- R6Class(
  classname = "Lrnr_solnp",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner_function = metalearner_linear,
                          eval_function = loss_squared_error,
                          make_sparse = TRUE, convex_combination = TRUE,
                          init_0 = FALSE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights", "offset"
    ),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      learner_function <- params$learner_function
      eval_function <- params$eval_function
      outcome_type <- self$get_outcome_type(task)

      # specify data
      X <- as.matrix(task$X)
      Y <- outcome_type$format(task$Y)

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
        eval_result <- eval_function(preds, Y)

        if (!is.null(attr(eval_result, "risk"))) {
          risk <- eval_result
        } else {
          loss <- eval_result
          risk <- weighted.mean(loss, weights)
        }
        return(risk)
      }
      if (params$convex_combination) {
        eq_fun <- function(alphas) {
          sum(alphas)
        }
        eqB <- 1
        LB <- rep(0L, ncol(task$X))
      } else {
        eq_fun <- NULL
        eqB <- NULL
        LB <- NULL
      }
      p <- ncol(X)

      if (params$init_0) {
        init_alphas <- rep(0, p)
      } else {
        init_alphas <- rep(1 / p, p)
      }
      fit_object <- Rsolnp::solnp(
        init_alphas, risk,
        eqfun = eq_fun, eqB = eqB,
        LB = LB,
        control = list(trace = 0)
      )
      coefs <- fit_object$pars
      names(coefs) <- colnames(task$X)

      if (params$make_sparse) {
        max_coef <- max(coefs)
        threshold <- max_coef / 1000
        coefs[coefs < threshold] <- 0
        if (params$convex_combination) {
          # renormalize so coefficients sum to 1
          coefs <- coefs / sum(coefs)
        }
      }
      fit_object$coefficients <- coefs
      fit_object$training_offset <- task$has_node("offset")
      fit_object$name <- "solnp"
      return(fit_object)
    },
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- as.matrix(task$X)
      alphas <- self$fit_object$coefficients

      if (self$fit_object$training_offset) {
        predictions <- self$params$learner_function(alphas, X, task$offset)
      } else {
        predictions <- self$params$learner_function(alphas, X)
      }
      return(predictions)
    },
    .required_packages = c("Rsolnp")
  )
)
