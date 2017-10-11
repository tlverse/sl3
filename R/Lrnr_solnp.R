#' Nonlinear Optimization via Augmented Lagrange
#'
#' This meta-learner provides fitting procedures for any pairing of loss 
#' function and metalearner function, subject to constraints. The
#' optimization problem is solved by making use of \code{Rsolnp::solnp}, using
#' Lagrange multipliers. For further details, consult the documentation of the
#' \code{Rsolnp} package.
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
#'
#' @export
#
Lrnr_solnp <- R6Class(classname = "Lrnr_solnp",
                              inherit = Lrnr_base, portable = TRUE,
                              class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(params = params)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),
    .train = function(task) {

      verbose = getOption("sl3.verbose")
      params <- self$params
      learner_function <- params$learner_function
      loss_function <- params$loss_function
      X <- as.matrix(task$X)
      Y <- task$Y
      weights <- task$weights
      risk <- function(alphas) {
        preds <- learner_function(alphas, X)
        losses <- loss_function(preds, Y)
        risk <- weighted.mean(losses, weights)
        
        return(risk)
      }
      eq_fun <- function(alphas) {
        sum(alphas)
      }
      p <- ncol(X)
      init_alphas <- rep(1/p, p)
      fit_object <- Rsolnp::solnp(init_alphas, risk,
                                  eqfun = eq_fun, eqB = 1,
                                  LB = rep(0L, ncol(task$X)),
                                  control = list(trace=0))
      fit_object$coef <- fit_object$pars
      names(fit_object$coef) <- colnames(task$X)

      fit_object$name <- "solnp"
      return(fit_object)
    },
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- as.matrix(task$X)
      alphas <- self$fit_object$coef
      predictions <- self$params$learner_function(alphas, X)
      
      return(predictions)
    },
    .required_packages = c("Rsolnp")
  ),
)

