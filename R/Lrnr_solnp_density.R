#' Nonlinear Optimization via Augmented Lagrange
#'
#' This meta-learner provides fitting procedures for density estimation, finding
#' convex combinations of candidate density estimators by minimizing the
#' cross-validated negative log-likelihood loss of each candidate density. The
#' optimization problem is solved by making use of \code{\link[Rsolnp]{solnp}},
#' using Lagrange multipliers. For further details, consult the documentation of
#' the \code{Rsolnp} package.
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
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#
Lrnr_solnp_density <- R6Class(
  classname = "Lrnr_solnp_density",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(params = params)
    }
  ),

  private = list(
    .covariates = NULL,
    .properties = "density",

    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      eval_fun_loss <- function(alphas) {
        sum(-log(as.vector(as.matrix(task$X) %*% alphas)))
      }
      eq_fun <- function(alphas) {
        sum(alphas)
      }
      fit_object <- Rsolnp::solnp(
        runif(ncol(task$X)), eval_fun_loss,
        eqfun = eq_fun, eqB = 1,
        LB = rep(0L, ncol(task$X))
      )
      fit_object$coef <- fit_object$pars
      names(fit_object$coef) <- colnames(task$X)
      if (verbose) {
        cat("\ndensity meta-learner fit:\n")
        print(fit_object$coef)
      }
      fit_object$name <- "solnp"
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- task$X
      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) {
        coef <- private$.fit_object$coef
        if (!all(is.na(coef))) {
          predictions <- data.table::data.table(as.matrix(X
          [,
            which(!is.na(coef)),
            drop = FALSE, with = FALSE
          ]) %*%
            coef[!is.na(coef)])
        } else {
          stop("all SL model coefficients are NA.")
        }
        setnames(predictions, "likelihood")
      }
      return(predictions)
    },
    .required_packages = c("Rsolnp")
  )
)
