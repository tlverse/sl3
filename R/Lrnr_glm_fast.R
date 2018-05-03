## ------------------------------------------------------------------------
## Faster GLM with speedglm, fall back on glm.fit in case of error
## - Always use the internal fitting function (speedglm.wfit, glm.fit)
## - GLM objects are stripped of all the junk (minimal memory footprint)
## - No formula interface (design mat is the input data.table in task$X)
## - Separate interface for interactions (params[["interactions"]])
## - Can override the covariates with a subset of those in task$nodes$covariates
##   (params[["covariates"]])
## - All predictions are based on external matrix multiplication with a
##   family-based link functions
## ------------------------------------------------------------------------

#' Computationally Efficient GLMs
#'
#' This learner provides faster fitting procedures for generalized linear models
#' using the \code{speedglm} package. Arguments are passed to
#' \code{\link[speedglm]{speedglm.wfit}}. Uses \code{\link[stats]{glm.fit}} as a
#' fallback if \code{\link[speedglm]{speedglm.wfit}} fails.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats glm predict family
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
#'   \item{\code{intercept=TRUE}}{If \code{TRUE}, an intercept term is included}
#'   \item{\code{method="Cholesky"}}{The matrix decomposition method to use}
#'   \item{\code{...}}{Other parameters to be passed to
#'     \code{\link[speedglm]{speedglm.wfit}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_glm_fast <- R6Class(
  classname = "Lrnr_glm_fast", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(intercept = TRUE, method = "Cholesky", ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .default_params = list(method = "Cholesky"),

    .properties = c("continuous", "binomial", "weights", "offset"),

    .train = function(task) {
      verbose <- getOption("sl3.transform.offset")
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }

      family_name <- args$family$family
      linkinv_fun <- args$family$linkinv
      link_fun <- args$family$linkfun

      # specify data
      if (args$intercept) {
        args$X <- as.matrix(task$X_intercept)
      } else {
        args$X <- as.matrix(task$X)
      }
      args$y <- outcome_type$format(task$Y)
      args$trace <- FALSE

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset_transformed(link_fun)
      }

      SuppressGivenWarnings({
        fit_object <- try(
          call_with_args(speedglm::speedglm.wfit, args),
          silent = TRUE
        )
      }, GetWarningsToSuppress())

      if (inherits(fit_object, "try-error")) {
        # if failed, fall back on stats::glm
        if (verbose) {
          message(paste(
            "speedglm::speedglm.wfit failed, falling back on",
            "stats:glm.fit;", fit_object
          ))
        }
        args$ctrl <- glm.control(trace = FALSE)
        args$x <- args$X

        SuppressGivenWarnings({
          fit_object <- call_with_args(stats::glm.fit, args)
        }, GetWarningsToSuppress())

        fit_object$linear.predictors <- NULL
        fit_object$weights <- NULL
        fit_object$prior.weights <- NULL
        fit_object$y <- NULL
        fit_object$residuals <- NULL
        fit_object$fitted.values <- NULL
        fit_object$effects <- NULL
        fit_object$qr <- NULL
      }

      fit_object$linkinv_fun <- linkinv_fun
      fit_object$link_fun <- link_fun
      fit_object$training_offset <- task$has_node("offset")
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      if (self$params$intercept) {
        X <- task$X_intercept
      } else {
        X <- task$X
      }

      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) {
        coef <- self$fit_object$coef
        if (!all(is.na(coef))) {
          eta <- as.matrix(X[
            , which(!is.na(coef)),
            drop = FALSE,
            with = FALSE
          ]) %*% coef[!is.na(coef)]

          if (self$fit_object$training_offset) {
            offset <- task$offset_transformed(self$fit_object$link_fun, for_prediction = TRUE)
            eta <- eta + offset
          }

          predictions <- as.vector(self$fit_object$linkinv_fun(eta))
        }
      }
      return(predictions)
    },
    .required_packages = c("speedglm")
  )
)
