#' Generalized Linear Models
#'
#' This learner provides fitting procedures for generalized linear models using
#' \code{\link[stats]{glm.fit}}.
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
#'   \item{\code{...}}{Parameters passed to \code{\link[stats]{glm}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_glm <- R6Class(
  classname = "Lrnr_glm", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(intercept = TRUE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),

    .train = function(task) {
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
        args$x <- as.matrix(task$X_intercept)
      } else {
        args$x <- as.matrix(task$X)
      }
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset_transformed(link_fun)
      }

      args$ctrl <- glm.control(trace = FALSE)
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
      fit_object$linkinv_fun <- linkinv_fun
      fit_object$link_fun <- link_fun
      fit_object$training_offset <- task$has_node("offset")
      return(fit_object)
    },
    .predict = function(task) {
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
          eta <- as.matrix(X
          [, which(!is.na(coef)),
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
    }
  )
)
