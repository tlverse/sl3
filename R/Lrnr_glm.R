#' GLM Fits
#'
#' This learner provides fitting procedures for generalized linear models by way
#' of a wrapper relying on \code{stats::glm}.
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
#' @field family A \code{family} object from package \code{stats} describing the
#'  error family of the model to be fit. See the documentation for the package
#'  \code{stats} for details, or consult \code{stats::family} directly.
#' @field ... Additional arguments.
#'
#' @importFrom R6 R6Class
#' @importFrom stats glm predict family
#'
#' @export
#
Lrnr_glm <- R6Class(classname = "Lrnr_glm", inherit = Lrnr_base,
                    portable = TRUE, class = TRUE,
  public = list(
    initialize = function(family = gaussian(), ...) {
      params <- list(family = family, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),
    .train = function(task) {
      args <- self$params
      
      
      outcome_type <- self$get_outcome_type(task)
      family <- get_glm_family(args$family, outcome_type)
      args$family <- family
      family_name <- family$family
      linkinv_fun <- family$linkinv
      
      # specify data

      args$x <- as.matrix(task$X_intercept)
      args$y <- task$format_Y(outcome_type)
      
      if(task$has_node("weights")){
        args$weights <- task$weights
      }
      
      if(task$has_node("offset")){
        args$offset <- task$offset
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

      return(fit_object)
    },
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- task$X_intercept
      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) {
        coef <- private$.fit_object$coef
        if (!all(is.na(coef))) {
          eta <- as.matrix(X[, which(!is.na(coef)), drop = FALSE,
                           with = FALSE]) %*% coef[!is.na(coef)]
          predictions <- as.vector(private$.fit_object$linkinv_fun(eta))
        }
      }
      return(predictions)
    }
  ),
)

