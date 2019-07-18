#' Density Estimation With Mean Model and Homoscedastic Errors
#'
#' This learner assumes a mean model with homoscedastic errors: Y ~ E(Y|W) + epsilon. E(Y|W) is fit using any mean learner,
#' and then the errors are fit with kernel density estimation.
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
#'   \item{\code{binomial_learner}}{The learner to wrap.}
#' }
#'
#' @template common_parameters
#
Lrnr_density_hse <- R6Class(
  classname = "Lrnr_density_hse",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(mean_learner = NULL, ...) {
      if (is.null(mean_learner)) {
        mean_learner <- make_learner(Lrnr_glm_fast)
      }
      params <- list(mean_learner = mean_learner, ...)
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("density"),

    .train = function(task) {
      mean_learner <- self$params$mean_learner
      mean_fit <- mean_learner$train(task)

      # TODO: maybe these should be cv errors?
      mean_preds <- mean_fit$predict()
      errors <- task$Y - mean_preds
      dens_fit <- density(errors)
      fit_object <- list(mean_fit = mean_fit, dens_fit = dens_fit)
      return(fit_object)
    },

    .predict = function(task) {
      mean_fit <- self$fit_object$mean_fit
      dens_fit <- self$fit_object$dens_fit
      mean_preds <- mean_fit$predict(task)
      errors <- task$Y - mean_preds
      dens_preds <- approx(dens_fit$x, dens_fit$y, errors, rule = 2)$y
      # dens_preds[is.na(dens_preds)] <- 0
      return(dens_preds)
    },
    .required_packages = c("randomForest")
  )
)
