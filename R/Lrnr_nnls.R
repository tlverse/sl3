#' Non-negative Linear Least Squares
#'
#' This learner provides fitting procedures for models with non-negative linear
#' least squares, internally using the \code{nnls} package and
#' \code{\link[nnls]{nnls}} function.
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
#' @return Learner object with methods for both training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{...}}{Not used.}
#' }
#'
#' @template common_parameters
#
Lrnr_nnls <- R6Class(
  classname = "Lrnr_nnls", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(params = params, ...)
    },
    print = function() {
      print(self$name)
      print(self$fits)
    }
  ),

  active = list(
    fits = function() {
      fit_object <- private$.fit_object
      if (!is.null(fit_object)) {
        data.table::data.table(lrnrs = fit_object$lrnrs, weights = fit_object$x)
      } else {
        data.table::data.table(lrnrs = character(0), weights = numeric(0))
      }
    }
  ),

  private = list(
    .properties = c("continuous"),
    .train = function(task) {
      x <- task$X
      y <- task$Y
      wts <- task$weights
      fit_object <- nnls::nnls(sqrt(wts)*as.matrix(x), sqrt(wts)*y)
      fit_object$lrnrs <- names(task$X)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      predictions <- as.matrix(task$X) %*% coef(private$.fit_object)
      return(predictions)
    },
    .required_packages = c("nnls")
  ),
)
