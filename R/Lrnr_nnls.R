#' Non-negative Linear Least Squares
#'
#' This learner provides fitting procedures for models with non-negative linear
#' least squares, internally using the \code{nnls} package. For details on the
#' fitting procedure, consult the documentation of the \code{nnls} package.
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
Lrnr_nnls <- R6Class(classname = "Lrnr_nnls", inherit = Lrnr_base,
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
      fit_object = private$.fit_object
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
      fit_object <- nnls::nnls(as.matrix(x), y)
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

