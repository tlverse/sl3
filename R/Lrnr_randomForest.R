#' Random Forest Models
#'
#' This learner provides fitting procedures for random forest models, using the
#' \code{randomForest} package. For details on the fitting procedure, consult
#' the documentation of the \code{randomForest} package.
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
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#
Lrnr_randomForest <- R6Class(classname = "Lrnr_randomForest",
                             inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params = list(...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .train = function(task) {
      X <- task$X
      Y <- task$Y
      fit_object <- randomForest::randomForest(y = Y, x = X, ntree = 100,
                                               keep.forest = TRUE,
                                               mtry = floor(ncol(X)),
                                               nodesize = 5, maxnodes = NULL,
                                               importance = FALSE)
      return(fit_object)
    },
    .predict = function(task) {
      predictions = stats::predict(private$.fit_object, newdata = task$X)
      return(predictions)
    },
    .required_packages = c("randomForest")
  )
)

