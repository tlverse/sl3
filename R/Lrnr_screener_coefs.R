#' Random Forest Screener
#'
#' This learner provides covariate screening procedures using variable
#' importance measures as produced by random forest models, provided by the
#' \code{randomForest} package, \code{\link[randomForest]{randomForest}}
#' function.
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
#'   \item{\code{nVar = 10}}{Number of covariates to select.}
#'   \item{\code{ntree = 1000}}{Number of trees in forest.}
#'   \item{\code{...}}{Other parameters passed to
#'     \code{\link[randomForest]{randomForest}}.}
#' }
#
Lrnr_screener_coefs <- R6Class(
  classname = "Lrnr_screener_coefs",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, threshold=1e-3, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("screener"),

    .train = function(task) {
      learner <- self$params$learner
      fit <- learner$train(task)
      coefs <- as.vector(coef(fit))
      covs <- task$nodes$covariates
      selected <- covs[which(abs(coefs)>self$params$threshold)]

      fit_object <- list(selected = selected)
      return(fit_object)
    },

    .predict = function(task) {
      task$X[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },

    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },

    .required_packages = c()
  )
)
