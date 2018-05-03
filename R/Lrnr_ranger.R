#' Ranger
#'
#' This learner provides fitting procedures for a fast implementation of Random
#' Forests, particularly suited for high dimensional data, using the
#' \code{ranger} package, using \code{\link[ranger]{ranger}} function.
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
#'   \item{\code{num.trees=500}}{Number of trees in forest}
#'   \item{\code{write.forest=TRUE}}{If \code{TRUE}, forest is stored, which is
#'     required for prediction. Set to FALSE to reduce memory usage if no
#'     prediction intended.}
#'   \item{\code{...}}{Other parameters passed to
#'     \code{\link[ranger]{ranger}}.}
#' }
#'
#' @template common_parameters
#'
Lrnr_ranger <- R6Class(
  classname = "Lrnr_ranger", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(num.trees = 500,
                          write.forest = TRUE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical"),

    .train = function(task) {
      args <- self$params
      dat <- cbind(task$Y,task$X)
      colnames(dat)[1] <- task$nodes$outcome
      args$data <- dat
      args$dependent.variable.name <- task$nodes$outcome
      fit_object <- call_with_args(ranger::ranger, args)
      return(fit_object)
    },

    .predict = function(task) {
      predictions <- stats::predict(
        private$.fit_object, data = task$X,
        type = "response"
      )
      preds <- predictions[1]
      return(preds)
    },
    .required_packages = c("ranger")
  )
)