#' Wrapping Learner for Package Caret
#'
#' This learner wraps \code{\link[caret]{train}}, providing a procedure to fit the
#' algorithms available in package \code{caret}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict family
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
#'   \item{\code{algorithm}}{The algorithm to use. This argument is passed as
#'   `method` to \code{\link[caret]{train}}. For a detailed description of the
#'   methods available in the package, please consult the documentation for
#'   \code{\link[caret]{train}}.}
#'
#'   \item{\code{method}}{An optional argument specifying the method for
#'   parameter selection criterion. Default is set to "CV" instead of the
#'   \code{\link[caret]{train}} default "boot". This argument is passed to
#'   `trControl` and will not be effective if `trControl` is specified. For a
#'   detailed description, please consult the documentation for
#'   \code{\link[caret]{train}}.}
#'
#'   \item{\code{metric}}{An optional argument specifying the summary metric to
#'   be used to select the optimal model. If not specified, it will be inferred
#'   depending on the outcome type. For a detailed description, please consult
#'   the documentation for \code{\link[caret]{train}}.}
#'
#'   \item{\code{trControl}}{An optional \code{\link[caret]{trainControl}}
#'   object controlling the computational nuances. If not specified, it will be
#'   initialized with `method`. For a detailed description, please consult the
#'   documentation for \code{\link[caret]{train}}.}
#'
#'   \item{\code{...}}{Other parameters passed to \code{\link[caret]{train}}.}
#' }
#
Lrnr_caret <- R6Class(
  classname = "Lrnr_caret", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(algorithm,
                              method = "CV",
                              metric = NULL,
                              trControl = caret::trainControl(method = method),
                              ...) {
      params <- list(
        method = algorithm,
        ...
      )
      # provide two ways for users to specify trControl
      ## 1. Pass the method to `method`
      ## 2. Pass a list of trainControl arguments to `trControl`
      if (typeof(trControl) == "list") {
        params$trControl <- call_with_args(caret::trainControl, trControl)
      } else {
        stop("Specified trControl type is unsupported in Lrnr_caret.")
      }
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical"),

    .train = function(task) {
      # set type
      outcome_type <- self$get_outcome_type(task)
      if (outcome_type$type == "continuous") {
        train_type <- "regression"
      } else if (outcome_type$type %in% c("binomial", "categorical")) {
        train_type <- "classification"
      } else {
        stop("Specified outcome type is unsupported in Lrnr_caret.")
      }
      # load args
      args <- self$params
      # process args
      ## data
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)
      ## metric
      args$metric <- ifelse(is.null(args$metric),
        ifelse(train_type == "regression",
          "RMSE",
          "Accuracy"
        ),
        args$metric
      )
      # fit
      fit_object <- call_with_args(caret::train, args,
        other_valid = list("method", "metric", "y")
      )
      return(fit_object)
    },

    .predict = function(task) {
      outcome_type <- self$training_outcome_type
      if (outcome_type$type == "continuous") {
        predict_type <- "regression"
      } else if (outcome_type$type %in% c("binomial", "categorical")) {
        predict_type <- "classification"
      } else {
        stop("Specified outcome type is unsupported in Lrnr_caret.")
      }
      if (predict_type == "regression") {
        predictions <- stats::predict(private$.fit_object,
          newdata = task$X, type = "raw"
        )
      } else {
        predictions <- stats::predict(private$.fit_object,
          newdata = task$X, type = "prob"
        )[, 2]
      }
      predictions <- as.numeric(predictions)
      return(predictions)
    },

    .required_packages = c("caret")
  )
)
