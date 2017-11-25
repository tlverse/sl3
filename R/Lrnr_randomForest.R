#' Random Forests
#'
#' This learner provides fitting procedures for random forest models, using the
#' \code{randomForest} package, using \code{\link[randomForest]{randomForest}}
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
#'   \item{\code{ntree=100}}{Number of trees in forest}
#'   \item{\code{keep.forest=TRUE}}{If \code{TRUE}, forest is stored, which is
#'     required for prediction.}
#'   \item{\code{nodesize=5}}{Minimum number of observations in terminal (leaf)
#'     nodes.}
#'   \item{\code{maxnodes=NULL}}{Maximum number of terminal (leaf) nodes in each
#'     tree.}
#'   \item{\code{importance=FALSE}}{Store variable importance information.}
#'   \item{\code{...}}{Other parameters passed to
#'     \code{\link[randomForest]{randomForest}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_randomForest <- R6Class(classname = "Lrnr_randomForest",
                             inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(ntree = 100,
                    keep.forest = TRUE,
                    nodesize = 5, maxnodes = NULL,
                    importance = FALSE, ...) {
      params = args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      args$x <- task$X
      args$y <- outcome_type$format(task$Y)

      if (is.null(args$mtry)) {
        args$mtry = floor(ncol(args$x))
      }
      if (outcome_type$type == "binomial") {
        args$y <- factor(args$y, levels = c(0, 1))
      }
      rf_fun <- getS3method("randomForest", "default",
                            envir = getNamespace("randomForest"))
      fit_object <- call_with_args(rf_fun, args)
      return(fit_object)
    },

    .predict = function(task) {
      outcome_type <- private$.training_outcome_type
      type <- ifelse(outcome_type$type %in% c("binomial","categorical"),
                     "prob", "response")
      predictions = stats::predict(private$.fit_object, newdata = task$X,
                                   type = type)
      if (outcome_type$type == "binomial") {
        # extract p(Y=1)
        predictions <- predictions[, 2]
      } else if (outcome_type$type == "categorical") {
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("randomForest")
  )
)

