#' Random Forests
#'
#' This learner provides fitting procedures for random forest models, using the
#' \code{randomForest} package, using \code{\link[randomForest]{randomForest}}
#' function.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom utils getS3method
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
#'   \item{\code{ntree = 500}}{Number of trees to grow. This should not be set
#'   to too small a number, to ensure that every input row gets predicted at
#'   least a few times.}
#'   \item{\code{keep.forest = TRUE}}{If \code{TRUE}, forest is stored, which is
#'     required for prediction.}
#'   \item{\code{nodesize = 5}}{Minimum number of observations in terminal
#'   nodes.}
#'   \item{\code{...}}{Other parameters passed to
#'   \code{\link[randomForest]{randomForest}}.}
#' }
#' 
#' @examples 
#' library(randomForest)
#' 
#' # Load data
#' data(cpp_imputed)
#' 
#' # Create sl3 Task
#' task <- sl3_Task$new(
#'   cpp_imputed,
#'   covariates = c(
#'     "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"
#'   ),
#'   outcome = "haz"
#' )
#' 
#' # Create learner, train, and get predictions
#' randomForest_learner <- Lrnr_randomForest$new(ntree = 400)
#' randomForest_fit <- randomForest_learner$train(task)
#' randomForest_pred <- randomForest_fit$predict()
Lrnr_randomForest <- R6Class(
  classname = "Lrnr_randomForest",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(ntree = 500,
                          keep.forest = TRUE,
                          nodesize = 5, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    importance = function(...) {
      self$assert_trained()

      # initiate argument list for randomForest::importance
      args <- list(...)
      args$x <- self$fit_object

      # calculate importance metrics
      importance_fun <- utils::getS3method("importance", "randomForest",
        envir = getNamespace("randomForest")
      )

      importance_result <- call_with_args(importance_fun, args)

      # sort by decreasing importance
      if (ncol(importance_result) > 1) {
        # if multiple metrics, choose first column
        message(paste0(
          "Sorting covariates by ", colnames(importance_result)[1],
          " importance metric"
        ))
      }
      importance_result <- importance_result[order(importance_result[, 1],
        decreasing = TRUE
      ), ]
      return(importance_result)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "importance"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      args$x <- task$X
      args$y <- outcome_type$format(task$Y)

      if (is.null(args$mtry)) {
        args$mtry <- floor(ncol(args$x))
      }
      if (outcome_type$type == "binomial") {
        args$y <- factor(args$y, levels = c(0, 1))
      }
      rf_fun <- utils::getS3method("randomForest", "default",
        envir = getNamespace("randomForest")
      )
      fit_object <- call_with_args(rf_fun, args)
      return(fit_object)
    },
    .predict = function(task) {
      outcome_type <- private$.training_outcome_type
      type <- ifelse(outcome_type$type %in% c("binomial", "categorical"),
        "prob", "response"
      )
      predictions <- stats::predict(
        private$.fit_object,
        newdata = task$X,
        type = type
      )
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

#' Importance
#' Extract variable importance measures produced by
#' \code{\link[randomForest]{randomForest}} and order in decreasing order of
#' importance.
#'
#' @param type either 1 or 2, specifying the type of importance measure (1=mean
#' decrease in accuracy, 2=mean decrease in node impurity).
importance <- function(type = ...) {
  self$assert_trained()
  args <- list(...)
  args$x <- self$fit_object
  importance_fun <- getS3method("importance", "randomForest",
    envir = getNamespace("randomForest")
  )
  importance_res <- call_with_args(importance_fun, args)
  importance_res[order(importance_res, decreasing = T), ]
}
