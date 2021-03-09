#' Ranger: Fast(er) Random Forests
#'
#' This learner provides fitting procedures for a fast implementation of Random
#' Forests, particularly suited for high dimensional data, using the routines
#' from \pkg{ranger} thru a call to the function \code{\link[ranger]{ranger}}.
#' Variable importance functionality is also provided through invocation of the
#' \code{\link[ranger]{importance}} method.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'  - \code{num.trees = 500}: Number of trees to be used in growing the forest.
#'  - \code{write.forest = TRUE}: If \code{TRUE}, forest is stored, which is
#'      required for prediction. Set to \code{FALSE} to reduce memory usage if
#'      downstream prediction is not intended.
#'  - \code{importance = "none"}: Variable importance mode, one of "none",
#'      "impurity", "impurity_corrected", "permutation". The "impurity" measure
#'      is the Gini index for classification, the variance of the responses for
#'      regression, and the sum of test statistics (for survival analysis, see
#'      the \code{splitrule} argument of \code{\link[ranger]{ranger}}).
#'  - \code{num.threads = 1}: Number of threads.
#'  - \code{...}: Other parameters passed to \code{\link[ranger]{ranger}}. See
#'      it's documentation for details.
#'
#' @examples
#' data(mtcars)
#' mtcars_task <- sl3_Task$new(
#'   data = mtcars,
#'   covariates = c(
#'     "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
#'     "gear", "carb"
#'   ),
#'   outcome = "mpg"
#' )
#' # simple prediction
#' ranger_lrnr <- Lrnr_ranger$new()
#' ranger_fit <- ranger_lrnr$train(mtcars_task)
#' ranger_preds <- ranger_fit$predict()
#'
#' # variable importance
#' ranger_lrnr_importance <- Lrnr_ranger$new(importance = "impurity_corrected")
#' ranger_fit_importance <- ranger_lrnr_importance$train(mtcars_task)
#' ranger_importance <- ranger_fit_importance$importance()
#'
#' # screening based on variable importance, example in glm pipeline
#' ranger_importance_screener <- Lrnr_screener_importance$new(
#'   learner = ranger_lrnr_importance, num_screen = 3
#' )
#' glm_lrnr <- make_learner(Lrnr_glm)
#' ranger_screen_glm_pipe <- Pipeline$new(ranger_importance_screener, glm_lrnr)
#' ranger_screen_glm_pipe_fit <- ranger_screen_glm_pipe$train(mtcars_task)
Lrnr_ranger <- R6Class(
  classname = "Lrnr_ranger", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(num.trees = 500,
                          write.forest = TRUE,
                          importance = "none",
                          num.threads = 1,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    importance = function(...) {
      if (self$fit_object$importance.mode == "none") {
        stop(
          "This learner was instantiated with the default argument, ",
          "importance=none. Modify this to assess variable importance."
        )
      }
      self$assert_trained()

      # initiate argument list for ranger::importance
      args <- list(...)
      args$x <- self$fit_object

      # calculate importance metrics
      importance_result <- call_with_args(ranger::importance, args,
        keep_all = TRUE
      )

      # sort according to decreasing importance
      return(importance_result[order(importance_result, decreasing = TRUE)])
    }
  ),

  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "importance",
      "weights"
    ),

    .train = function(task) {
      args <- self$params
      if (task$has_node("weights")) {
        args$case.weights <- task$weights
      }
      data_in <- cbind(task$Y, task$X)
      colnames(data_in)[1] <- task$nodes$outcome
      args$data <- data_in
      args$dependent.variable.name <- task$nodes$outcome
      args$probability <- task$outcome_type$type == "categorical"
      fit_object <- call_with_args(ranger::ranger, args)
      return(fit_object)
    },

    .predict = function(task) {
      # extract numeric predictions from custom class ranger.prediction
      predictions <- stats::predict(
        private$.fit_object,
        data = task$X,
        type = "response",
        num.threads = self$params$num.threads
      )

      predictions <- predictions[[1]]

      if (task$outcome_type$type == "categorical") {
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("ranger")
  )
)
