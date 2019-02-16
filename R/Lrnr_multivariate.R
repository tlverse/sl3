#' Multivariate Learner
#'
#' This learner applies a univariate outcome learner across a vector of outcome variables,
#' effectively transforming it into a multivariate outcome learner
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
#'   \item{\code{learner}}{The learner to wrap.}
#' }
#'
#' @template common_parameters
#
Lrnr_multivariate <- R6Class(
  classname = "Lrnr_multivariate",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner = NULL, ...) {
      if (is.null(learner)) {
        learner <- make_learner(Lrnr_glm_fast)
      }
      params <- list(learner = learner, ...)
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("categorical"),
    .train_sublearners = function(task) {
      learner <- self$params$learner
      outcome_cols <- task$nodes$outcome

      train_univariate_learner <- function(outcome_col, learner, task) {
        univariate_task <- task$next_in_chain(outcome = outcome_col)
        fit_object <- delayed_learner_train(learner, univariate_task)
      }



      univariate_fits <- lapply(outcome_cols, train_univariate_learner, learner, task)
      return(bundle_delayed(univariate_fits))
    },
    .train = function(task, trained_sublearners) {
      outcome_fits <- trained_sublearners
      names(outcome_fits) <- task$nodes$outcome
      fit_object <- list(outcome_fits = outcome_fits, outcome_cols = task$nodes$outcome)

      return(fit_object)
    },

    .predict = function(task) {
      predict_univariate_learner <- function(outcome_col, outcome_fits, task) {
        univariate_task <- task$next_in_chain(outcome = outcome_col)
        univariate_fit <- outcome_fits[[outcome_col]]
        univariate_preds <- univariate_fit$predict(univariate_task)
        return(univariate_preds)
      }

      outcome_cols <- self$fit_object$outcome_cols

      univariate_preds <- sapply(outcome_cols, predict_univariate_learner, self$fit_object$outcome_fits, task)

      # TODO: maybe pack predictions
      predictions <- pack_predictions(univariate_preds)
      return(predictions)
    },
    .required_packages = c()
  )
)
