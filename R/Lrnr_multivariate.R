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
#'
#' @examples
#' library(data.table)
#'
#' # simulate data
#' set.seed(123)
#' n <- 1000
#' p <- 5
#' pY <- 3
#' W <- matrix(rnorm(n * p), nrow = n)
#' colnames(W) <- sprintf("W%d", seq_len(p))
#' Y <- matrix(rnorm(n * pY, 0, 0.2) + W[, 1], nrow = n)
#' colnames(Y) <- sprintf("Y%d", seq_len(pY))
#' data <- data.table(W, Y)
#' covariates <- grep("W", names(data), value = TRUE)
#' outcomes <- grep("Y", names(data), value = TRUE)
#'
#' # make sl3 task
#' task <- sl3_Task$new(data.table::copy(data),
#'   covariates = covariates,
#'   outcome = outcomes
#' )
#'
#' # train multivariate learner and make predictions
#' mv_learner <- make_learner(Lrnr_multivariate, make_learner(Lrnr_glm_fast))
#' mv_fit <- mv_learner$train(task)
#' mv_pred <- mv_fit$predict(task)
#' mv_pred <- unpack_predictions(mv_pred)
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
  active = list(
    name = function() {
      learner_name <- self$params$learner$name
      # learner_names = sapply(learners, function(learner) learner$name)
      # name = paste(learner_names, collapse="x")
      name <- sprintf("%s_multivariate", learner_name)
      return(name)
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

      univariate_fits <- lapply(
        outcome_cols, train_univariate_learner,
        learner, task
      )
      return(bundle_delayed(univariate_fits))
    },
    .train = function(task, trained_sublearners) {
      outcome_fits <- trained_sublearners
      names(outcome_fits) <- task$nodes$outcome
      fit_object <- list(
        outcome_fits = outcome_fits,
        outcome_cols = task$nodes$outcome
      )
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

      univariate_preds <- sapply(
        outcome_cols, predict_univariate_learner,
        self$fit_object$outcome_fits, task
      )

      # TODO: maybe pack predictions
      predictions <- pack_predictions(univariate_preds)
      return(predictions)
    },
    .required_packages = NULL
  )
)
