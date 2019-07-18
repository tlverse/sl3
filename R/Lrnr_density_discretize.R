#' Density from Classification
#'
#' This learner discretizes a continuous density and then fits a categorical learner
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
#'   \item{\code{categorical_learner}}{The learner to wrap.}
#' }
#'
#' @template common_parameters
#
Lrnr_density_discretize <- R6Class(
  classname = "Lrnr_density_discretize",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(categorical_learner = NULL, type = "equal_mass",
                              n_bins = 20, ...) {
      if (is.null(categorical_learner)) {
        categorical_learner <- make_learner(Lrnr_glmnet)
      }
      params <- list(
        type = type, n_bins = n_bins,
        categorical_learner = categorical_learner, ...
      )
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("density"),

    .train = function(task) {
      discretized <- discretize_variable(task$Y,
        type = self$params$type,
        n_bins = self$params$n_bins,
        breaks = self$params_breaks
      )

      # make discretized task
      new_columns <-
        task$add_columns(data.table(
          discrete_Y =
            factor(discretized$x_discrete)
        ))
      discrete_task <- task$next_in_chain(
        outcome = "discrete_Y",
        column_names = new_columns
      )

      # fit categorical learner to discretized task
      categorical_fit <- self$params$categorical_learner$train(discrete_task)

      fit_object <- list(
        categorical_fit = categorical_fit,
        breaks = discretized$breaks
      )
      return(fit_object)
    },

    .predict = function(task) {
      # make discretized task
      discretized <- discretize_variable(task$Y,
        breaks = self$fit_object$breaks
      )
      new_columns <-
        task$add_columns(data.table(
          discrete_Y =
            factor(discretized$x_discrete)
        ))
      discrete_task <- task$next_in_chain(
        outcome = "discrete_Y",
        column_names = new_columns
      )

      # predict categorical learner on discretized task
      raw_preds <- self$fit_object$categorical_fit$predict(discrete_task)
      predmat <- unpack_predictions(raw_preds)

      bin_lengths <- diff(self$fit_object$breaks)
      scale_mat <- matrix(rep(1 / bin_lengths, each = task$nrow),
        nrow = task$nrow
      )
      predmat <- predmat * scale_mat

      # subset predictions to only those bins relevant
      obs_pred <- predmat[cbind(seq_len(task$nrow), discretized$x_discrete)]
      return(obs_pred)
    },
    .required_packages = c()
  )
)
