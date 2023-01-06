#' Density from Classification
#'
#' This learner discretizes a continuous density and then fits a categorical learner
#'
#' @docType class
#'
#' @export
#'
#' @importFrom origami make_folds
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
#' 
#' @examples 
#' # load example data
#' data(cpp_imputed)
#' 
#' # create sl3 task
#' task <- sl3_Task$new(
#'   cpp_imputed, 
#'   covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs"), 
#'   outcome = "haz")
#' 
#' # train density discretize learner and make predictions
#' lrnr_discretize <- Lrnr_density_discretize$new(
#'   categorical_learner = Lrnr_glmnet$new()
#' )
#' lrnr_discretize_fit <- lrnr_discretize$train(task)
#' lrnr_discretize_pred <- lrnr_discretize_fit$predict()
Lrnr_density_discretize <- R6Class(
  classname = "Lrnr_density_discretize",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(categorical_learner = NULL, type = "equal_mass",
                          n_bins = 20, breaks = NULL, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("density"),
    .train = function(task) {
      discretized <- discretize_variable(task$Y,
        type = self$params$type,
        n_bins = self$params$n_bins,
        breaks = self$params$breaks
      )

      # make discretized task
      folds <- tryCatch(
        {
          origami::make_folds(strata_ids = factor(discretized$x_discrete))
        },
        warning = function(c) {
          message("Cannot construct stratified CV due to insufficient sample size for at least one level of discretized outcome, using folds from original task")
          task$folds
        }
      )
      new_columns <- task$add_columns(
        data.table(discrete_Y = factor(discretized$x_discrete))
      )
      discrete_task <- task$next_in_chain(
        outcome = "discrete_Y",
        column_names = new_columns,
        folds = folds
      )
      # fit categorical learner to discretized task
      categorical_learner <- self$params$categorical_learner
      if (is.null(categorical_learner)) {
        message("Categorical learner NULL, fitting with LASSO regression")
        categorical_learner <- make_learner(Lrnr_glmnet)
      }
      categorical_fit <- categorical_learner$train(discrete_task)

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
      folds <- tryCatch(
        {
          origami::make_folds(strata_ids = factor(discretized$x_discrete))
        },
        warning = function(c) {
          message("Cannot construct stratified CV due to insufficient sample size for at least one level of discretized outcome, using folds from original task")
          task$folds
        }
      )
      new_columns <- task$add_columns(
        data.table(discrete_Y = factor(discretized$x_discrete))
      )
      discrete_task <- task$next_in_chain(
        outcome = "discrete_Y",
        column_names = new_columns,
        folds = folds
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
