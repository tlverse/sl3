#' Bound Predictions
#'
#' This learner bounds predictions. Intended for use in a pipeline.
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
#' @section Parameters:
#'   - \code{bound}: Either a vector of length two, with lower and upper bounds,
#'     or a vector of length 1 with a lower bound, and the upper will be set
#'     symmetrically as 1 - the lower bound. Both bounds must be provided when
#'     the variable type of the task's outcome is continuous.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' hal_lrnr <- Lrnr_hal9001$new(
#'   max_degree = 1, num_knots = c(20, 10), smoothness_orders = 0
#' )
#' lasso_lrnr <- Lrnr_glmnet$new()
#' glm_lrnr <- Lrnr_glm$new()
#' ranger_lrnr <- Lrnr_ranger$new()
#' lrnr_stack <- make_learner(Stack, lasso_lrnr, glm_lrnr, ranger_lrnr)
#' lrnr_bound <- Lrnr_bound$new(c(-2, 2))
#' stack_bounded_preds <- Pipeline$new(lrnr_stack, lrnr_bound)
#' metalrnr_discrete_MSE <- Lrnr_cv_selector$new(loss_squared_error)
#' discrete_sl <- Lrnr_sl$new(
#'   learners = stack_bounded_preds, metalearner = metalrnr_discrete_MSE
#' )
#' discrete_sl_fit <- discrete_sl$train(task)
#' preds <- discrete_sl_fit$predict()
#' range(preds)
Lrnr_bound <- R6Class(
  classname = "Lrnr_bound",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(bound = 0.005) {
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights", "wrapper"
    ),

    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)
      if (outcome_type == "continuous" & length(self$params$bound) == 1) {
        stop(
          "Both upper and lower bounds are required when the outcome is",
          "continuous."
        )
      }
      fit_object <- list()
      return(fit_object)
    },

    .predict = function(task = NULL) {
      X <- as.matrix(task$X)
      bounds <- self$params$bound

      bound <- function(x, bounds) {
        lower <- bounds[[1]]
        if (length(bounds) > 1) {
          upper <- bounds[[2]]
        } else {
          upper <- 1 - lower
        }
        pmin(pmax(x, lower), upper)
      }

      predictions <- bound(X, bounds)
      return(predictions)
    }
  )
)
