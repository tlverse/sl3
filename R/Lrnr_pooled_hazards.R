#' Classification from Pooled Hazards
#'
#' This learner provides converts a binomial learner into a multinomial learner
#' using a pooled hazards model.
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
#'   \item{\code{binomial_learner}}{The learner to wrap.}
#' }
#'
#' @template common_parameters
#'
#' @examples
#' library(data.table)
#' set.seed(74294)
#'
#' n <- 500
#' x <- rnorm(n)
#' epsilon <- rnorm(n)
#' y <- 3 * x + epsilon
#' data <- data.table(x = x, y = y)
#' task <- sl3_Task$new(data, covariates = c("x"), outcome = "y")
#'
#' # instantiate learners
#' hal <- Lrnr_hal9001$new(
#'   lambda = exp(seq(-1, -13, length = 100)),
#'   max_degree = 6,
#'   smoothness_orders = 0
#' )
#' hazard_learner <- Lrnr_pooled_hazards$new(hal)
#' density_learner <- Lrnr_density_discretize$new(
#'   hazard_learner,
#'   type = "equal_range",
#'   n_bins = 5
#' )
#'
#' # fit discrete density model to pooled hazards data
#' set.seed(74294)
#' fit_density <- density_learner$train(task)
#' pred_density <- fit_density$predict()
Lrnr_pooled_hazards <- R6Class(
  classname = "Lrnr_pooled_hazards",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(binomial_learner = NULL, ...) {
      if (is.null(binomial_learner)) {
        binomial_learner <- make_learner(Lrnr_glm_fast)
      }
      params <- list(binomial_learner = binomial_learner, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("categorical"),
    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)

      if (outcome_type$type != "categorical") {
        stop("Lrnr_pooled_hazards only works for categorical outcomes")
      }

      hazards_task <- pooled_hazard_task(task)
      outcome_levels <- task$outcome_type$levels
      binomial_learner <- self$params$binomial_learner
      hazards_fit <- binomial_learner$train(hazards_task)
      # NOTE: drop hazards training_task to save memory
      hazards_fit$set_train(hazards_fit$fit_object, NULL)
      fit_object <- list(
        hazards_fit = hazards_fit,
        outcome_levels = outcome_levels
      )
      return(fit_object)
    },
    .predict = function(task) {
      pred_hazards_task <- pooled_hazard_task(task, trim = FALSE)
      raw_preds <- self$fit_object$hazards_fit$predict(pred_hazards_task)
      predmat <- matrix(raw_preds, nrow = task$nrow, byrow = FALSE)

      # probability of surviving until time t
      psurv <- t(apply(1 - predmat, 1, cumprod))
      psurv <- cbind(1, psurv)[, seq_len(ncol(predmat))]
      predictions <- psurv * predmat
      predictions <- normalize_rows(predictions)

      predictions <- pack_predictions(predictions)
      return(predictions)
    },
    .required_packages = c()
  )
)
