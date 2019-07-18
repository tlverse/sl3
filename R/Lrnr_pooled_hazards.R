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
#
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
      fit_object <- list(hazards_fit = hazards_fit,
                         outcome_levels = outcome_levels)
      return(fit_object)
    },

    .predict = function(task) {
      pred_hazards_task <- pooled_hazard_task(task, trim = FALSE)
      raw_preds <- self$fit_object$hazards_fit$predict(pred_hazards_task)
      predmat <- matrix(raw_preds, nrow = task$nrow, byrow = FALSE)

      # probability of surviving until time t
      psurviv <- t(apply(1 - predmat, 1, cumprod))
      psurviv <- cbind(1, psurviv)[, seq_len(ncol(predmat))]
      predictions <- psurviv * predmat
      predictions <- normalize_rows(predictions)

      predictions <- pack_predictions(predictions)
      return(predictions)
    },
    .required_packages = c()
  )
)
