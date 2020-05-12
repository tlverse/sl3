#' Fit conditional hazards by first transfomring the task
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
Lrnr_conditional_hazards <- R6Class(
  classname = "Lrnr_conditional_hazards",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(outcome_name, Delta, T_tilde, binomial_learner = NULL, ...) {
      if (is.null(binomial_learner)) {
        binomial_learner <- make_learner(Lrnr_glm_fast)
      }
      params <- list(binomial_learner = binomial_learner, 
        outcome_name = outcome_name, Delta = Delta, T_tilde = T_tilde, ...)
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("categorical"),

    .train = function(task) {
    	# TODO: check if necessary
      # outcome_type <- self$get_outcome_type(task)

      # if (outcome_type$type != "categorical") {
      #   stop("Lrnr_pooled_hazards only works for categorical outcomes")
      # }

      # transform task
      long_tmle_task <- conditional_hazards_task(task, TRUE, 
        self$params$outcome_name, self$params$Delta, self$params$T_tilde)

      binomial_learner <- self$params$binomial_learner
      hazards_fit <- binomial_learner$train(long_tmle_task)

      # TODO: check if necessary
      # # outcome_levels <- task$outcome_type$levels
      # # NOTE: drop hazards training_task to save memory
      # hazards_fit$set_train(hazards_fit$fit_object, NULL)
      fit_object <- list(
        hazards_fit = hazards_fit
      )
      return(fit_object)
    },

    .predict = function(task) {
    	# transform task
      full_tmle_task <- conditional_hazards_task(task, FALSE, 
        self$params$outcome_name, self$params$Delta, self$params$T_tilde)
      raw_preds <- self$fit_object$hazards_fit$predict(full_tmle_task)

      # pred_hazards_task <- pooled_hazard_task(task, trim = FALSE)
      # raw_preds <- self$fit_object$hazards_fit$predict(pred_hazards_task)
      # predmat <- matrix(raw_preds, nrow = task$nrow, byrow = FALSE)

      # # probability of surviving until time t
      # psurv <- t(apply(1 - predmat, 1, cumprod))
      # psurv <- cbind(1, psurv)[, seq_len(ncol(predmat))]
      # predictions <- psurv * predmat
      # predictions <- normalize_rows(predictions)

      # predictions <- pack_predictions(predictions)
      predictions <- raw_preds
      return(predictions)
    },
    .required_packages = c()
  )
)
