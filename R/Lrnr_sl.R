utils::globalVariables(c("self"))

#' SuperLearner Algorithm
#'
#' Learner that encapsulates the Super Learner algorithm. Fits metalearner on
#' cross-validated predictions from learners. Then forms a pipeline with the
#' learners.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#'   \item{\code{learners}}{The "library" of learners to include}
#'   \item{\code{metalearner}}{The metalearner to be fit on predictions from the
#'     library.}
#'   \item{\code{folds=NULL}}{An \code{origami} folds object. If \code{NULL},
#'     folds from the task are used.}
#'   \item{\code{keep_extra=TRUE}}{Stores all sub-parts of the SL computation.
#'     When set to \code{FALSE} the resultant object has a memory footprint
#'     that is significantly reduced through the discarding of intermediary
#'     data structures.}
#'   \item{\code{...}}{Not used.}
#' }
#'
#' @template common_parameters
#
Lrnr_sl <- R6Class(
  classname = "Lrnr_sl", inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learners, metalearner, folds = NULL,
                              keep_extra = TRUE, ...) {
      # kludge to deal with stack as learners
      if (inherits(learners, "Stack")) {
        learners <- learners$params$learners
      }
      params <- list(
        learners = learners, metalearner = metalearner,
        folds = folds, keep_extra = keep_extra, ...
      )
      super$initialize(params = params, ...)
    },

    print = function() {
      lrn_names <- lapply(self$params$learners, function(obj) obj$name)
      print("SuperLearner:")
      str(lrn_names)
      if (self$is_trained) {
        fit_object <- private$.fit_object
        if (self$params$keep_extra) {
          # standard printing when all sub-parts of SL computation are stored
          print(fit_object$cv_meta_fit)
          print("Cross-validated risk (MSE, squared error loss):")
          print(self$cv_risk(loss_squared_error))
        } else {
          # just print the remaining full fit object when keep_extra = FALSE
          print(fit_object$full_fit$params$learners[[2]])
        }
      }
    },

    metalearner_fit = function() {
      self$assert_trained()
      return(private$.fit_object$cv_meta_fit$fit_object)
    },
    cv_risk = function(loss_fun) {

      # get risks for cv learners (nested cv)
      cv_stack_fit <- self$fit_object$cv_fit
      stack_risks <- cv_stack_fit$cv_risk(loss_fun)
      coefs <- self$coefficients
      set(stack_risks, , "coefficients", coefs[match(stack_risks$learner, names(coefs))])

      # get risks for super learner (revere cv)
      sl_risk <- cv_risk(self, loss_fun)
      set(sl_risk, , "learner", "SuperLearner")

      # combine and return
      risks <- rbind(stack_risks, sl_risk)
      return(risks)
    },
    predict_fold = function(task, fold_number = "validation") {
      meta_task <- self$fit_object$cv_fit$chain_fold(task, fold_number)
      meta_predictions <- self$fit_object$cv_meta_fit$predict(meta_task)
    }
  ),

  active = list(
    name = function() {
      name <- paste("CV", self$params$learner$name, sep = "_")
    },

    coefficients = function() {
      self$assert_trained()
      return(coef(self$fit_object$cv_meta_fit))
    }
  ),

  private = list(
    .properties = c("wrapper", "cv"),

    .train_sublearners = function(task) {
      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        # TODO: this breaks if task is delayed
        folds <- task$folds
      }
      # make stack and CV learner objects
      learners <- self$params$learners
      learner_stack <- do.call(Stack$new, learners)
      cv_stack <- Lrnr_cv$new(learner_stack, folds = folds, full_fit = TRUE)
      cv_stack$custom_chain(drop_offsets_chain)

      # fit stack on CV data
      cv_fit <- delayed_learner_train(cv_stack, task)

      # fit meta-learner
      metalearner <- self$params$metalearner
      cv_meta_task <- delayed_learner_fit_chain(cv_fit, task)
      cv_meta_fit <- delayed_learner_train(metalearner, cv_meta_task)

      # refit stack on full data
      stack_fit <- delayed_learner_train(learner_stack, task)

      # form full SL fit -- a pipeline with the stack fit to the full data,
      # and the metalearner fit to the cv predictions
      fit_object <- list(
        cv_fit = cv_fit, cv_meta_task = cv_meta_task,
        cv_meta_fit = cv_meta_fit
      )
      return(bundle_delayed(fit_object))
    },

    .train = function(task, trained_sublearners) {
      fit_object <- trained_sublearners

      # construct full fit pipeline
      full_stack_fit <- fit_object$cv_fit$fit_object$full_fit
      full_stack_fit$custom_chain(drop_offsets_chain)

      # propagate stack errors from cross-validation to full refit
      cv_errors <- fit_object$cv_fit$fit_object$is_error
      full_stack_fit$update_errors(cv_errors)

      full_fit <- make_learner(Pipeline, full_stack_fit, fit_object$cv_meta_fit)



      fit_object$full_fit <- full_fit

      if (self$params$keep_extra) {
        keep <- names(fit_object)
      } else {
        keep <- c("full_fit")
        # TODO: replace learners with zero weight with smaller fit objects
        # coefs <- self$coefficients
        # nz_learners <- which(coefs > 0)
      }
      return(fit_object[keep])
    },

    .predict = function(task) {
      predictions <- private$.fit_object$full_fit$base_predict(task)
      return(predictions)
    }
  )
)

#' Chain while dropping offsets
#'
#' Allows the dropping of offsets when calling the chain method. This is simply
#' a modified version of the chain method found in \code{Lrnr_base}. INTERNAL
#' USE ONLY.
#'
#' @param task An object of class \code{sl3_Task}.
#'
#' @keywords internal
#
drop_offsets_chain <- function(learner, task) {
  predictions <- learner$predict(task)
  predictions <- as.data.table(predictions)
  # Add predictions as new columns
  new_col_names <- task$add_columns(learner$fit_uuid, predictions)
  # new_covariates = union(names(predictions),task$nodes$covariates)
  return(task$next_in_chain(
    covariates = names(predictions),
    column_names = new_col_names,
    offset = NULL
  ))
}
