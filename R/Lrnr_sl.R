utils::globalVariables(c("self"))

#' The Super Learner Algorithm
#'
#' Learner that encapsulates the Super Learner algorithm. Fits metalearner on
#' cross-validated predictions from learners. Then forms a pipeline with the
#' learners.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table setcolorder
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
#'   \item{\code{metalearner}}{The metalearner to be fit on predictions from
#'     the library.} If null, \code{\link{default_metalearner} is used to
#'     construct a metalearner based on the outcome_type of the training task.}
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
Lrnr_sl <- R6Class(
  classname = "Lrnr_sl", inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learners, metalearner = "default", folds = NULL,
                          keep_extra = TRUE, ...) {
      # kludge to deal with stack as learners
      if (inherits(learners, "Stack")) {
        learners <- learners$params$learners
      }

      if (inherits(learners, "Lrnr_base")) {
        learners <- list(learners)
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

          # compute MSE once and store, only compute if not available
          # (risk estimates are stored to avoid unnecessary re-calculation)
          if (is.null(private$.cv_risk)) {
            tryCatch(
              {
                # try using eval function based on outcome type
                eval_fun <- private$.params$metalearner$params$eval_function
                private$.cv_risk <- self$cv_risk(eval_fun)
              },
              error = function(c) {
                # check training outcome type explicitly
                metalearner <- default_metalearner(self$training_outcome_type)
                eval_fun <- metalearner$params$eval_function
                private$.cv_risk <- self$cv_risk(eval_fun)
              }
            )
          }
          print("Cross-validated risk:")
          print(private$.cv_risk)
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
    cv_risk = function(eval_fun) {
      # get risks for cv learners (nested cv)
      cv_stack_fit <- self$fit_object$cv_fit
      stack_risks <- cv_stack_fit$cv_risk(eval_fun)

      coefs <- self$coefficients
      if (!is.null(coefs)) {
        ordered_coefs <- coefs[match(stack_risks$learner, names(coefs))]
      } else {
        # metalearner did not provide coefficients.
        ordered_coefs <- rep(NA, length(stack_risks$learner))
      }
      set(stack_risks, , "coefficients", ordered_coefs)

      # make sure that coefficients is the second column, even if the
      # metalearner did not provide coefficients.
      data.table::setcolorder(
        stack_risks,
        c(names(stack_risks)[1], "coefficients")
      )

      # get risks for super learner ("revere" CV)
      sl_risk <- cv_risk(self, eval_fun)
      set(sl_risk, , "learner", "SuperLearner")

      # combine and return
      risks <- rbind(stack_risks, sl_risk)
      return(risks)
    },
    predict_fold = function(task, fold_number = "validation",
                            pred_unique_ts = FALSE) {
      fold_number <- interpret_fold_number(fold_number)
      revere_task <- task$revere_fold_task(fold_number)
      if (fold_number == "full") {
        preds <- self$predict(revere_task)
      } else {
        meta_task <- self$fit_object$cv_fit$chain_fold(
          revere_task,
          fold_number
        )
        preds <- self$fit_object$cv_meta_fit$predict(meta_task)

        if (pred_unique_ts) {
          ### Time-series addition:
          # Each time point gets an unique final prediction
          folds <- revere_task$folds
          index_val <- unlist(lapply(folds, function(fold) {
            fold$validation_set
          }))
          preds_unique <- unique(index_val)

          if (length(unique(index_val)) != length(index_val)) {
            # Average over the same predictions:
            preds <- data.table(index_val, preds)
            preds <- preds[, mean(preds), index_val]
            preds <- as.numeric(preds$V1)
          }
        }
      }

      return(preds)
    },
    update = function(task, drop_old = FALSE) {
      if (!self$is_trained) {
        return(self$train(task))
      }

      fit_object <- self$fit_object
      fit_object$cv_fit <- fit_object$cv_fit$update(task, drop_old = drop_old)

      # fit meta-learner
      fit_object$cv_meta_task <- fit_object$cv_fit$chain(task)
      fit_object$cv_meta_fit <-
        self$params$metalearner$train(fit_object$cv_meta_task)

      # construct full fit pipeline
      full_stack_fit <- fit_object$cv_fit$fit_object$full_fit
      full_stack_fit$custom_chain(drop_offsets_chain)

      full_fit <- make_learner(
        Pipeline, full_stack_fit,
        fit_object$cv_meta_fit
      )
      fit_object$full_fit <- full_fit

      new_object <- self$clone() # copy parameters, and whatever else
      new_object$set_train(fit_object, task)
      return(new_object)
    }
  ),
  active = list(
    name = function() {
      name <- paste("CV", self$params$learner$name, sep = "_")
    },
    coefficients = function() {
      self$assert_trained()
      return(coef(self$fit_object$cv_meta_fit))
    },
    learner_fits = function() {
      result <- self$fit_object$full_fit$learner_fits[[1]]$learner_fits
      return(result)
    }
  ),
  private = list(
    .properties = c("wrapper", "cv"),
    .cv_risk = NULL, # store risk estimates (avoid re-calculation on print)

    .train_sublearners = function(task) {
      # if we get a delayed task, evaluate it
      # TODO: this is a kludge:
      # ideally we'd have Lrnr_sl work on delayed tasks like other learners
      if (inherits(task, "Delayed")) {
        task <- task$compute()
      }

      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        # TODO: this breaks if task is delayed
        folds <- task$folds
      }

      # construct default metalearner if necessary
      metalearner <- self$params$metalearner
      if (is.character(metalearner) && (metalearner == "default")) {
        metalearner <- default_metalearner(task$outcome_type)
        private$.params$metalearner <- metalearner
      }

      # make stack and CV learner objects
      learners <- self$params$learners
      learner_stack <- do.call(Stack$new, list(learners))
      cv_stack <- Lrnr_cv$new(learner_stack, folds = folds, full_fit = TRUE)

      # TODO: read custom chain w/ better cv chain code
      # cv_stack$custom_chain(drop_offsets_chain)

      # fit stack on CV data
      cv_fit <- delayed_learner_train(cv_stack, task)

      # fit meta-learner
      cv_meta_task <- delayed_learner_fit_chain(cv_fit, task)
      cv_meta_fit <- delayed_learner_train(metalearner, cv_meta_task)

      # form full SL fit -- a pipeline with the stack fit to the full data,
      # and the metalearner fit to the CV predictions
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

      full_fit <- make_learner(
        Pipeline, full_stack_fit,
        fit_object$cv_meta_fit
      )
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
      full_task <- task$revere_fold_task("full")
      predictions <- private$.fit_object$full_fit$base_predict(full_task)
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
  # pull out the validation task if we're in a revere context
  task <- task$revere_fold_task("validation")
  predictions <- learner$predict(task)
  predictions <- as.data.table(predictions)
  # Add predictions as new columns
  if (nrow(task$data) != nrow(predictions)) {
    # Gather validation indexes:
    val_index <- unlist(lapply(task$folds, function(fold) {
      fold$validation_set
    }))
    task <- task$subset_task(val_index)
    new_col_names <- task$add_columns(predictions, learner$fit_uuid)
  } else {
    new_col_names <- task$add_columns(predictions, learner$fit_uuid)
  }
  # new_covariates = union(names(predictions),task$nodes$covariates)
  return(task$next_in_chain(
    covariates = names(predictions),
    column_names = new_col_names,
    offset = NULL
  ))
}
