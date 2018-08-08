# sort n-dimensional array (for multinomial/multivariate SL support)
aorder <- function(mat, index, along = 1) {
  dims <- safe_dim(mat)
  args <- ifelse(along == seq_along(dims), "index", "")
  indexer <- paste(c(args, "drop=F"), collapse = ",")
  call <- sprintf("mat[%s]", indexer)
  result <- eval(parse(text = call))

  return(result)
}

#' Fit/Predict a learner with Cross Validation
#'
#' A wrapper around any learner that generates cross-validate predictions
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom origami training validation fold_index
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
#'   \item{\code{learner}}{The learner to wrap}
#'   \item{\code{folds=NULL}}{An \code{origami} folds object. If \code{NULL},
#'    folds from the task are used}
#' }
#
Lrnr_cv <- R6Class(
  classname = "Lrnr_cv",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner, folds = NULL, ...) {
      # check if learner is a list of learners, and if so, make a stack
      if (is.list(learner) && all(sapply(learner, inherits, "Lrnr_base"))) {
        learner <- Stack$new(learner)
      }
      params <- list(learner = learner, folds = folds, ...)
      super$initialize(params = params, ...)
    },

    cv_risk = function(loss) {
      preds <- self$predict()
      task <- self$training_task
      risks <- apply(preds, 2, risk, task$Y, loss, task$weights)
    },

    print = function() {
      print("Lrnr_cv")
      print(self$params$learner)
      # todo: check if fit
    },
    predict_fold = function(task, fold_number = 0) {
      if (fold_number != 0) {
        fold_fit <- self$fit_object$fold_fits[[fold_number]]
        return(fold_fit$predict(task))
      } else {
        return(self$predict(task))
      }
    },
    chain_fold = function(task, fold_number = 0) {
      predictions <- self$predict_fold(task, fold_number)
      # Add predictions as new columns
      new_col_names <- task$add_columns(self$fit_uuid, predictions)
      # new_covariates = union(names(predictions),task$nodes$covariates)
      return(task$next_in_chain(
        covariates = names(predictions),
        column_names = new_col_names
      ))
    }
  ),

  active = list(
    name = function() {
      name <- paste("CV", self$params$learner$name, sep = "_")
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
      learner <- self$params$learner

      train_task <- function(task, fold) {
        return(task[fold$training_set])
      }

      delayed_train_task <- delayed_fun(train_task)

      delayed_cv_train <- function(fold, learner, task) {
        training_task <- delayed_train_task(task, fold)
        training_task$sequential <- TRUE
        fit_object <- delayed_learner_train(learner, training_task)
        return(fit_object)
      }

      # TODO: maybe write delayed_cross_validate (as it'd be a neat thing to
      # have around anyway)
      cv_results <- lapply(folds, delayed_cv_train, learner, task)
      result <- bundle_delayed(cv_results)
      return(result)
    },

    .train = function(task, trained_sublearners) {
      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        folds <- task$folds
      }

      fold_fits <- trained_sublearners
      learner <- self$params$learner
      ever_error <- NULL

      if (inherits(learner, "Stack")) {
        # if we're cross-validating a stack, check for learner errors in any
        # folds and then drop for all folds
        errored_learners <- sapply(fold_fits, function(fold_fit) {
          fold_fit$fit_object$is_error
        })
        if (is.vector(errored_learners)) {
          errored_learners <- matrix(
            errored_learners,
            ncol = length(errored_learners)
          )
        }
        ever_error <- apply(errored_learners, 1, any)
        if (any(ever_error)) {
          # drop errored learners from all folds
          for (fold_fit in fold_fits) {
            fold_fit$update_errors(ever_error)
          }
          # warn about dropping
          errored_learners <- learner$params$learners[ever_error]
          errored_names <- sapply(errored_learners, `[[`, "name")
          all_names <- paste(errored_names, collapse = ", ")
          warning(paste(
            "The following learners failed for one or more folds",
            "and will be dropped from all folds: ", all_names
          ))
        }
      }

      fit_object <- list(
        folds = folds, fold_fits = fold_fits,
        is_error = ever_error
      )
      return(fit_object)
    },

    .predict = function(task) {
      # if(!identical(task,private$.training_task)){
      #   stop("task must match training task for Lrnr_cv")
      # }
      # doing train and predict like this is stupid, but that's the paradigm
      # (for now!)
      folds <- task$folds
      fold_fits <- private$.fit_object$fold_fits

      cv_predict <- function(fold, fold_fits, task) {
        validation_task <- validation(task)
        index <- validation()
        fit <- fold_index(fold_fits)[[1]]
        predictions <- fit$base_predict(validation_task)
        list(index = index, predictions = predictions)
      }

      # fold_predictions = cross_validate(cv_predict, folds, fold_fits, task,
      # future.globals = FALSE)
      # don't use cross_validate as it will call future_lapply
      fold_predictions <- lapply(folds, cv_predict, fold_fits, task)
      index <- unlist(lapply(fold_predictions, `[[`, "index"))
      predictions <- data.table::rbindlist(lapply(
        fold_predictions, `[[`,
        "predictions"
      ))
      predictions <- aorder(predictions, order(index))
      return(predictions)
    },
    .required_packages = c("origami")
  )
)
