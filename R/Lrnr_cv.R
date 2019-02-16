# sort n-dimensional array (for multinomial/multivariate SL support)
aorder <- function(mat, index, along = 1) {
  dims <- safe_dim(mat)
  args <- ifelse(along == seq_along(dims), "index", "")
  indexer <- paste(c(args, "drop=F"), collapse = ",")
  call <- sprintf("mat[%s]", indexer)
  result <- eval(parse(text = call))

  return(result)
}


#' Subset Tasks for CV
#' THe functions use origami folds to subset tasks. These functions are used by Lrnr_cv
#' (and therefore other learners that use Lrnr_cv). So that nested cv works properly, currently
#' the subsetted task objects do not have fold structures of their own, and so generate them from
#' defaults if nested cv is requested.
#' @importFrom origami training
#' @param task a task to subset
#' @param fold an origami fold object to use for subsetting
#' @export
#' @rdname cv_helpers
train_task <- function(task, fold) {
  return(task$subset_task(training(fold = fold), drop_folds = TRUE))
}

#' @importFrom origami validation
#' @export
#' @rdname cv_helpers
validation_task <- function(task, fold) {
  return(task$subset_task(validation(fold = fold), drop_folds = TRUE))
}

interpret_fold_number <- function(fold_number) {
  if (fold_number == -1) {
    fold_number <- "full"
  } else if (fold_number == 0) {
    fold_number <- "validation"
  }
  return(fold_number)
}

#' Fit/Predict a learner with Cross Validation
#'
#' A wrapper around any learner that generates cross-validate predictions
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom origami training validation fold_index cross_validate
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
#'   \item{\code{full_fit=FALSE}}{If \code{TRUE}, also fit the underlying learner on the full data.
#'   This can then be accessed with predict_fold(task, fold_number="full")
#'   }
#' }
#
Lrnr_cv <- R6Class(
  classname = "Lrnr_cv",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner, folds = NULL, full_fit = FALSE, ...) {
      # check if learner is a list of learners, and if so, make a stack
      if (is.list(learner) && all(sapply(learner, inherits, "Lrnr_base"))) {
        learner <- Stack$new(learner)
      }
      params <- list(learner = learner, folds = folds, full_fit = full_fit, ...)
      super$initialize(params = params, ...)
    },

    cv_risk = function(loss_fun) {
      return(cv_risk(self, loss_fun))
    },

    print = function() {
      print("Lrnr_cv")
      print(self$params$learner)
      # todo: check if fit
    },
    predict_fold = function(task, fold_number = "validation") {
      fold_number <- interpret_fold_number(fold_number)
      if (fold_number == "validation") {
        # return cross validation predicitons (what Lrnr_cv$predict does, so use that)
        return(self$predict(task))
      } else if (fold_number == "full") {
        # check if we did a fold fit, and use that fit if available
        if (self$params$full_fit) {
          fold_fit <- self$fit_object$full_fit
        } else {
          stop("full fit requested, but Lrnr_cv was constructed with full_fit=FALSE")
        }
      } else {
        # use the requested fold fit
        fold_number <- as.numeric(fold_number)
        if (is.na(fold_number) || !(fold_number > 0)) {
          stop("fold_number must be 'full', 'validation', or a positive integer")
        }
        fold_fit <- self$fit_object$fold_fits[[as.numeric(fold_number)]]
      }

      revere_task <- task$revere_fold_task(fold_number)
      preds <- fold_fit$predict(revere_task)
      return(preds)
    },
    chain_fold = function(task, fold_number = "validation") {
      # TODO: make this respect custom_chain

      # Add predictions as new columns
      revere_task <- task$revere_fold_task(fold_number)

      predictions <- self$predict_fold(revere_task, fold_number)
      new_col_names <- revere_task$add_columns(predictions, self$fit_uuid)
      # new_covariates = union(names(predictions),task$nodes$covariates)
      return(revere_task$next_in_chain(
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

      # delayed_train_task <- delayed_fun(train_task)

      delayed_cv_train <- function(fold, learner, task) {
        fold_number <- fold_index()
        revere_task <- task$revere_fold_task(fold_number)
        training_task <- train_task(revere_task, fold)
        fit_object <- delayed_learner_train(learner, training_task)
        return(fit_object)
      }

      if (self$params$full_fit) {
        full_task <- task$revere_fold_task("full")
        full_fit <- delayed_learner_train(learner, full_task)
      } else {
        full_fit <- NULL
      }

      cv_results <- lapply(folds, delayed_cv_train, learner, task)
      results <- list(full_fit = full_fit, fold_fits = bundle_delayed(cv_results))
      return(bundle_delayed(results))
    },

    .train = function(task, trained_sublearners) {
      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        folds <- task$folds
      }

      fold_fits <- trained_sublearners$fold_fits
      full_fit <- trained_sublearners$full_fit

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
        folds = folds, fold_fits = fold_fits, full_fit = full_fit,
        is_error = ever_error
      )
      return(fit_object)
    },

    .predict = function(task) {
      folds <- task$folds
      fold_fits <- private$.fit_object$fold_fits

      cv_predict <- function(fold, fold_fits, task) {
        fold_number <- fold_index()
        revere_task <- task$revere_fold_task(fold_number)

        validation_task <- validation_task(revere_task, fold)
        index <- validation()
        fit <- fold_index(fold_fits)[[1]]
        predictions <- fit$base_predict(validation_task)
        list(index = index, predictions = predictions)
      }

      results <- cross_validate(cv_predict, folds, fold_fits, task, use_future = FALSE)
      predictions <- aorder(results$predictions, order(results$index))
      return(predictions)
    },
    .required_packages = c("origami")
  )
)
