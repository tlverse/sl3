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

      if (nrow(revere_task$data) != nrow(predictions)) {
        # Gather validation indexes:
        val_index <- unlist(lapply(revere_task$folds, function(fold) {
          fold$validation_set
        }))
        revere_task <- revere_task$subset_task(val_index)
        new_col_names <- revere_task$add_columns(predictions, self$fit_uuid)
      } else {
        new_col_names <- revere_task$add_columns(predictions, self$fit_uuid)
      }

      return(revere_task$next_in_chain(
        covariates = names(predictions),
        column_names = new_col_names
      ))
    },

    update = function(task) {
      # identify the folds that already have fold fits
      folds <- task$folds
      eval_past_fold <- lapply(seq_len(length(folds)), function(x) {
        if (x > length(self$training_task$folds)) {
          equal <- FALSE
        } else {
          equal_training <- all.equal(
            self$training_task$folds[[x]]$training_set,
            task$folds[[x]]$training_set
          )
          equal_validation <- all.equal(
            self$training_task$folds[[x]]$validation_set,
            task$folds[[x]]$validation_set
          )
          equal <- equal_training & equal_validation
        }
        return(equal)
      })
      # retain past fold fits
      past_folds <- which(unlist(eval_past_fold))
      past_fold_fits <- self$fit_object$fold_fits[past_folds]
      # subset new folds
      new_folds <- task$folds[which(!unlist(eval_past_fold))]
      # construct new task with only new folds
      new_task <- task$next_in_chain(folds = new_folds)
      # set up training for new fold fits
      new_fold_fits <- self$train(new_task)

      # update fit_object
      fit_object <- new_fold_fits$fit_object
      new_fold_fits <- fit_object$fold_fits
      all_fold_fits <- c(past_fold_fits, new_fold_fits)
      fit_object$fold_fits <- all_fold_fits
      fit_object$folds <- folds
      new_object <- self$clone() # copy parameters, and whatever else
      new_object$set_train(fit_object, task)
      return(new_object)
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
      verbose <- getOption("sl3.verbose")

      # if we get a delayed task, evaluate it
      # TODO: this is a kludge -- ideally we'd have Lrnr_cv work on delayed tasks like other learners
      if (inherits(task, "Delayed")) {
        task <- task$compute()
      }

      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        folds <- task$folds
      }
      learner <- self$params$learner

      # delayed_train_task <- delayed_fun(train_task)

      delayed_cv_train <- function(fold, learner, task) {
        fold_number <- fold_index()
        revere_task <- task$revere_fold_task(fold_number)
        training_task <- train_task(revere_task, fold)
        if (verbose) {
          delayed_name <- sprintf("CV %s fold %s", learner$name, fold_number)
        } else {
          delayed_name <- learner$name
        }

        fit_object <- delayed_learner_train(learner, training_task, delayed_name)
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
        all_fits <- fold_fits
        if (self$params$full_fit) {
          all_fits <- c(all_fits, full_fit)
        }

        errored_learners <- sapply(all_fits, function(fold_fit) {
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

          if (self$params$full_fit) {
            full_fit$update_errors(ever_error)
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
      # Avoids issues with repeated validation samples in time-series cv
      preds <- as.data.table(results$predictions)
      if (length(unique(results$index)) == nrow(preds)) {
        predictions <- aorder(results$predictions, order(results$index))
      } else {
        predictions <- aorder(results$predictions, seq(1, nrow(results$predictions)))
      }

      return(predictions)
    },
    .required_packages = c("origami")
  )
)
