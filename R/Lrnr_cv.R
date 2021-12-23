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

digest_fold <- function(fold) {
  digest(fold[c("training_set", "validation_set")])
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
#' @importFrom origami training validation fold_index cross_validate combiner_c
#' @importFrom dplyr %>% group_by summarise_all select
#'
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
    cv_risk = function(eval_fun) {
      return(cv_risk(self, eval_fun))
    },
    print = function() {
      print("Lrnr_cv")
      print(self$params$learner)
      # todo: check if fit
    },
    predict_fold = function(task, fold_number = "validation", pred_unique_ts = FALSE) {
      fold_number <- interpret_fold_number(fold_number)
      if (fold_number == "validation") {
        # return cross validation predicitons (what Lrnr_cv$predict does, so use that)
        preds <- self$predict(task)

        ### Time-series addition:
        # Each time point gets an unique final prediction
        if (pred_unique_ts) {
          folds <- task$folds
          index_val <- unlist(lapply(folds, function(fold) {
            fold$validation_set
          }))
          preds_unique <- unique(index_val)

          if (length(unique(index_val)) != length(index_val)) {
            # Average over the same predictions:
            preds <- data.table(index_val, preds)

            preds <- preds %>%
              group_by(index_val) %>%
              summarise_all(mean) %>%
              select(-1)
          }
        }
        return(preds)
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

      if (fold_number == "validation") {
        return(self$chain(task))
      }

      revere_task <- task$revere_fold_task(fold_number)


      predictions <- self$predict_fold(revere_task, fold_number)
      # TODO: make same fixes made to chain here
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
    update = function(task, drop_old = FALSE) {
      if (!self$is_trained) {
        return(self$base_train(task))
      }

      # identify the folds that already have fold fits
      folds <- task$folds
      fold_digests <- sapply(folds, digest_fold)

      old_folds <- self$training_task$folds
      old_fold_digests <- sapply(old_folds, digest_fold)

      # retain past fold fits
      if (drop_old) {
        past_fold_idx <- which(old_fold_digests %in% fold_digests)
        past_folds <- old_folds[past_fold_idx]
        past_fold_fits <- self$fit_object$fold_fits[past_fold_idx]
      } else {
        past_folds <- old_folds
        past_fold_fits <- self$fit_object$fold_fits
      }

      # subset new folds
      new_fold_idx <- which(!(fold_digests %in% old_fold_digests))
      new_folds <- folds[new_fold_idx]


      if (length(new_folds) == 0) {
        # nothing to update
        return(self)
      }
      # construct new task with only new folds
      new_task <- task$next_in_chain(folds = new_folds)

      # set up training for new fold fits
      new_object <- self$clone() # copy parameters, and whatever else
      params <- self$params
      params$folds <- NULL
      do.call(new_object$initialize, params)
      new_fold_fits <- self$train(new_task)

      # update fit_object
      fit_object <- new_fold_fits$fit_object
      new_fold_fits <- fit_object$fold_fits
      all_fold_fits <- c(past_fold_fits, new_fold_fits)
      fit_object$fold_fits <- all_fold_fits
      fit_object$folds <- folds

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

      # fix fold indicies
      folds <- lapply(seq_along(folds), function(fold_index) {
        origami::make_fold(fold_index, folds[[fold_index]]$training_set, folds[[fold_index]]$validation_set)
      })

      cv_predict <- function(fold, fold_fits, task) {
        fold_number <- fold_index()
        revere_task <- task$revere_fold_task(fold_number)

        validation_task <- validation_task(revere_task, fold)
        index <- validation()
        fit <- fold_index(fold_fits)[[1]]
        predictions <- fit$base_predict(validation_task)
        list(
          index = index,
          fold_index = rep(fold_index(), length(index)),
          predictions = data.table(predictions)
        )
      }

      comb_ctrl <- list(combiners = list(
        index = combiner_c, fold_index = combiner_c,
        predictions = function(x) rbindlist(x, fill = TRUE)
      ))
      results <- cross_validate(cv_predict, folds, fold_fits, task,
        use_future = FALSE,
        .combine_control = comb_ctrl
      )
      # Avoids issues with repeated validation samples in time-series cv
      preds <- as.data.table(results$predictions)

      # try to throw out columns with bad predictions
      good_preds <- unlist(preds[, lapply(.SD, function(x) all(!is.na(x)))])
      preds <- preds[, which(good_preds), with = FALSE]

      predictions <- aorder(preds, order(results$index, results$fold_index))

      # don't convert to vector if learner is stack, as stack won't
      if ((ncol(predictions) == 1) && !inherits(self$params$learner, "Stack")) {
        predictions <- unlist(predictions)
      }
      return(predictions)
    },
    .chain = function(task) {
      task <- task$revere_fold_task("validation")
      predictions <- self$predict(task)
      predictions <- as.data.table(predictions)


      # TODO: consider making this check a bit more careful
      if (nrow(task$data) != nrow(predictions)) {
        # TODO: this is copied from cv_risk, we should put in a function somewhere
        get_obsdata <- function(fold, task) {
          list(loss_dt = data.table(
            fold_index = fold_index(),
            index = validation(),
            obs = validation(task$Y),
            id = validation(task$id),
            weights = validation(task$weights)
          ))
        }

        loss_dt <- origami::cross_validate(get_obsdata, task$folds, task)$loss_dt
        loss_dt <- loss_dt[order(index, fold_index)]

        task_to_chain <- task$subset_task(loss_dt$index, drop_folds = TRUE)
      } else {
        task_to_chain <- task
      }

      # Add predictions as new columns
      new_col_names <- task_to_chain$add_columns(predictions, self$fit_uuid)
      # new_covariates = union(names(predictions),task$nodes$covariates)
      return(task_to_chain$next_in_chain(
        covariates = names(predictions),
        column_names = new_col_names,
        offset = NULL,
        folds = NULL
      ))
    },
    .required_packages = c("origami")
  )
)
