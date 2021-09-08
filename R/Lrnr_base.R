#' Base Class for all sl3 Learners
#'
#' Generally this base learner class should not be instantiated. Intended to be
#' an abstract class, although abstract classes are not explicitly supported
#' by \pkg{R6}. All learners support the methods and fields documented below.
#' For more information on a particular learner, see its help file.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction
#'
#' @format \code{\link{R6Class}} object.
#'
#' @template common_parameters
#' @template Lrnr_base_extra
#'
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom uuid UUIDgenerate
#' @importFrom BBmisc requirePackages
#'
#' @family Learners
Lrnr_base <- R6Class(
  classname = "Lrnr_base",
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(params = NULL, name = NULL, ...) {
      private$.load_packages()
      if (is.null(params)) {
        params <- list(...)
      }

      private$.params <- params
      private$.name <- name
      private$.learner_uuid <- UUIDgenerate(use.time = TRUE)

      invisible(self)
    },
    subset_covariates = function(task) {
      # learners subset task covariates based on their covariate set
      if ("covariates" %in% names(self$params) &&
        !is.null(self$params[["covariates"]])) {
        task_covs <- task$nodes$covariates
        learner_covs <- self$params$covariates
        task_covs_missing <- setdiff(learner_covs, task_covs)

        # omit missingness indicators from covariates missing in the task
        delta_idx <- grep("delta_", task_covs_missing)
        if (length(delta_idx) > 0) {
          delta_missing <- task_covs_missing[delta_idx]
          task_covs_missing <- task_covs_missing[-delta_idx]
        }

        # error when task is missing covariates
        if (length(task_covs_missing) > 0) {
          stop(
            sprintf(
              "Task missing the following covariates expected by %s: %s",
              self$name, paste(task_covs_missing, collapse = ", ")
            )
          )
        }

        # subset task covariates to only includes those in learner covariates
        covs_subset <- intersect(task_covs, learner_covs)

        # return updated task
        if (length(delta_idx) == 0) {
          # re-order the covariate subset to match order of learner covariates
          ordered_covs_subset <- covs_subset[match(covs_subset, learner_covs)]
          return(task$next_in_chain(covariates = ordered_covs_subset))
        } else {
          # incorporate missingness indicators in task covariates subset & sort
          covs_subset_delta <- c(covs_subset, delta_missing)
          ord_covs <- covs_subset_delta[match(covs_subset_delta, learner_covs)]

          # incorporate missingness indicators in task data
          delta_missing_data <- matrix(0, nrow(task$data), length(delta_idx))
          colnames(delta_missing_data) <- delta_missing
          cols <- task$add_columns(data.table(delta_missing_data))

          return(task$next_in_chain(
            covariates = ord_covs,
            column_names = cols
          ))
        }
      } else {
        return(task)
      }
    },
    get_outcome_type = function(task) {
      outcome_type <- task$outcome_type
      if (!is.null(self$params$outcome_type)) {
        # learners can override task outcome type
        outcome_type <- self$params$outcome_type
      }
      # make sure outcome type is a variable_type object
      if (is.character(outcome_type)) {
        outcome_type <- variable_type(type = outcome_type, x = task$Y)
      }
      return(outcome_type)
    },
    get_outcome_range = function(task = NULL, fold_number = "full") {
      # return the support of learner
      # if task is specified, return task observations based supports
      # TODO: fold
      warning(paste(
        "Cannot get the outcome range of this learner.",
        "Returning an approximated range."
      ))
      average <- try(apply(self$training_task$Y, 2, FUN = mean))
      if (class(average) == "try-error") {
        average <- mean(self$training_task$Y)
        minimum <- min(self$training_task$Y)
        maximum <- max(self$training_task$Y)
        range <- c(
          minimum + 0.5 * (minimum - average) / 3.,
          maximum + 0.5 * (maximum - average) / 3.
        )
      } else {
        minimum <- apply(self$training_task$Y, 2, FUN = min)
        maximum <- apply(self$training_task$Y, 2, FUN = max)
        range <- rbind(
          minimum + 0.5 * (minimum - average) / 3.,
          maximum + 0.5 * (maximum - average) / 3.
        )
      }
      return(range)
    },
    base_train = function(task, trained_sublearners = NULL) {

      # trains learner to data
      assert_that(is(task, "sl3_Task"))

      task <- self$subset_covariates(task)
      processed_task <- self$process_formula(task)

      verbose <- getOption("sl3.verbose")

      if (!is.null(trained_sublearners)) {
        fit_object <- private$.train(processed_task, trained_sublearners)
      } else {
        fit_object <- private$.train(processed_task)
      }
      new_object <- self$clone() # copy parameters, and whatever else
      new_object$set_train(fit_object, task)
      return(new_object)
    },
    set_train = function(fit_object, training_task) {
      private$.fit_object <- fit_object
      # for predict/chaining subset covariates to be same as training task
      if (!inherits(training_task, "sl3_revere_Task") &&
        (is.null(private$.params$covariates))) {
        private$.params$covariates <- training_task$nodes$covariates
      }
      save_training <- getOption("sl3.save.training")
      if (is.null(save_training) || save_training) {
        private$.training_task <- training_task
      }
      if (!is.null(training_task)) {
        private$.training_outcome_type <- self$get_outcome_type(training_task)
      }
      private$.fit_uuid <- UUIDgenerate(use.time = TRUE)
    },
    assert_trained = function() {
      if (!self$is_trained) {
        stop(paste(
          "Learner has not yet been trained to data.",
          "Call learner$train(task) first."
        ))
      }
    },
    base_predict = function(task = NULL) {
      self$assert_trained()
      if (is.null(task)) {
        task <- private$.training_task
      }

      assert_that(is(task, "sl3_Task"))
      task <- self$subset_covariates(task)
      task <- self$process_formula(task)

      predictions <- private$.predict(task)

      ncols <- ncol(predictions)
      if (!is.null(ncols) && (ncols == 1)) {
        predictions <- as.vector(predictions)
      }
      return(predictions)
    },
    base_chain = function(task = NULL) {
      self$assert_trained()
      if (is.null(task)) {
        task <- private$.training_task
      }

      assert_that(is(task, "sl3_Task"))
      task <- self$subset_covariates(task)
      task <- self$process_formula(task)

      # use custom chain function if provided
      if (!is.null(private$.custom_chain)) {
        next_task <- private$.custom_chain(self, task)
      } else {
        next_task <- private$.chain(task)
      }
      return(next_task)
    },
    train_sublearners = function(task) {
      # TODO: add error handling
      task <- delayed_learner_subset_covariates(self, task)
      task <- delayed_learner_process_formula(self, task)

      return(private$.train_sublearners(task))
    },
    train = function(task) {
      delayed_fit <- delayed_learner_train(self, task)
      verbose <- getOption("sl3.verbose")
      return(delayed_fit$compute(
        job_type = sl3_delayed_job_type(),
        progress = verbose
      ))
    },
    predict = function(task = NULL) {
      delayed_preds <- delayed_learner_fit_predict(self, task)
      return(delayed_preds$compute(job_type = sl3_delayed_job_type()))
    },
    sample = function(task, n_samples = 30, fold_number = "full") {
      stop("This learner does not have a sampling method.")
    },
    chain = function(task = NULL) {
      delayed_chained <- delayed_learner_fit_chain(self, task)
      return(delayed_chained$compute(job_type = sl3_delayed_job_type()))
    },
    print = function() {
      print(self$name)
      # print(self$params)
      fit_object <- private$.fit_object
      if (!is.null(fit_object)) print(fit_object)
    },
    custom_chain = function(new_chain_fun = NULL) {
      private$.custom_chain <- new_chain_fun
    },
    predict_fold = function(task, fold_number = "full") {
      # support legacy "magic number" definitions
      fold_number <- interpret_fold_number(fold_number)
      # for non-CV learners, do full predict no matter what, but warn about it
      # if fold_number is something else
      if (fold_number != "full") {
        warning(
          self$name,
          " is not cv-aware: self$predict_fold reverts to self$predict"
        )
      }
      self$predict(task)
    },
    reparameterize = function(new_params) {
      # modify learner parameters
      new_self <- self$clone()
      new_self$.__enclos_env__$private$.params[names(new_params)] <-
        new_params[]
      return(new_self)
    },
    retrain = function(new_task, trained_sublearners = NULL) {

      # retrains fitted learner on a new task
      assert_that(is(new_task, "sl3_Task"))
      stopifnot(self$is_trained)

      verbose <- getOption("sl3.verbose")

      # copy fit, reset covariates parameter, and retrain as new object
      new_self <- self$clone()
      if ("covariates" %in% names(new_self$params) &
        !is.null(new_self$params[["covariates"]])) {
        idx <- which(names(new_self$params) == "covariates")
        params_no_covars <- new_self$.__enclos_env__$private$.params[-idx]
        new_self$.__enclos_env__$private$.params <- params_no_covars
      }
      if (!is.null(trained_sublearners)) {
        new_fit_object <-
          new_self$.__enclos_env__$private$.train(
            new_task,
            trained_sublearners
          )
      } else {
        new_fit_object <- new_self$.__enclos_env__$private$.train(new_task)
      }
      new_object <- new_self$clone() # copy parameters, and whatever else
      new_object$set_train(new_fit_object, new_task)
      return(new_object)
    },
    process_formula = function(task) {
      if ("formula" %in% names(self$params) &&
        !is.null(self$params[["formula"]])) {
        form <- self$params$formula
        if (class(form) != "formula") form <- as.formula(form)

        # check response variable corresponds to outcome in task, if provided
        if (attr(terms(form), "response")) {
          if (!all.vars(form)[1] == task$nodes$outcome) {
            stop(paste0(
              "Outcome variable in formula ", all.vars(form)[1],
              " does not match the task's outcome ", task$nodes$outcome
            ))
          }
          formula_covars <- all.vars(form)[-1]
        } else {
          formula_covars <- all.vars(form)
        }
        # check that regressors in the formula are contained in the task
        if (!all(formula_covars %in% task$nodes$covariates)) {
          stop("Regressors in the formula are not covariates in task")
        }

        # get data corresponding to formula and add new columns to the task
        data <- as.data.table(stats::model.matrix(form, data = task$data))
        formula_cols <- names(data)
        if (any(grepl("Intercept", formula_cols))) {
          formula_cols <- formula_cols[!grepl("Intercept", formula_cols)]
        }
        new_cols <- setdiff(formula_cols, names(task$data))
        data <- data[, new_cols, with = FALSE]
        new_cols <- task$add_columns(data)
        return(
          task$next_in_chain(covariates = formula_cols, column_names = new_cols)
        )
      } else {
        return(task)
      }
    }
  ),
  active = list(
    is_trained = function() {
      return(!is.null(private$.fit_object))
    },
    fit_object = function() {
      fit_object <- private$.fit_object
      return(fit_object)
    },
    name = function() {
      # TODO: allow custom names
      if (is.null(private$.name)) {
        params <- self$params
        if (length(params) > 0) {
          # TODO: sanitize further
          atom_params <- sapply(params, is.atomic)
          params <- params[atom_params]
        }
        props <- c(list(class(self)[1]), params)
        name <- paste(props, collapse = "_")
        private$.name <- name
      }
      return(private$.name)
    },
    learner_uuid = function() {
      return(private$.learner_uuid)
    },
    fit_uuid = function() {
      return(private$.fit_uuid)
    },
    params = function() {
      return(private$.params)
    },
    training_task = function() {
      return(private$.training_task)
    },
    training_outcome_type = function() {
      return(private$.training_outcome_type)
    },
    properties = function() {
      return(private$.properties)
    },
    coefficients = function() {
      self$assert_trained()
      coefs <- try(coef(self$fit_object))
      if (inherits(coefs, "try-error")) {
        return(NULL)
      } else {
        return(coefs)
      }
    }
  ),
  private = list(
    .name = NULL,
    .fit_object = NULL,
    .training_task = NULL,
    .training_outcome_type = NULL,
    .learner_uuid = NULL,
    .fit_uuid = NULL,
    .params = NULL,
    .required_packages = NULL,
    .properties = list(),
    .custom_chain = NULL,
    .train_sublearners = function(task) {
      # train sublearners here
      return(NULL)
    },
    .train = function(task) {
      stop(paste(
        "Learner is meant to be abstract, you should instead use",
        "specific learners. See sl3_list_learners()"
      ))
    },
    .predict = function(task) {
      predictions <- predict(private$.fit_object, newdata = task$X)
      return(predictions)
    },
    .chain = function(task) {
      predictions <- self$predict(task)
      predictions <- as.data.table(predictions)
      # Add predictions as new columns
      task <- task$revere_fold_task("full")
      new_col_names <- task$add_columns(predictions, self$fit_uuid)
      # new_covariates = union(names(predictions),task$nodes$covariates)
      return(task$next_in_chain(
        covariates = names(predictions),
        column_names = new_col_names
      ))
    },
    .load_packages = function() {
      if (!is.null(private$.required_packages)) {
        requirePackages(
          private$.required_packages,
          why = class(self)[1],
          default.method = "load"
        )
      }
    }
  )
)

#' @rdname Lrnr_base
#'
#' @param learner_class The learner class to instantiate.
#' @param ... Parameters with which to instantiate the learner. See Parameters
#'  section below.
#'
#' @export
#
make_learner <- function(learner_class, ...) {
  if (is.character(learner_class)) {
    learner_class <- get(learner_class)
  }
  learner_class$new(...)
}
