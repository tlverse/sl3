# these functions exist for two reasons:
# 1) a delayed(learner) is not a learner and therefore doesn't have the same
#    members as a learner
# 2) delayed_fun does not respect member function's environment (containing self
#    and private)

#' Learner helpers
#'
#' @param learner_class The learner class to instantiate.
#' @param ... Parameters with which to instantiate the learner.
#'
#' @rdname learner_helpers
#'
#' @export
#
delayed_make_learner <- function(learner_class, ...) {
  pred_delayed <- delayed_fun(
    make_learner,
    sequential = TRUE
  )(learner_class, ...)
  return(pred_delayed)
}

#' @param learner A learner object to fit to the task.
#' @param task The task on which to fit.
#' @param trained_sublearners Any data obtained from a \code{train_sublearners}
#'  step.
#'
#' @rdname learner_helpers
#'
#' @export
#
learner_train <- function(learner, task, trained_sublearners) {
  learner$base_train(task, trained_sublearners)
}

#' @rdname learner_helpers
#' @param name a more detailed name for this delayed task, if necessary
#' @export
#
delayed_learner_train <- function(learner, task, name = NULL) {
  start <- proc.time()
  trained_sublearners <- learner$train_sublearners(task)
  timer_trained_sublearners <- proc.time() - start

  train_delayed <- delayed_fun(learner_train)(learner, task,
    trained_sublearners)

  if (is.null(name)) {
    name <- learner$name
  }

  train_delayed$name <- name

  if (!is.null(trained_sublearners)) {
    # if a learner is sequential assume the train step is minimal and don't
    # parallelize
    train_delayed$sequential <- TRUE
  }
  return(train_delayed)
}

#' @param learner_fit a learner object that has already been fit
#'
#' @rdname learner_helpers
#'
#' @export
#
learner_fit_predict <- function(learner_fit, task = NULL) {
  learner_fit$base_predict(task)
}

#' @rdname learner_helpers
#'
#' @export
#
delayed_learner_fit_predict <- function(learner_fit, task = NULL) {
  pred_delayed <- delayed_fun(
    learner_fit_predict,
    sequential = TRUE
  )(learner_fit, task)
  pred_delayed$name <- "predict"
  return(pred_delayed)
}

#' @rdname learner_helpers
#'
#' @export
#
learner_fit_chain <- function(learner_fit, task = NULL) {
  learner_fit$base_chain(task)
}

#' @rdname learner_helpers
#'
#' @export
#
delayed_learner_fit_chain <- function(learner_fit, task = NULL) {
  chain_delayed <- delayed_fun(
    learner_fit_chain,
    sequential = TRUE
  )(learner_fit, task)
  chain_delayed$name <- "chain"
  return(chain_delayed)
}


#' @rdname learner_helpers
#'
#' @export
#
learner_subset_covariates <- function(learner, task) {
  learner$subset_covariates(task)
}

#' @rdname learner_helpers
#'
#' @export
#
learner_process_formula <- function(learner, task) {
  learner$process_formula(task)
}

#' @rdname learner_helpers
#'
#' @export
#
delayed_learner_subset_covariates <- function(learner, task) {
  if (is(task, "Delayed")) {
    # only delay if task is delayed
    subset_delayed <- delayed_fun(learner_subset_covariates)(learner, task)
    subset_delayed$name <- "subset"
  } else {
    subset_delayed <- learner_subset_covariates(learner, task)
  }
  return(subset_delayed)
}

#' @rdname learner_helpers
#'
#' @export
#
delayed_learner_process_formula <- function(learner, task) {
  if (is(task, "Delayed")) {
    # only delay if task is delayed
    process_delayed <- delayed_fun(learner_process_formula)(learner, task)
    process_delayed$name <- "formula"
  } else {
    process_delayed <- learner_process_formula(learner, task)
  }
  return(process_delayed)
}

sl3_delayed_job_type <- function() {
  if (getOption("sl3.enable.future")) {
    return(delayed::FutureJob)
  } else {
    return(delayed::SequentialJob)
  }
}
