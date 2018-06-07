#' Base Class for all sl3 Learners.
#'
#' Generally this base learner class shouldn't be instantiated. It's intended to
#' be an abstract class, although abstract classes aren't explicitly supported
#' by \code{R6}. All learners support the methods and fields documented below.
#' For more information on a particular learner, see its help file.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#
Lrnr_base <- R6Class(
  classname = "Lrnr_base",
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(params = NULL, ...) {
      private$.load_packages()
      if (is.null(params)) {
        params <- list(...)
      }

      private$.params <- params
      private$.learner_uuid <- UUIDgenerate(use.time = TRUE)

      invisible(self)
    },

    subset_covariates = function(task) {
      # allows learners to use only a subset of covariates
      if ("covariates" %in% names(self$params) &&
        !is.null(self$params[["covariates"]])) {
        task_covariates <- task$nodes$covariates
        subset_covariates <- intersect(task_covariates, self$params$covariates)
        return(task$next_in_chain(covariates = subset_covariates))
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

    base_train = function(task, trained_sublearners = NULL) {
      # trains learner to data
      assert_that(is(task, "sl3_Task"))

      # TODO: add error handling
      subsetted_task <- self$subset_covariates(task)

      if (!is.null(trained_sublearners)) {
        fit_object <- private$.train(subsetted_task, trained_sublearners)
      } else {
        fit_object <- private$.train(subsetted_task)
      }
      new_object <- self$clone() # copy parameters, and whatever else
      new_object$set_train(fit_object, task)
      return(new_object)
    },

    set_train = function(fit_object, training_task) {
      private$.fit_object <- fit_object
      save_training <- getOption("sl3.save.training")
      if (is.null(save_training) || save_training) {
        private$.training_task <- training_task
      }
      private$.training_outcome_type <- self$get_outcome_type(training_task)
      private$.fit_uuid <- UUIDgenerate(use.time = TRUE)
    },

    assert_trained = function() {
      if (!self$is_trained) {
        stop(paste(
          "Learner has not yet been train to data.",
          "Call learner$train(task) first."
        ))
      }
    },

    base_predict = function(task = NULL) {
      self$assert_trained()
      if (is.null(task)) {
        task <- private$.training_task
      } else {
        task <- task$next_in_chain(covariates <-
          private$.training_task$nodes$covariates)
      }
      assert_that(is(task, "sl3_Task"))
      subsetted_task <- self$subset_covariates(task)
      predictions <- private$.predict(subsetted_task)

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
      } else {
        task <- task$next_in_chain(covariates <-
          private$.training_task$nodes$covariates)
      }
      assert_that(is(task, "sl3_Task"))
      subsetted_task <- self$subset_covariates(task)
      # use custom chain function if provided
      if (!is.null(private$.custom_chain)) {
        next_task <- private$.custom_chain(subsetted_task)
      } else {
        next_task <- private$.chain(subsetted_task)
      }
      return(next_task)
    },

    train_sublearners = function(task) {
      return(private$.train_sublearners(task))
    },

    train = function(task) {
      delayed_fit <- delayed_learner_train(self, task)
      return(delayed_fit$compute())
    },

    predict = function(task = NULL) {
      delayed_preds <- delayed_learner_fit_predict(self, task)
      return(delayed_preds$compute())
    },

    chain = function(task = NULL) {
      delayed_chained <- delayed_learner_fit_chain(self, task)
      return(delayed_chained$compute())
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
    predict_fold = function(task, fold_number){
      warning(self$name, " is not a cv-aware learner, so self$predict_fold reverts to self$predict")
      self$predict(task)
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
        "specific learners. See listLearners()"
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
      new_col_names <- task$add_columns(self$fit_uuid, predictions)
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
