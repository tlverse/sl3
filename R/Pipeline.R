#' Pipeline (chain) of learners.
#'
#' A Pipeline of learners is a way to "chain" Learners together, where the
#' output of one learner is used as output for the next learner. This can be
#' used for things like screening, two stage machine learning methods, and Super
#' Learning. A pipeline is fit by fitting the first \code{Learner}, calling
#' \code{chain()} to create the next task, which becomes the training data for
#' the next \code{Learner}. Similarly, for prediction, the predictions from the
#' first \code{Learner} become the data to predict on for the next
#' \code{Learner}.
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
#'   \item{\code{...}}{Parameters should be individual \code{Learner}s, in the
#'     order they should be applied.}
#' }
#'
#' @template common_parameters
#
Pipeline <- R6Class(
  classname = "Pipeline",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(...) {
      learners <- list(...)
      params <- list(learners = learners)
      learners_trained <- sapply(learners, `[[`, "is_trained")

      if (all(learners_trained)) {
        # we've been passed a list of existing fits so we're already fit
        private$.fit_object <- list(learner_fits = learners)
        private$.training_task <- learners[[1]]$training_task
      }
      super$initialize(params = params)
    },

    print = function() {
      if (is.null(private$.fit_object)) {
        lapply(self$params$learners, print)
      } else {
        lapply(private$.fit_object, print)
      }
    }
  ),

  active = list(
    name = function() {
      learners <- self$params$learners
      learner_names <- sapply(learners, function(learner) learner$name)
      name <- paste(learner_names, collapse = "___")
      return(name)
    }
  ),

  private = list(
    .train_sublearners = function(task) {
      learners <- self$params$learners
      learner_names <- sapply(learners, function(learner) learner$name)
      learner_fits <- as.list(rep(NA, length(learners)))
      names(learner_fits) <- learner_names
      current_task <- task

      for (i in seq_along(learners)) {
        current_learner <- learners[[i]]
        fit <- delayed_learner_train(current_learner, current_task)
        next_task <- delayed_learner_fit_chain(fit, current_task)
        learner_fits[[i]] <- fit
        current_task <- next_task
      }
      return(bundle_delayed(learner_fits))
    },

    .train = function(task, trained_sublearners) {
      fit_object <- list(learner_fits = trained_sublearners)
      return(fit_object)
    },

    .predict = function(task) {
      # prediction is just chaining until you get to the last fit, and then
      # calling predict
      learner_fits <- private$.fit_object$learner_fits
      next_task <- task

      for (i in seq_along(learner_fits)) {
        current_task <- next_task
        current_fit <- learner_fits[[i]]
        next_task <- current_fit$base_chain(current_task)
      }
      # current_task is now the task for the last fit, so we can just do this
      predictions <- current_fit$base_predict(current_task)
      return(predictions)
    }
  )
)
