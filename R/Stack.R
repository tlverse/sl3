#' Learner Stacking
#'
#' A Stack is a special Learner that combines multiple other learners,
#' "stacking" their predictions in columns.
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
#'   \item{\code{...}}{Parameters should be individual \code{Learner}s.}
#' }
#'
#' @template common_parameters
#
Stack <- R6Class(
  classname = "Stack",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(...) {
      learners <- list(...)
      if (length(learners) == 1) {
        if (inherits(learners[[1]], "Stack")) {
          # if we were passed a stack (instead of learners as separate
          # parameters), just copy that stack's learners
          learners <- learners[[1]]$params$learners
        } else if (is.list(learners[[1]])) {
          # if we were passed a list of learners (instead of learners as
          # separate parameters), use that list
          learners <- learners[[1]]
        }
      }
      # catch learner names and make unique if there's repetition
      learner_names <- names(learners)
      if (is.null(learner_names)) {
        learner_names <- sapply(learners, `[[`, "name")
      }
      if (any(duplicated(learner_names))) {
        learner_names <- make.unique(learner_names, sep = "_")
      }
      private$.learner_names <- learner_names
      params <- list(learners = learners)

      learners_trained <- sapply(learners, `[[`, "is_trained")

      if (all(learners_trained)) {
        # we've been passed a list of existing fits so we're already fit
        private$.fit_object <- list(
          learner_fits = learners,
          learner_errors = list(),
          is_error = rep(FALSE, length(learners))
        )
        private$.training_task <- learners[[1]]$training_task
      }

      super$initialize(params = params)
    },
    print = function() {
      if (is.null(private$.fit_object)) {
        print(private$.learner_names)
        # lapply(self$params$learners, print)
      } else {
        print(private$.learner_names)
        # lapply(private$.fit_object, print)
      }
    },
    update_errors = function(is_error) {
      private$.fit_object$is_error <- is_error
    }
  ),

  active = list(
    name = function() {
      # learners = self$params$learners
      # learner_names = sapply(learners, function(learner) learner$name)
      # name = paste(learner_names, collapse="x")
      name <- "Stack"
      return(name)
    },
    learner_fits = function() {
      result <- self$fit_object$learner_fits
      return(result)
    }
  ),

  private = list(
    # modified names of learners
    .learner_names = NULL,

    .train_sublearners = function(task) {
      # generate training subtasks
      learners <- self$params$learners
      subtasks <- lapply(learners, function(learner) {
        if (learner$is_trained) {
          return(learner)
        } else {
          delayed_learner <- delayed_learner_train(learner, task)
          delayed_learner$expect_error <- TRUE
          return(delayed_learner)
        }
      })
      return(bundle_delayed(subtasks))
    },
    .train = function(task, trained_sublearners) {
      # check fits for errors
      is_error <- sapply(trained_sublearners, function(result) {
        inherits(result, "error") || inherits(result, "try-error")
      })
      learner_errors <- trained_sublearners[is_error]
      errored_learners <- self$params$learners[is_error]

      for (i in seq_along(errored_learners)) {
        message <- learner_errors[[i]]
        learner <- errored_learners[[i]]
        warning(sprintf(
          "%s failed with message: %s. It will be removed from the stack",
          learner$name, message
        ))
      }
      if (all(is_error)) {
        stop("All learners in stack have failed")
      }

      learner_names <- private$.learner_names[!is_error]
      names(trained_sublearners) <- learner_names

      fit_object <- list(
        learner_fits = trained_sublearners,
        learner_errors = learner_errors, is_error = is_error
      )
      return(fit_object)
    },
    .predict = function(task) {
      is_error <- private$.fit_object$is_error
      learner_fits <- private$.fit_object$learner_fits[!is_error]
      learners <- self$params$learners[!is_error]
      learner_names <- private$.learner_names[!is_error]
      n_to_pred <- task$nrow
      
      # \begin ----- check if hierarchical
      if (length(grep("Lrnr_aggregate", learner_names)) != 0) {
        # if "Lrnr_aggregate" is contained in the library
        # change the corresponding dim and data
        n_to_pred <- length(unique(task$id))
      }
      # \end ----- check if hierarchical
      
      n_learners <- length(learner_names)




      ## Cannot use := to add columns to a null data.table (no columns),
      ## hence we have to first seed an initial column, then delete it later
      learner_preds <- data.table::data.table(
        ..delete = rep(NA, n_to_pred)
      )



      for (i in seq_along(learner_fits)) {
        current_fit <- learner_fits[[i]]

        current_preds <- rep(NA, n_to_pred)
        try({
          current_preds <- current_fit$base_predict(task)
        })


        pred_names <- private$.name_preds(learner_names[i], current_preds)


        set(learner_preds, j = pred_names, value = as.data.table(current_preds))
        invisible(NULL)
      }

      learner_preds$..delete <- NULL

      return(learner_preds)
    },
    .name_preds = function(learner_name, preds) {
      current_names <- learner_name
      if (!is.na(safe_dim(preds)[2]) &&
        safe_dim(preds)[2] > 1) {
        prednames <- colnames(preds)
        if (is.null(prednames)) {
          prednames <- sprintf("col_%03d", seq_len(ncol(preds)))
        }
        current_names <- paste0(current_names, "_", prednames)
        stopifnot(length(current_names) == safe_dim(preds)[2])
      }

      return(current_names)
    }
  )
)
