#' Revere (SplitSpecific) Task
#'
#' A task that has different realizations in different folds
#' Useful for Revere CV operations
#'
#' Learners with property "cv" must use these tasks correctly
#'
#' Other learners will treat this as the equivalent of the "full" task.
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
sl3_revere_Task <- R6Class(
  classname = "sl3_revere_Task",
  inherit = sl3_Task,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(generator_fun, task) {
      private$.generator_fun <- generator_fun
      private$.input_task <- task
      private$.task_cache <- new.env()

      ## disabling this in case it masks errors
      # initialize other private members to the contents of full task so naive use of this task will act like full task
      # full_task <- self$revere_fold_task("full")
      # full_task_private <- full_task$.__enclos_env__$private
      # invisible(lapply(
      #   names(full_task_private),
      #   function(name) {
      #     assign(name, full_task_private[[name]], private)
      #   }
      # ))
    },
    revere_fold_task = function(fold_number) {
      fold_task <- get0(as.character(fold_number), envir = self$task_cache, inherits = FALSE)
      if (is.null(fold_task)) {
        fold_task <- self$generator_fun(fold_number, self$input_task)
        assign(as.character(fold_number), fold_task, envir = self$task_cache)
      }

      return(fold_task)
    }
  ),
  active = list(
    task_cache = function() {
      return(private$.task_cache)
    },
    input_task = function() {
      return(private$.input_task)
    },
    generator_fun = function() {
      return(private$.generator_fun)
    },
    folds = function(){
      return(self$input_task$folds)
    }
  ),
  private = list(
    .task_cache = NULL,
    .input_task = NULL,
    .generator_fun = NULL
  )
)
