#' Modify Task
#'
#' This learner modifies its chained task with a custom function
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
#'   \item{\code{interactions}}{A list of lists, with each inner list containing
#'     the covariates from which to create interaction terms.}
#'   \item{\code{warn_on_existing}}{If \code{TRUE}, produce a warning if there
#'     is already a column with a name matching this given interaction term.}
#' }
Lrnr_modify_task <- R6Class(
  classname = "Lrnr_modify_task",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(modify_fun, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("preprocessing"),

    .train = function(task) {
      fit_object <- list("trained")
      return(fit_object)
    },

    .predict = function(task = NULL) {
      stop("This learner should be used for chaining only")
    },

    .chain = function(task = NULL) {
      
      args <- self$params
      modify_fun <- args$modify_fun
      args$modify_fun <- NULL
      args$task <- task
      new_task <- do.call(modify_fun,args)

      return(new_task)
    }
  )
)
