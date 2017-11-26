#' Define interactions terms
#'
#' This learner adds interactions to its chained task
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
#'     the covariates to create an interaction for.}
#' }
#
Lrnr_define_interactions <- R6Class(
  classname = "Lrnr_define_interactions",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(interactions, ...) {
      params <- list(interactions = interactions, ...)
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("preprocessing"),

    .train = function(task) {
      new_task <- task$add_interactions(self$params$interactions)
      interaction_names <- setdiff(
        new_task$nodes$covariates,
        task$nodes$covariates
      )
      fit_object <- list(interaction_names = interaction_names)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      stop("This learner should be used for chaining only")
    },

    .chain = function(task = NULL) {
      if (!identical(task, private$.training_task)) {
        new_learners <- task$add_interactions(self$params$interactions)
      }
      covariates_and_interactions <-
        unique(c(task$nodes$covariates, private$.fit_object$interaction_names))
      return(task$next_in_chain(covariates = covariates_and_interactions))
    }
  )
)
