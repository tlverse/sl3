#' Conversion to Design Matrices
#'
#' Utility for converting \code{data.frame} with raw data columns into a model
#' matrix with columns like interaction terms and factor indicators.
#' TODO: reimplement this without using \code{model.matrix}.
#'
#' @docType class
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field interactions A character or model formula object identifying the exact
#'  interaction terms to be computed in building a design matrix.
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @family Learners
#'
#' @export
#
Lrnr_define_interactions <- R6Class(classname = "Lrnr_define_interactions",
                                    inherit = Lrnr_base,
                                    portable = TRUE,
                                    class = TRUE,
  public = list(
    initialize = function(interactions, ...) {
      params = list(interactions = interactions, ...)
      super$initialize(params = params, ...)
    }),
  private = list(
    .train = function(task) {
      new_task <- task$add_interactions(self$params$interactions)
      interaction_names <- setdiff(new_task$nodes$covariates,
                                   task$nodes$covariates)
      fit_object <- list(interaction_names = interaction_names)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      stop("This learner should be used for chaining only")
    },
    .chain = function(task = NULL) {
      if(!identical(task,private$.training_task)) {
        new_learners = task$add_interactions(self$params$interactions)
      }
      covariates_and_interactions <- unique(c(task$nodes$covariates,
                                              private$.fit_object$interaction_names))
      return(task$next_in_chain(covariates = covariates_and_interactions))
    }
  )
)

