#' Define interactions terms
#'
#' This learner adds interactions to its chained task. Intended for use in a
#' Pipeline, defining a coupling of the interactions with the learner.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table data.table
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'   - \code{interactions}: A list whose elements are a character
#'     vector of covariates from which to create interaction terms.
#'   - \code{warn_on_existing}: If \code{TRUE}, produce a warning if there
#'     is already a column with a name matching this given interaction term.
#'
#' @examples
#' data(cpp_imputed)
#' covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
#' outcome <- "haz"
#' task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
#' interactions <- list(c("apgar1", "parity"), c("apgar5", "parity"))
#' lrnr_interact <- Lrnr_define_interactions$new(
#'   list(c("apgar1", "parity"), c("apgar5", "parity"))
#' )
#' lrnr_glm <- Lrnr_glm$new()
#' interaction_pipeline_glm <- make_learner(Pipeline, lrnr_interact, lrnr_glm)
#' fit <- interaction_pipeline_glm$train(task)
Lrnr_define_interactions <- R6Class(
  classname = "Lrnr_define_interactions",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(interactions, warn_on_existing = TRUE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("preprocessing"),
    .train = function(task) {
      fit_object <- list(interaction_names = self$params$interactions)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      stop("This learner should be used for chaining only")
    },
    .chain = function(task = NULL) {
      new_task <- task$add_interactions(
        self$params$interactions,
        self$params$warn_on_existing
      )

      return(new_task)
    }
  )
)
