#' Variable Importance Screener
#'
#' This learner screens covariates based on their variable importance, where the
#' importance values are obtained from the \code{learner}. Any learner with an
#' \code{importance} method can be used. The set of learners with support for
#' \code{importance} can be found with \code{sl3_list_learners("importance")}.
#' Like all other screeners, this learner is intended for use in a
#' \code{\link{Pipeline}}, so the output from this learner (i.e., the selected
#' covariates) can be used as input for the next learner in the pipeline.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#'  - \code{learner}: An instantiated learner that supports variable importance.
#'      The set of learners with this support can be obtained via
#'      \code{sl3_list_learners("importance")}.
#'  - \code{num_screen = 5}: The top n number of "most impotant" variables to
#'      retain.
#'  - \code{...}: Other parameters passed to \code{learner}'s \code{importance}
#'      function.
#'
#' @examples
#'
Lrnr_screener_importance <- R6Class(
  classname = "Lrnr_screener_importance",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, num_screen = 5, ...) {
      if (!("importance" %in% learner$properties)) {
        stop(paste0(
          "No importance support for ", learner$name,
          ". Check learner properties for 'importance'."
        ))
      }

      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("screener"),

    .train = function(task) {
      params <- self$params

      # isolate importance-specific arguments
      not_importance_args <- c("num_screen", "learner")
      importance_args <- params[-which(names(params) %in% not_importance_args)]

      # calculate variable importance
      fit <- params$learner$train(task)
      importance_result <- fit$importance(importance_args)

      # extract variable names from importance result object
      if (is.null(rownames(importance_result))) {
        if (is.null(names(importance_result))) {
          stop("Cannot find covariate names in importance result.")
        } else {
          importance_names_sorted <- names(importance_result)
        }
      } else {
        importance_names_sorted <- rownames(importance_result)
      }

      # rename categorical covs that were discretized & given level-based names
      # e.g., cov "color" was one-hot encoded and renamed as "color_blue",
      # "color_green", "color_red", so we change all three back to "color"
      covs <- task$nodes$covariates
      matched_covs <- match(covs, importance_names_sorted)
      if (any(is.na(matched_covs))) {
        # which cov names do not exist in the importance_names_sorted?
        unmatched_covs <- covs[is.na(matched_covs)]
        for (i in 1:length(unmatched_covs)) {
          # which importance_names_sorted correspond to one cov
          idx <- grep(unmatched_covs[i], importance_names_sorted)
          # rename importance_names_sorted according to true cov name
          importance_names_sorted[idx] <- rep(unmatched_covs[i], length(idx))
        }
        importance_names_sorted <- unique(importance_names_sorted)
      }

      # subset to num_screen "most important" covariates
      selected <- importance_names_sorted[1:params$num_screen]
      fit_object <- list(selected = selected)
      return(fit_object)
    },
    .predict = function(task) {
      task$data[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    .required_packages = c()
  )
)
