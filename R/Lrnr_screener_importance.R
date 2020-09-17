#' Variable Importance Screener
#'
#' This learner provides screening of covariates based on the variables sorted
#' in decreasing order of importance, where the importance metric is based on
#' a learner that supports importance.
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
#'   \item{\code{learner}}{An instantiated learner that supports variable
#'   importance.}
#'   \item{\code{num_screen = 5}}{The top number of most important variables
#'   to retain.}
#'   \item{\code{...}}{Other parameters passed to \code{learner}'s
#'   \code{importance} function.}
#' }
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
      learner <- params$learner
      fit <- learner$train(task)
      importance_result <- do.call(fit$importance, importance_args)

      # extract variable names from importance result object
      if (is.null(rownames(importance_result))) {
        if (is.null(names(importance_result))) {
          stop("Importance result missing variable names. Cannot subset covs.")
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
      no_match_covs <- is.na(pmatch(covs, importance_names_sorted))
      if (any(no_match_covs)) {
        # which cov names do not exist in the importance_names_sorted?
        no_match_covs_idx <- which(no_match_covs)
        for (i in 1:length(no_match_covs_idx)) {
          cov_idx <- no_match_covs_idx[i]
          # which importance_names_sorted correspond to one cov
          idx <- grep(covs[cov_idx], importance_names_sorted)
          # rename importance_names_sorted according to true cov name
          importance_names_sorted[idx] <- rep(covs[cov_idx], length(idx))
        }
        importance_names_sorted <- unique(importance_names_sorted)
      }

      # subset to num_screen "most important" covariates
      num_screen <- params$num_screen
      selected <- importance_names_sorted[1:num_screen]
      fit_object <- list(selected = selected)
      return(fit_object)
    },
    .predict = function(task) {
      task$X[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    .required_packages = c()
  )
)
