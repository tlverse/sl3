#' stratify fit by one variable
#'
#' @docType class
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
#'   \item{\code{lrnr="lrnr"}}{ An initialized Lrnr object.
#'   }
#'   \item{\code{variable_stratify="variable_stratify"}}{ character of the
#'    variable in `covariates` that need to be stratified on. Only support
#'    numerical factors
#'   }
#'   \item{\code{...}}{ Other parameters passed directly to
#'    \code{lrnr$train}. See its documentation for details.
#'   }
#' }
#
Lrnr_stratified <- R6Class(
  classname = "Lrnr_stratified", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(lrnr, variable_stratify, ...) {
      # lrnr is an already initialized learner
      params <- list(lrnr = lrnr, variable_stratify = variable_stratify, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial"),

    .train = function(task) {
      args <- self$params
      args$X <- as.matrix(task$X)
      variable_stratify_stratas <- unique(args$X[, args$variable_stratify])

      # fit_object is a dict of Lrnr
      fit_object <- list()
      for (strata in variable_stratify_stratas) {
        index_in_strata <- which(args$X[, args$variable_stratify] == strata)
        sub_task <- task$subset_task(row_index = index_in_strata)
        # remove the `variable_stratify` from the sub task
        sub_task <- sub_task$next_in_chain(
          covariates = sub_task$nodes$covariates[
              sub_task$nodes$covariates != args$variable_stratify
            ]
        )
        # WILSON: I assume the `variable_stratify` is a numeric multinomial
        # factor. since there is no dict object in R
        fit_object[[as.character(strata)]] <- args$lrnr$train(sub_task)
      }
      return(fit_object)
    },
    .predict = function(task = NULL) {
      lrnr_dict <- self$fit_object
      variable_stratify_stratas <- as.numeric(names(lrnr_dict))
      variable_stratify <- self$params$variable_stratify

      X_new <- as.matrix(task$X)
      variable_stratify_stratas_new <- unique(X_new[, variable_stratify])
      if (
        any(
          sort(variable_stratify_stratas_new) != sort(variable_stratify_stratas)
        )
      ) {
        stop("There is new strata in the prdiction data that is not present in
          training data!")
      }

      prediction_df_dict <- list()
      for (strata in variable_stratify_stratas) {
        index_subtask <- which(X_new[, variable_stratify] == strata)
        # construct subtask
        sub_task <- task$subset_task(row_index = index_subtask)
        sub_task <- sub_task$next_in_chain(
          covariates = sub_task$nodes$covariates[
              sub_task$nodes$covariates != variable_stratify
            ]
        )
        # predict on the subtask
        prediction_subtask <- learner_fit_predict(
            lrnr_dict[[as.character(strata)]],
            sub_task
          )
        prediction_df <- data.frame(prediction = prediction_subtask,
                                    original_index = index_subtask)
        prediction_df_dict[[as.character(strata)]] <- prediction_df
      }
      prediction_df_dict <- do.call(rbind, prediction_df_dict)
      # sort by original row index
      prediction_df_dict <- prediction_df_dict[
        order(prediction_df_dict$original_index),
      ]
      return(prediction_df_dict$prediction)
    },
    # WILSON: how can we access the field of the sub learner?
    .required_packages = c("hal9001")
  )
)
