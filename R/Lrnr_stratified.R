#' Stratify learner fits by a single variable
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
#'   \item{\code{learner="learner"}}{An initialized Lrnr_* object.
#'   }
#'   \item{\code{variable_stratify="variable_stratify"}}{\code{character} giving
#'    the variable in the covariates on which to stratify. Supports only
#'    variables with discrete levels coded as \code{numeric}.
#'   }
#'   \item{\code{...}}{Other parameters passed directly to
#'    \code{learner$train}. See its documentation for details.
#'   }
#' }
#
Lrnr_stratified <- R6Class(
  classname = "Lrnr_stratified", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, variable_stratify, ...) {
      # learner is an already initialized learner
      params <- list(
        learner = learner, variable_stratify = variable_stratify,
        ...
      )
      super$initialize(params = params, ...)
    }
  ),
  active = list(
    name = function() {
      name <- paste("strat", self$params$variable_stratify,
        self$params$learner$name,
        sep = "_"
      )
    }
  ),
  private = list(
    .properties = c("continuous", "binomial"),

    .train = function(task) {
      args <- self$params
      args$X <- as.matrix(task$X)
      strata_ids <- unlist(task$data[, args$variable_stratify, with = FALSE])
      variable_stratify_stratas <- unique(strata_ids)

      # fit_object is a dictionary of instantiated of Lrnr_* objects
      fit_object <- list()
      for (strata in variable_stratify_stratas) {
        index_in_strata <- which(strata_ids == strata)
        sub_task <- task$subset_task(
          row_index = index_in_strata,
          drop_folds = TRUE
        )
        # remove the `variable_stratify` from the sub-task
        sub_task <- sub_task$next_in_chain(
          covariates = sub_task$nodes$covariates[
            sub_task$nodes$covariates != args$variable_stratify
          ]
        )
        # assume that `variable_stratify` is a numeric multinomial factor
        fit_object[[as.character(strata)]] <- args$learner$train(sub_task)
      }
      return(fit_object)
    },
    .predict = function(task = NULL) {
      learner_dict <- self$fit_object
      variable_stratify_stratas <- names(learner_dict)
      variable_stratify <- self$params$variable_stratify

      strata_ids <- unlist(task$data[, variable_stratify, with = FALSE])
      variable_stratify_stratas_new <- unique(strata_ids)
      
      if (
        length(
          setdiff(variable_stratify_stratas_new, variable_stratify_stratas)
        ) > 0
      ) {
        stop("There is new strata in the prediction data that is not present in
              training data!")
      }

      prediction_df_dict <- list()
      # predictions <- aorder(results$predictions, order(results$index))

      for (strata in variable_stratify_stratas_new) {
        index_subtask <- which(strata_ids == strata)
        # construct subtask
        sub_task <- task$subset_task(row_index = index_subtask)
        sub_task <- sub_task$next_in_chain(
          covariates = sub_task$nodes$covariates[
            sub_task$nodes$covariates != variable_stratify
          ]
        )
        # predict on the subtask
        prediction_subtask <- learner_fit_predict(
          learner_dict[[as.character(strata)]],
          sub_task
        )
        result <- list(
          prediction = prediction_subtask,
          original_index = index_subtask
        )
        prediction_df_dict[[as.character(strata)]] <- result
      }
      results <- apply(do.call(rbind, prediction_df_dict), 2, as.list)
      results <- origami::combine_results(results)

      predictions <- aorder(results$prediction, order(results$original_index))
      return(predictions)
    },
    .required_packages = NULL
  )
)
