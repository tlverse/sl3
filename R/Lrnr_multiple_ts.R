#' Stratify univariable time-series learners by time-series
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
#'   \item{\code{variable_stratify="variable_stratify"}}{A \code{character}
#'    giving the variable in the covariates on which to stratify. Supports only
#'    variables with discrete levels coded as \code{numeric}.
#'   }
#'   \item{\code{...}}{Other parameters passed directly to
#'    \code{learner$train}. See its documentation for details.
#'   }
#' }
#
Lrnr_multiple_ts <- R6Class(
  classname = "Lrnr_multiple_ts", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, ...) {
      # learner is an already initialized learner
      params <- list(
        learner = learner,
        ...
      )
      super$initialize(params = params, ...)
    },
    create_task = function(task) {
      id <- task$nodes$time
      suppressWarnings(data <- melt(task$data, id = id, variable.name = "series"))

      # Create a time-series task
      task_new <- sl3_Task$new(
        data = data,
        nodes = list(covariates = "value", outcome = "value", id = "series"),
        folds = task$folds
      )

      return(task_new)
    }
  ),
  active = list(
    name = function() {
      name <- paste("stratified_time_series",
        self$params$learner$name,
        sep = "_"
      )
    }
  ),
  private = list(
    .properties = c("continuous", "timeseries"),
    .train = function(task) {
      args <- self$params
      learners <- args$learner

      # TO DO: add task restructure in case in wide format
      # task <- self$create_task(task)

      data <- task$data

      strata_ids <- unlist(task$data[, task$nodes$id, with = FALSE])
      variable_stratify_stratas <- unique(strata_ids)

      # fit_object is a dictionary of instantiated of Lrnr_* objects
      fit_object <- list()
      for (strata in variable_stratify_stratas) {
        index_in_strata <- which(strata_ids == strata)
        sub_task <- task[index_in_strata]
        # data_subset <- data[index_in_strata, ]
        # sub_task <- sl3_Task$new(
        #   data = data_subset,
        #   nodes = task$nodes,
        #   folds = task$folds
        # )

        ### Issue: this changes fold structure...
        # sub_task <- task_new$subset_task(
        #  row_index = index_in_strata,
        #  drop_folds = TRUE
        # )
        # assume that `variable_stratify` is a numeric multinomial factor
        fit_object[[as.character(strata)]] <- args$learner$train(sub_task)
      }
      return(fit_object)
    },
    .predict = function(task = NULL) {
      learner_dict <- self$fit_object
      variable_stratify_stratas <- names(learner_dict)

      # TO DO: add task restructure in case in wide format
      # task <- self$create_task(task)

      data <- task$data

      strata_ids <- unlist(task$data[, task$nodes$id, with = FALSE])
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
        sub_task <- task[index_subtask]
        #
        # data_subset <- data[index_subtask, ]
        # sub_task <- sl3_Task$new(
        #   data = data_subset,
        #   nodes = task$nodes,
        #   folds = task$folds
        # )
        ### Issue: this changes fold structure...
        # sub_task <- task_new$subset_task(row_index = index_subtask)

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
