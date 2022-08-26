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
#' 
#' @examples 
#' library(data.table)
#' library(origami)
#' library(R6)
#' library(delayed)
#' library(dplyr)
#' 
#' set.seed(49753)
#' 
#' # Simulate simple AR(2) process
#' data <- matrix(arima.sim(model = list(ar = c(.9, -.2)), n = 200))
#' id <- c(rep("Series_1", 50), rep("Series_2", 50), rep("Series_3", 50), rep("Series_4", 50))
#' 
#' data <- data.frame(data)
#' data$id <- as.factor(id)
#' 
#' data <- data %>%
#'   group_by(id) %>%
#'   dplyr::mutate(time = 1:n())
#' 
#' data$W1 <- rbinom(200, 1, 0.6)
#' data$W2 <- rbinom(200, 1, 0.2)
#' 
#' data <- as.data.table(data)
#' 
#' folds <- origami::make_folds(
#'   data,
#'   t = max(data$time),
#'   id = data$id,
#'   time = data$time,
#'   fold_fun = folds_rolling_window_pooled,
#'   window_size = 20,
#'   validation_size = 15,
#'   gap = 0,
#'   batch = 10
#' )
#' 
#' # Create sl3 Task
#' task <- sl3_Task$new(
#'   data = data, outcome = "data",
#'   time = "time", id = "id",
#'   covariates = c("W1", "W2"),
#'   folds = folds
#' )
#' 
#' train_task <- training(task, fold = task$folds[[1]])
#' valid_task <- validation(task, fold = task$folds[[1]])
#' 
#' # Create learner, train, and get predictions
#' arima_learner <- Lrnr_arima$new()
#' multiple_ts_arima_learner <- Lrnr_multiple_ts$new(arima_learner)
#' multiple_ts_arima_fit <- multiple_ts_arima_learner$train(train_task)
#' multiple_ts_arima_pred <- multiple_ts_arima_fit$predict(valid_task)
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
