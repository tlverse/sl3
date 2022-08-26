#' Time-specific weighting of prediction losses
#'
#' A wrapper around any learner that reweights observations. This reweighted
#' is intended for time series, and ultimately assigns weights to losses. This
#' learner is particularly useful as a metalearner wrapper. It can be used to
#' create a time-adaptive ensemble, where a super learner is created in a manner
#' that places more weight (with max weight of 1) on recent losses, and less
#' weight is placed on losses further in the past.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom origami training validation fold_index cross_validate combiner_c
#' @importFrom dplyr %>% group_by summarise_all select
#'
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
#'   \item{\code{learner}}{The learner to wrap}
#'   \item{\code{folds=NULL}}{An \code{origami} folds object. If \code{NULL},
#'    folds from the task are used}
#'   \item{\code{full_fit=FALSE}}{If \code{TRUE}, also fit the underlying
#'   learner on the full data. This can then be accessed with
#'   \code{predict_fold(task, fold_number="full")}}
#'   \item{\code{window}}{Observations corresponding to times outside of the
#'   window are assigned weight of 0, and obervations corresponding to times
#'   within the window are assigned weight of 1. The window is defined with
#'   respect to the difference from the maximum time, where all times are
#'   obtained from the task node for time. For example, if the maximum time is
#'   100 and the window is 10, then obervations corresponding to times 90-100
#'   are assigned weight 1 and obervations for times 1-89 are assigned weight 0.
#'   If \code{rate} is provided with \code{window}, then times within the
#'   window are assigned according to the \code{rate} argument (and potentially
#'   \code{delay_decay}), and the times outside of the window are still
#'   assigned weight of 0.}
#'   \item{\code{rate}}{A rate of decay to apply to the losses, where the decay
#'   function is (1-rate)^lag and the lag is the difference from all times to
#'   the maximum time.}
#'   \item{\code{delay_decay}}{The amount of time to delay decaying weights,
#'   for optional use with \code{rate} argument. The delay decay is subtracted
#'   from the lags, such that lags less than the delay decay have lag of 0 and
#'   thus weight of 1. For example, a delay decay of 10 assigns weight 1 to
#'   observations that are no more than 10 time points away from the maximum
#'   time; and for observations that are more than 10 time points away from the
#'   maximum time, the weight is assigned according to the decay function.
#'   In this example, observations corresponding to 11 time points away from the
#'   maximum time would be assigned lag=1, 11-10, when setting the weights
#'   with respect to (1-rate)^lag.}
#'   \item{\code{...}}{Not currently used.}
#' }
#' 
#' @examples 
#' library(data.table)
#' 
#' # Load data
#' data(bsds)
#' bsds <- bsds[1:500, ]
#' data <- as.data.table(bsds)
#' data[, time := .I]
#' 
#' # Create sl3 Task
#' outcome <- "cnt"
#' covars <- c("registered", "temp", "windspeed", "hum", "weekday")
#' node_list <- list(outcome = outcome, time = "time", covariates = covars)
#' folds <- make_folds(
#'   data[1:400, ],
#'   fold_fun = folds_rolling_window, 
#'   window_size = 50,
#'   validation_size = 50, 
#'   gap = 0, 
#'   batch = 50
#' )
#' training_task <- sl3_Task$new(data[1:400, ], nodes = node_list, folds = folds)
#' folds <- make_folds(
#'   data[401:500, ],
#'   fold_fun = folds_rolling_window, 
#'   window_size = 50,
#'   validation_size = 50, 
#'   gap = 0, 
#'   batch = 50
#' )
#' forecast_task <- sl3_Task$new(data[401:500, ], nodes = node_list, folds = folds)
#' 
#' # Create learners, train, and get predictions
#' meta_learner <- Lrnr_solnp$new(metalearner_linear, loss_squared_error)
#' window1_meta_learner <- Lrnr_ts_weights$new(meta_learner, window = 1)
#' window40_meta_learner <- Lrnr_ts_weights$new(meta_learner, window = 40)
#' decay_meta_learner <- Lrnr_ts_weights$new(meta_learner, rate = 0.05)
#' delay_decay_meta_learner <- Lrnr_ts_weights$new(
#'   meta_learner, 
#'   rate = 0.05, 
#'   delay_decay = 40
#' )
#' 
#' glm_learner <- Lrnr_glm$new()
#' mean_learner <- Lrnr_mean$new()
#' earth_learner <- Lrnr_earth$new()
#' stacked_learners <- make_learner(Stack, glm_learner, mean_learner, earth_learner)
#' window1_sl_learner <- Lrnr_sl$new(stacked_learners, window1_meta_learner)
#' window40_sl_learner <- Lrnr_sl$new(stacked_learners, window40_meta_learner)
#' decay_sl_learner <- Lrnr_sl$new(stacked_learners, decay_meta_learner)
#' delay_decay_sl_learner <- Lrnr_sl$new(stacked_learners, delay_decay_meta_learner)
#' 
#' window1_fit <- window1_sl_learner$train(training_task)
#' window1_pred <- window1_fit$predict(forecast_task)
#' 
#' window40_fit <- window40_sl_learner$train(training_task)
#' window40_pred <- window40_fit$predict(forecast_task)
#' 
#' decay_fit <- decay_sl_learner$train(training_task)
#' decay_pred <- decay_fit$predict(forecast_task)
#' 
#' delay_decay_fit <- delay_decay_sl_learner$train(training_task)
#' delay_decay_pred <- delay_decay_fit$predict(forecast_task)
Lrnr_ts_weights <- R6Class(
  classname = "Lrnr_ts_weights",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner, window = NULL, rate = NULL,
                          delay_decay = NULL, ...) {
      if (!("weights" %in% learner$properties) &&
        !("wrapper" %in% learner$properties)) {
        stop(paste0("Learner ", learner$name, " does not support weights."))
      }

      if (is.null(rate) && is.null(window)) {
        stop(paste0("The window and/or rate must be provided."))
      }

      if (is.null(rate) && !is.null(delay_decay)) {
        stop(paste0("delay_decay cannot be used without rate."))
      }

      params <- list(
        learner = learner, window = window, rate = rate,
        delay_decay = delay_decay, ...
      )
      super$initialize(params = params, ...)
    }
  ),
  active = list(
    name = function() {
      name <- paste(self$params$learner$name,
        "ts", self$params$window, self$params$rate,
        sep = "_"
      )
      params <- self$params
      if (!is.null(params$name)) {
        name <- params$name
      } else {
        window <- ifelse(is.null(params$window), "NULL", params$window)
        rate <- ifelse(is.null(params$rate), "NULL", params$rate)
        delay <- ifelse(is.null(params$delay_decay), "NULL", params$delay_decay)
        name <- paste(
          class(self)[1], window, rate, delay, params$learner$name,
          sep = "_"
        )
      }
    }
  ),
  private = list(
    .properties = c("wrapper", "cv"),
    .train = function(task) {

      # ensure task contains node for time
      if (!("time" %in% names(task$nodes))) {
        stop("Task must contain node for times.")
      }

      # retain relevant nodes from task
      weights <- task$weights
      times <- task$time
      max_time <- max(times)

      # assign weights according to window
      if (!is.null(self$params$window)) {
        window <- max_time - self$params$window
        weights <- weights * ifelse(times <= window, 0, 1)
      }

      # assign weights according to rate of decay
      if (!is.null(self$params$rate)) {
        lags <- max_time - times
        if (!is.null(self$params$delay_decay)) {
          lags_delayed <- lags - self$params$delay_decay
          lags <- ifelse(lags_delayed < 0, 0, lags_delayed)
        }
        weights <- weights * (1 - self$params$rate)^lags
      }

      # update task to include weights
      cols <- task$add_columns(data.table(ts_weight = weights))
      wt_task <- task$next_in_chain(column_names = cols, weights = "ts_weight")

      # train learner on task with weights
      learner <- self$params$learner
      fit_object <- self$params$learner$train(wt_task)

      return(fit_object)
    },
    .predict = function(task) {
      self$fit_object$predict(task)
    }
  )
)
