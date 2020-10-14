#' Time-specific weighting of prediction losses 
#'
#' A wrapper around any learner that reweights observations. This reweighted 
#' is intended for time series, and allows one to assign weights to learner
#' losses. This learner is particularly useful as an overlay to a metalearner,
#' i.e. the learner that's being wrap in this learner is a metalearner; 
#' because this learner allows one to assign weights to losses, the super 
#' learner can be ensembled in a manner that places more emphasis on recent 
#' losses, and less weight is placed on learner losses further in the past. 
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
#'   \item{\code{window}}{Times within the window are assigned weight of 1, and 
#'   times outside of the window are assigned weight of 0. The window is 
#'   defined with respect to the difference from the maximum time. For example 
#'   if the maximum time (obtained from the task) is 100 and the window is 10, 
#'   then all times 90-100 are assigned weight 1 and times 1-89 are assigned 
#'   weight 0.}
#'   \item{\code{rate}}{A rate of decay to apply to the losses, where the decay 
#'   function is (1-rate)^lag and the lag is the difference from all times to 
#'   the maximum time. The times are obtained from the task's time node.}
#'   \item{\code{delay_decay}}{The amount of time to delay decaying weights, 
#'   and for optional use with \code{rate} argument. The delay decay is 
#'   subtracted from the lags, such that lags less than the delay decay are 0. 
#'   For example, a delay decay of 10 begins the decaying of weights after 10 
#'   time points from the maximum time.}
#'   \item{\code{...}}{Not currently used.}
#' }
#
Lrnr_ts_weights <- R6Class(
  classname = "Lrnr_ts_weights",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner, window = NULL, rate = NULL, 
                          delay_decay = NULL, ...) {
      
      if(!("weights" %in% learner$properties) && 
         !("wrapper" %in% learner$properties)){
        stop(paste0("Learner ", learner$name, " does not support weights."))
      }
      
      if(is.null(rate) && is.null(window)){
        stop(paste0("The window and/or rate must be provided."))
      }
      
      if(is.null(rate) && !is.null(delay_decay)){
        stop(paste0("delay_decay cannot be used without rate."))
      }
      
      params <- args_to_list()
      super$initialize(params = params, ...)
    },

    print = function() {
      print("Lrnr_cv")
      print(self$params$learner)
      # todo: check if fit
    }
  ),

  active = list(
    name = function() {
      paste(self$params$learner$name, "ts", self$params$window,
            self$params$rate, sep = "_")
    }
  ),

  private = list(
    .properties = c("wrapper", "cv"),
    
    .train = function(task, trained_sublearners) {
      verbose <- getOption("sl3.verbose")
      
      weights <- task$weights
      times <- task$time
      if(is.null(times)){
        stop("Task must contain node for times.")
      }
      max_time <- max(times)
      
      
      if(!is.null(self$params$window)){
        window <- max_time-self$params$window
        weights <- weights * ifelse(times <= window, 0, 1)
      }
      
      if(!is.null(self$params$rate)){
        lags <- max_time - times
        if(!is.null(self$params$delay_decay)){
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
