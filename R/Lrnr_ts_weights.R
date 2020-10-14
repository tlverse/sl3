#' Time specific weights
#'
#' A wrapper around any learner that reweights observations
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
#'   \item{\code{full_fit=FALSE}}{If \code{TRUE}, also fit the underlying learner on the full data.
#'   This can then be accessed with predict_fold(task, fold_number="full")
#'   }
#' }
#
Lrnr_ts_weights <- R6Class(
  classname = "Lrnr_ts_weights",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner, window = NULL, rate = NULL, ...) {
      params <- list(learner = learner, window = window, rate = rate, ...)
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
      name <- paste(self$params$learner$name, 
                    "ts",self$params$window,self$params$rate,
                    sep = "_")
    }
  ),

  private = list(
    .properties = c("wrapper", "cv"),

  
    .train = function(task, trained_sublearners) {
      verbose <- getOption("sl3.verbose")
      weights <- task$weights
      times <- task$time
      max_time <- max(times)
      
      if(!is.null(self$params$window)){
        weights <- weights * ifelse(times<(max_time-self$params$window),0,1)
      }
      
      if(!is.null(self$params$rate)){
        lag <- max_time - times
        weights <- weights * (1-self$params$rate)^lag
      }
      column_names <- task$add_columns(data.table(ts_weight=weights))
      wt_task <- task$next_in_chain(column_names=column_names, weights = "ts_weight")
      learner <- self$params$learner
      
      fit_object <- learner$train(wt_task)
      
      return(fit_object)
    },

    .predict = function(task) {
      self$fit_object$predict(task)
    },

    .required_packages = c("origami")
  )
)
