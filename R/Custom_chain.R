#' Customize chaining for a learner
#'
#' @docType class
#'
#' @family Learners
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction
#' @format \code{\link{R6Class}} object.
#' @field params A list of learners to chain.
#' @section Methods:
#' \describe{
#'   \item{\code{new(...)}}{This method is used to create a pipeline of
#'    learners. Arguments should be indiviual \code{Learner}s, in the order they
#'    should be applied.}
#' }
#
Custom_chain <- R6Class(classname = "Custom_chain",
                        inherit = Lrnr_base,
                        portable = TRUE,
                        class = TRUE,
  public = list(
    initialize = function(learner, chain_fun) {
      params <- args_to_list()
      learner <- params$learner
      if (learner$is_trained) {
        self$set_train(learner, learner$training_task)
      }
      super$initialize(params)
    }
  ),
  private = list(
    .train = function(task) {
      learner <- self$params$learner
      return(learner$base_train(task))
    },
    .predict = function(task) {
      learner_fit <- self$fit_object
      return(learner_fit$base_predict(task))
    },
    .chain = function(task) {
      learner_fit <- self$fit_object
      chain_fun <- self$params$chain_fun
      return(chain_fun(learner_fit, task))
    }
  )
)

#' Modify the chain behavior of a learner
#'
#' This function wraps a learner in such a way that the behavior of
#'  \code{learner$chain} is modified to use a new function definition.
#'  \code{learner$train} and \code{learner$predict} are unaffected.
#'
#' @param learner A \code{sl3} learner to modify.
#' @param chain_fun A function with arguments \code{learner} and \code{task}
#'  that defines the new chain behavior.
#'
#' @rdname Custom_chain
#'
#' @export
#
customize_chain <- function(learner, chain_fun) {
  return(make_learner(Custom_chain, learner, chain_fun))
}

