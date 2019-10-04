#' GBM - generalized boosted regression models
#'
#' This learner provides fitting procedures for building generalized boosted
#' regression models, using the function \code{\link[gbm]{gbm}} from the
#' \code{gbm} package.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
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
#'   \item{\code{n.trees}}{Integer specifying the total number of trees to fit.
#'     This is equivalent to the number of iterations and the number of basis
#'     functions in the additive expansion. Default is 10000.
#'   }
#'   \item{\code{interaction.depth}}{Integer specifying the maximum depth of
#'     each tree (i.e.,  the highest level ofvariable interactions allowed).
#'     A value of 1 implies an additive model, a valueof 2 implies a model with
#'     up to 2-way interactions, etc. Default is 2.
#'   }
#'   \item{\code{shrinkage}}{A shrinkage parameter applied to each tree in the
#'     expansion. Also known asthe learning rate or step-size reduction; 0.001
#'     to 0.1 usually work, but a smallerlearning rate typically requires more
#'     trees. Default is 0.001.
#'   }
#'   \item{\code{...}}{Other parameters passed to \code{\link[gbm]{gbm}}.
#'     See its documentation for details.
#'   }
#' }
#
Lrnr_gbm <- R6Class(
  classname = "Lrnr_gbm", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(n.trees = 10000, interaction.depth = 2,
                              shrinkage = 0.001, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial"),

    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      args$x <- task$X
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$w <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      if (outcome_type$type == "continuous") {
        distribution <- "gaussian"
      } else if (outcome_type$type == "binomial") {
        distribution <- "bernoulli"
      } else {
        stop("Unsupported outcome type for Lrnr_gbm.")
      }
      args$distribution <- distribution
      args$verbose <- FALSE

      fit_object <- call_with_args(gbm::gbm.fit, args)
      return(fit_object)
    },

    .predict = function(task) {
      preds <- stats::predict(
        object = private$.fit_object, newdata = task$X,
        n.trees = self$params$n.trees, type = "response"
      )
      return(preds)
    },
    .required_packages = c("gbm")
  )
)
