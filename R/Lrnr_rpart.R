#' Learner for Recursive Partitioning and Regression Trees
#'
#' This learner uses \code{\link[rpart]{rpart}} from the \pkg{rpart} package to
#' fit recursive partitioning and regression trees.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'  - \code{factor_binary_outcome = TRUE}: Logical indicating whether a binary
#'      outcome should be defined as a factor instead of a numeric. This
#'      only needs to be modified to \code{FALSE} when the user has a binary
#'      outcome and they would like to use the mean squared error (MSE) as the
#'      splitting metric.
#'   - \code{...}: Other parameters to be passed directly to
#'       \code{\link[rpart]{rpart}} (see its documentation for details), and
#'       additional arguments defined in \code{\link{Lrnr_base}}, such as
#'       \code{formula}.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#' rpart_lrnr <- Lrnr_rpart$new()
#' set.seed(693)
#' rpart_fit <- rpart_lrnr$train(task)
Lrnr_rpart <- R6Class(
  classname = "Lrnr_rpart",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(factor_binary_outcome = TRUE,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "categorical", "binomial", "weights"),
    .train = function(task) {
      args <- self$params

      outcome_type <- self$get_outcome_type(task)
      x <- as.matrix(task$X)
      y <- outcome_type$format(task$Y)

      if (outcome_type$type == "binomial" && factor_binary_outcome) {
        y <- factor(y, levels = c(0, 1))
      }

      args$formula <- data.frame(y = y, x)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      # call a function that fits your algorithm
      # with the argument list you constructed
      fit_object <- call_with_args(
        rpart::rpart,
        args,
        other_valid = names(formals(rpart::rpart.control)),
        ignore = "factor_binary_outcome"
      )
      return(fit_object)
    },
    .predict = function(task) {
      predictions <- stats::predict(self$fit_object, newdata = task$X)
      if (private$.training_outcome_type$type == "categorical") {
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("rpart")
  )
)
