#' Feed-Forward Neural Networks and Multinomial Log-Linear Models
#'
#' This learner provides feed-forward neural networks with a single hidden layer,
#' and for multinomial log-linear models.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for both training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{formula}}{A formula of the form class ~ x1 + x2 + ...}
#'   \item{\code{weights}}{(case) weights for each example – if missing defaults to 1}
#'   \item{\code{size}}{number of units in the hidden layer. Can be zero if there are skip-layer units.}
#'   \item{\code{entropy}}{switch for entropy (= maximum conditional likelihood) fitting. Default by least-squares.}
#'   \item{\code{decay}}{parameter for weight decay. Default 0.}
#'   \item{\code{maxit}}{maximum number of iterations. Default 100.}
#'   \item{\code{linout}}{switch for linear output units. Default logistic output units.}
#'   \item{\code{...}}{Other parameters passed to
#'     \code{\link[nnet]{nnet}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_nnet <- R6Class(
  classname = "Lrnr_nnet",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(size = 5, decay = 0, maxit = 100, linout = FALSE, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),

    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      # specify data
      args$x <- as.data.frame(task$X)
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(nnet::nnet, args, keep_all = TRUE)

      # if (self$params$serializeable) {
      #  invisible(fit_object$fit$state)
      # }

      return(fit_object)
    },

    .predict = function(task) {
      outcome_type <- private$.training_outcome_type

      if (outcome_type$type == "binomial") {
        predictions <- predict(private$.fit_object,
          newdata = data.frame(task$X),
          type = "raw"
        )
      } else {
        predictions <- predict(private$.fit_object,
          newdata = data.frame(task$X),
          type = "raw"
        )
      }

      return(predictions)
    },
    .required_packages = c("nnet")
  )
)
