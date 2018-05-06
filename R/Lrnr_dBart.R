#' Discrete Bayesian Additive Regression Tree sampler
#'
#' This learner implements BART algorithm in C++, using the \code{dbarts} package.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'   \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{Y}}{Outcome variable}
#'   \item{\code{X}}{Covariate dataframe}
#'   \item{\code{test}}{An optional matrix or data frame with the same number of
#'   predictors as data, or formula in backwards compatibility mode. If column names
#'   are present, a matching algorithm is used.}
#'   \item{\code{subset}}{An optional vector specifying a subset of observations to
#'   be used in the fitting process.}
#'   \item{\code{weights}}{An optional vector of weights to be used in the fitting
#'   process. When present, BART fits a model with observations y | x ~ N(f(x), σ^2 / w),
#'   where f(x) is the unknown function.}
#'   \item{\code{offset}}{An optional vector specifying an offset from 0 for the
#'   relationship between the underyling function, f(x), and the response y.
#'   Only is useful for binary responses, in which case the model fit is to assume
#'   P(Y = 1 | X = x) = Φ(f(x) + offset), where Φ is the standard normal cumulative
#'   distribution function.}
#'   \item{\code{offset.test}}{The equivalent of offset for test observations.
#'   Will attempt to use offset when applicable.}
#'   \item{\code{verbose}}{A logical determining if additional output is printed to
#'   the console. See dbartsControl.}
#'   \item{\code{n.samples}}{A positive integer setting the default number of
#'   posterior samples to be returned for each run of the sampler.
#'   Can be overriden at run-time. See dbartsControl.}
#'   \item{\code{tree.prior}}{An expression of the form cgm or cgm(power, base)
#'   setting the tree prior used in fitting.}
#'   \item{\code{node.prior}}{An expression of the form normal or normal(k) that
#'   sets the prior used on the averages within nodes.}
#'   \item{\code{resid.prior}}{An expression of the form chisq or chisq(df, quant)
#'   that sets the prior used on the residual/error variance.}
#'   \item{\code{control}}{An object inheriting from dbartsControl, created by the dbartsControl function.}
#'   \item{\code{sigma}}{A positive numeric estimate of the residual standard deviation.
#'   If NA, a linear model is used with all of the predictors to obtain one.}
#' }
#'
#' @template common_parameters
#

Lrnr_dBart <- R6Class(
  classname = "Lrnr_dBart",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(offset.test = offset, verbose = FALSE, n.samples = 800L,
                              tree.prior = cgm, ode.prior = normal, resid.prior = chisq,
                              control = dbartsControl(), sigma = NA_real_, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),

    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      # specify data
      args$X <- as.data.frame(task$X)
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(dbarts::bart, args)

      return(fit_object)
    },

    .predict = function(task) {
      # outcome_type <- private$.training_outcome_type
      predictions <- stats::predict(
        private$.fit_object,
        new_data = data.frame(task$X)
      )

      return(predictions)
    },
    .required_packages = c("rJava", "dbart")
  )
)
