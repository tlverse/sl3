#' Discrete Bayesian Additive Regression Tree sampler
#'
#' This learner implements BART algorithm in C++, using the \code{dbarts} package.
#' BART is a Bayesian sum-of-trees model in which each tree is constrained
#' by a prior to be a weak learner.
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
#'   \item{\code{x.test}}{Explanatory variables for test (out of sample) data.
#'   \code{bart} will generate draws of \eqn{f(x)} for each \eqn{x} which is a row of \code{x.test}.}
#'   \item{\code{sigest}}{For continuous response models, an estimate of the error variance, \eqn{\sigma^2},
#'   used to calibrate an inverse-chi-squared prior used on that parameter. If not supplied,
#'   the least-squares estimate is derived instead. See \code{sigquant} for more information.
#'   Not applicable when \eqn{y} is binary.}
#'   \item{\code{sigdf}}{Degrees of freedom for error variance prior.
#'   Not applicable when \eqn{y} is binary.}
#'   \item{\code{sigquant}}{The quantile of the error variance prior that the rough estimate
#'   (\code{sigest}) is placed at. The closer the quantile is to 1, the more aggresive the fit
#'   will be as you are putting more prior weight on error standard deviations (\eqn{\sigma})
#'   less than the rough estimate. Not applicable when \eqn{y} is binary.}
#'   \item{\code{k}}{For numeric \eqn{y}, \code{k} is the number of prior standard deviations
#'   \eqn{E(Y|x) = f(x)} is away from \eqn{\pm 0.5}{+/- 0.5}. The response (\code{y.train}) is
#'   internally scaled to range from \eqn{-0.5} to \eqn{0.5}. For binary \eqn{y}, \code{k} is
#'   the number of prior standard deviations \eqn{f(x)} is away from \eqn{\pm 3}{+/- 3}.
#'   In both cases, the bigger \eqn{k} is, the more conservative the fitting will be.}
#'   \item{\code{power}}{Power parameter for tree prior.}
#'   \item{\code{base}}{Base parameter for tree prior.}
#'   \item{\code{binaryOffset}}{ sed for binary \eqn{y}. When present, the model is
#'   \eqn{P(Y = 1 \mid x) = \Phi(f(x) + \mathrm{binaryOffset})}{P(Y = 1 | x) = \Phi(f(x) + binaryOffset)},
#'   allowing fits with probabilities shrunk towards values other than \eqn{0.5}.}
#'   \item{\code{weights}}{An optional vector of weights to be used in the fitting process.
#'   When present, BART fits a model with observations \eqn{y \mid x \sim N(f(x),
#'   \sigma^2 / w)}{y | x ~ N(f(x), \sigma^2 / w)}, where \eqn{f(x)} is the unknown function.}
#'   \item{\code{ntree}}{The number of trees in the sum-of-trees formulation.}
#'   \item{\code{ndpost}}{The number of posterior draws after burn in,
#'   \code{ndpost / keepevery} will actually be returned.}
#'   \item{\code{nskip}}{Number of MCMC iterations to be treated as burn in.}
#'   \item{\code{printevery}}{As the MCMC runs, a message is printed every \code{printevery} draws.}
#'   \item{\code{keepevery}}{Every \code{keepevery} draw is kept to be returned to the user.
#'   Useful for \dQuote{thinning} samples.}
#'   \item{\code{keeptrainfits}}{If \code{TRUE} the draws of \eqn{f(x)} for \eqn{x} corresponding
#'   to the rows of \code{x.train} are returned.}
#'   \item{\code{usequants}}{When \code{TRUE}, determine tree decision rules using estimated
#'   quantiles derived from the \code{x.train} variables. When \code{FALSE}, splits are
#'   determined using values equally spaced across the range of a variable. See details for more information.}
#'   \item{\code{numcut}}{The maximum number of possible values used in decision rules (see \code{usequants}, details).
#'   If a single number, it is recycled for all variables; otherwise must be a vector of length
#'   equal to \code{ncol(x.train)}. Fewer rules may be used if a covariate lacks enough unique values.}
#'   \item{\code{printcutoffs}}{The number of cutoff rules to printed to screen before the MCMC is run.
#'   Given a single integer, the same value will be used for all variables. If 0, nothing is printed.}
#'   \item{\code{verbose}}{Logical; if \code{FALSE} supress printing.}
#'   \item{\code{nchain}}{Integer specifying how many independent tree sets and fits should be calculated.}
#'   \item{\code{nthread}}{Integer specifying how many threads to use. Depending on the CPU architecture,
#'   using more than the number of chains can degrade performance for small/medium data sets.
#'   As such some calculations may be executed single threaded regardless.}
#'   \item{\code{combinechains}}{Logical; if \code{TRUE}, samples will be returned in arrays of
#'   dimensions equal to \code{nchain} \eqn{\times} \code{ndpost} \eqn{\times} number of observations.}
#'   \item{\code{keeptrees}}{Logical; must be \code{TRUE} in order to use \code{predict} with the result of a \code{bart} fit.}
#'   \item{\code{keepcall}}{Logical; if \code{FALSE}, returned object will have \code{call} set to
#'   \code{call("NULL")}, otherwise the call used to instantiate BART.}
#' }
#'
#' @template common_parameters
#

Lrnr_dbarts <- R6Class(
  classname = "Lrnr_dbarts",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(ndpost = 500, nskip = 100,
                              ntree = 200L, verbose = FALSE, keeptrees = TRUE, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),

    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      # specify data
      args$x.train <- as.data.frame(task$X)
      args$y.train <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- do.call(dbarts::bart, args)

      return(fit_object)
    },

    .predict = function(task) {
      outcome_type <- private$.training_outcome_type

      if (outcome_type$type == "binomial") {
        predictions <- rowMeans(stats::pnorm(t(stats::predict(
          private$.fit_object,
          data.frame(task$X),
          type = "response"
        ))))
        # predictions<-ifelse(predictions<0.5,0,1)
      } else {
        predictions <- rowMeans(t(stats::predict(
          private$.fit_object,
          data.frame(task$X)
        )))
      }

      return(predictions)
    },
    .required_packages = c("dbarts")
  )
)
