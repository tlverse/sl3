#' Generalized Random Forests Learner
#'
#' This learner implements Generalized Random Forests, using the \pkg{grf}
#' package. This is a pluggable package for forest-based statistical estimation
#' and inference. GRF currently provides non-parametric methods for
#' least-squares regression, quantile regression, and treatment effect
#' estimation (optionally using instrumental variables). Current implementation
#' trains a regression forest that can be used to estimate quantiles of the
#' conditional distribution of (Y|X=x).
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
#'   \item{\code{X}}{The covariates used in the quantile regression.}
#'   \item{\code{Y}}{The outcome.}
#'   \item{\code{quantiles}}{Vector of quantiles used to calibrate the forest.}
#'   \item{\code{regression.splitting}}{Whether to use regression splits when
#'    growing trees instead of specialized splits based on the quantiles (the
#'    default). Setting this flag to \code{TRUE} corresponds to the approach to
#'    quantile forests from Meinshausen (2006).}
#'   \item{\code{sample.fraction}}{Fraction of the data used to build each tree.
#'    Note: If honesty is used, these subsamples will further be cut in half.}
#'   \item{\code{mtry}}{Number of variables tried for each split.}
#'   \item{\code{num.trees}}{Number of trees grown in the forest.
#'    Note: Getting accurate confidence intervals generally requires more trees
#'    than getting accurate predictions.}
#'   \item{\code{num.threads}}{Number of threads used in training. If set to
#'    \code{NULL}, the software automatically selects an appropriate amount.}
#'   \item{\code{min.node.size}}{A target for the minimum number of observations
#'    in each tree leaf. Note that nodes with size smaller than
#'    \code{min.node.size} can occur, as in the \pkg{randomForest} package.}
#'   \item{\code{honesty}}{Whether or not honest splitting (i.e., sub-sample
#'    splitting) should be used.}
#'   \item{\code{alpha}}{A tuning parameter that controls the maximum imbalance
#'    of a split.}
#'   \item{\code{imbalance.penalty}}{A tuning parameter that controls how
#'    harshly imbalanced splits are penalized.}
#'   \item{\code{seed}}{The seed for the C++ random number generator.}
#'   \item{\code{clusters}}{Vector of integers or factors specifying which
#'    cluster each observation corresponds to.}
#'   \item{\code{samples_per_cluster}}{If sampling by cluster, the number of
#'    observations to be sampled from each cluster. Must be less than the size
#'    of the smallest cluster. If set to \code{NULL}, this value will be set to
#'    the size of the smallest cluster.}
#'   \item{\code{q}}{Vector of quantiles used to predict. This can be different
#'    than the vector of quantiles used for training.}
#' }
#'
#' @template common_parameters
#

Lrnr_grf <- R6Class(
  classname = "Lrnr_grf",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(quantiles = c(0.1, 0.5, 0.9),
                          regression.splitting = FALSE,
                          sample.fraction = 0.5,
                          mtry = NULL,
                          num.trees = 2000,
                          num.threads = NULL,
                          min.node.size = NULL,
                          honesty = TRUE,
                          alpha = 0.05,
                          imbalance.penalty = 0,
                          seed = NULL, 
                          clusters = NULL,
                          samples_per_cluster = NULL,
                          q = 0.5,
                          ...) {
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
      args$Y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(grf::quantile_forest, args)

      return(fit_object)
    },

    .predict = function(task) {
      # outcome_type <- private$.training_outcome_type
      q <- private$.params$q

      predictions <- stats::predict(
        private$.fit_object,
        new_data = data.frame(task$X),
        quantiles = q
      )

      return(predictions)
    },
    .required_packages = c("grf")
  )
)
