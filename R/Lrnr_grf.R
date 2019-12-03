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
#'   \item{\code{num.trees = 2000}}{Number of trees grown in the forest. NOTE:
#'    Getting accurate confidence intervals generally requires more trees than
#'    getting accurate predictions.}
#'   \item{\code{quantiles = c(0.1, 0.5, 0.9)}}{Vector of quantiles used to
#'    calibrate the forest.}
#'   \item{\code{regression.splitting = FALSE}}{Whether to use regression splits
#'    when growing trees instead of specialized splits based on the quantiles
#'    (the default). Setting this flag to \code{TRUE} corresponds to the
#'    approach to quantile forests from Meinshausen (2006).}
#'   \item{\code{clusters = NULL}}{Vector of integers or factors specifying
#'    which cluster each observation corresponds to.}
#'   \item{\code{equalize.cluster.weights = FALSE}}{If \code{FALSE}, each unit
#'    is given the same weight (so that bigger clusters get more weight). If
#'    \code{TRUE}, each cluster is given equal weight in the forest. In this
#'    case, during training, each tree uses the same number of observations from
#'    each drawn cluster: If the smallest cluster has K units, then when we
#'    sample a cluster during training, we only give a random K elements of the
#'    cluster to the tree-growing procedure. When estimating average treatment
#'    effects, each observation is given weight 1/cluster size, so that the
#'    total weight of each cluster is the same.}
#'   \item{\code{sample.fraction = 0.5}}{Fraction of the data used to build each
#'    tree. NOTE: If \code{honesty = TRUE}, these subsamples will further be cut
#'    by a factor of \code{honesty.fraction.}.}
#'   \item{\code{mtry = NULL}}{Number of variables tried for each split. By
#'    default, this is set based on the dimensionality of the predictors.}
#'   \item{\code{min.node.size = 5}}{A target for the minimum number of
#'    observations in each tree leaf. Note that nodes with size smaller than
#'    \code{min.node.size} can occur, as in the \pkg{randomForest} package.}
#'   \item{\code{honesty = TRUE}}{Whether or not honest splitting (i.e.,
#'    sub-sample splitting) should be used.}
#'   \item{\code{alpha = 0.05}}{A tuning parameter that controls the maximum
#'    imbalance of a split.}
#'   \item{\code{imbalance.penalty = 0}}{A tuning parameter that controls how
#'    harshly imbalanced splits are penalized.}
#'   \item{\code{num.threads = 1}}{Number of threads used in training. If set to
#'    \code{NULL}, the software automatically selects an appropriate amount.}
#'   \item{\code{quantiles_pred}}{Vector of quantiles used to predict. This can
#'    be different than the vector of quantiles used for training.}
#' }
#'
#' @template common_parameters
#

Lrnr_grf <- R6Class(
  classname = "Lrnr_grf",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(num.trees = 2000,
                          quantiles = c(0.1, 0.5, 0.9),
                          regression.splitting = FALSE,
                          clusters = NULL,
                          equalize.cluster.weights = FALSE,
                          sample.fraction = 0.5,
                          mtry = NULL,
                          min.node.size = 5,
                          honesty = TRUE,
                          alpha = 0.05,
                          imbalance.penalty = 0,
                          num.threads = 1,
                          quantiles_pred = 0.5,
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

      # set mtry arg based on dimensionality of X to match grf::quantile_forest
      if (is.null(args$mtry)) {
        args$mtry <- min(ceiling(sqrt(ncol(args$X)) + 20), ncol(args$X))
      }

      # train via call_with_args and return fitted object
      fit_object <- call_with_args(grf::quantile_forest, args)
      return(fit_object)
    },

    .predict = function(task) {
      # quantiles for which to predict
      quantiles_pred <- private$.params$quantiles_pred

      # generate predictions and output
      predictions <- stats::predict(
        private$.fit_object,
        new_data = data.frame(task$X),
        quantiles = quantiles_pred
      )
      return(predictions)
    },
    .required_packages = c("grf")
  )
)
