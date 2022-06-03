#' Generalized Random Forests Learner for CATEs
#'
#' This learner implements the so-called "Causal Forests" estimator of the 
#' conditonal average treatment effect, $\tau{x}$, using the \pkg{grf}
#' package. This is a pluggable package for forest-based statistical estimation
#' and inference. This learner taking A, Y, and X as inputs, estimates some nuisance 
#' components $Pr(A=1|X)$ and $E[Y|X$]$ and fits a "causal forests" object
#'  that can predict CATEs on new Xs. The learners sets parameters to default (detail these)
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
#' Update these accordingly
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

Lrnr_grfcate <- R6Class(
  classname = "Lrnr_grfcate",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function( #X=X,       # these are the inputs to the original function
                           #Y=Y,
                           #W=A,   # stands for treatment
                           Y.hat = NULL,  # this means that nuisance functions will be estimated 
                           W.hat = NULL,  # via a regression forest
                           num.trees = 2000,
                           sample.weights = NULL,
                           clusters = NULL,
                           equalize.cluster.weights = FALSE,
                           sample.fraction = 0.5,
                           mtry = NULL,    #
                           min.node.size = 5,
                           honesty = TRUE,
                           honesty.fraction = 0.5,
                           honesty.prune.leaves = TRUE,
                           alpha = 0.05,
                           imbalance.penalty = 0,
                           stabilize.splits = TRUE,
                           ci.group.size = 2,
                           tune.parameters = "none",   # this could be set to "all" for parameters to be tuned via CV
                           tune.num.trees = 200,
                           tune.num.reps = 50,
                           tune.num.draws = 1000,
                           compute.oob.predictions = TRUE,
                           num.threads = NULL,
                          
                          
                       
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
      args$X <- as.data.frame(task$X[,-1])      ## NK: I am making A part of X as Ivana suggested
      args$W <- task$X[[1]]       ## NK: let's check with Jeremy whether it works this way
      args$Y <- outcome_type$format(task$Y)
      
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }
      
      if (task$has_node("offset")) {
        args$offset <- task$offset
      }
      
      # set mtry arg based on dimensionality of X to match grf::causal_forest
      if (is.null(args$mtry)) {
        args$mtry <- min(ceiling(sqrt(ncol(args$X)) + 20), ncol(args$X))
      }
      
      # train via call_with_args and return fitted object
      fit_object <- call_with_args(grf::causal_forest, args)
      return(fit_object)
    },
    .predict = function(task) {
      # quantiles for which to predict
      #quantiles_pred <- private$.params$quantiles_pred
      
      # generate predictions and output
      predictions_list <- stats::predict(
        private$.fit_object,
        new_data = data.frame(task$X[,-1]),
        #quantiles = quantiles_pred
      )
      predictions <- as.numeric(predictions_list$predictions)
      return(predictions)
    },
    .required_packages = c("grf")
  )
)