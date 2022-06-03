#' Generalized Random Forests Learner for CATEs
#'
#' This learner implements the so-called "Causal Forests" estimator of the 
#' conditonal average treatment effect, $\tau{x}$, using the \pkg{grf}
#' package. This is a pluggable package for forest-based statistical estimation
#' and inference. This learner takes A, Y, and X as inputs. It is currently required that
#' A is the first column on the X matrix.  It required the estimation of the nuisance 
#' components $Pr(A=1|X)$ and $E[Y|X$]$, which as default are estimated using the regression 
#' forest function of the package, without tuning hyperparameters. Itfits a "causal forests" object
#'  that can predict CATEs on new Xs. The learner currently used tuning parameters set to default.
#'  All arguments are listed in initialize(), except for the data input and the mtry argument that are set later. 
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
#' 
#'   \item{\code{X}}{The covariates used. Note: with current defaults,
#'    the same set of covariates are used to estimate nuisance functions
#'     and the CATE. It may be preferable to allow  for V < X,
#'      in which case the nuisance functions need to be estimated in a separate step.}
#'    
#'    \item{\code{Y}}{The outcome (must be a numeric vector with no NAs). }
#'      
#'       '    
#'    \item{\code{W}}{Treatment assignment (must be a binary or real numeric vector with no NAs). }
#'         
#'    \item{\code{Y.hat=NULL}}{Estimates of the expected responses E[Y|Xi], marginalizing over treatment.
#'     If Y.hat = NULL, these are estimated using a separate regression forest.}
#'      
#'   
#'    \item{\code{W.hat=NULL}}{E[W | Xi]. If W.hat = NULL, these are estimated using a separate regression forest.}
#'         
#' 


#'   \item{\code{num.trees = 2000}}{Number of trees grown in the forest. NOTE:
#'    Getting accurate confidence intervals generally requires more trees than
#'    getting accurate predictions.}
#'
#'
#'   \item{\code{sample.weights	=NULL}}{Weights given to each sample in estimation. If NULL, each observation receives the same weight. Note: To avoid introducing confounding, weights should be independent of the potential outcomes given X. Default is NULL.}

#'   

#'   \item{\code{clusters = NULL}}{Vector of integers or factors specifying
#'    which cluster each observation corresponds to.}
#'    
#'   \item{\code{equalize.cluster.weights = FALSE}}{If \code{FALSE}, each unit
#'    is given the same weight (so that bigger clusters get more weight). If
#'    \code{TRUE}, each cluster is given equal weight in the forest. In this
#'    case, during training, each tree uses the same number of observations from
#'    each drawn cluster: If the smallest cluster has K units, then when we
#'    sample a cluster during training, we only give a random K elements of the
#'    cluster to the tree-growing procedure. When estimating average treatment
#'    effects, each observation is given weight 1/cluster size, so that the
#'    total weight of each cluster is the same.}
#'    
#'   \item{\code{sample.fraction = 0.5}}{Fraction of the data used to build each
#'    tree. NOTE: If \code{honesty = TRUE}, these subsamples will further be cut
#'    by a factor of \code{honesty.fraction.}.}
#'    
#'   \item{\code{mtry = NULL}}{Number of variables tried for each split. By
#'    default, this is set based on the dimensionality of the predictors.}
#'   \item{\code{min.node.size = 5}}{A target for the minimum number of
#'    observations in each tree leaf. Note that nodes with size smaller than
#'    \code{min.node.size} can occur, as in the \pkg{randomForest} package.}
#'   \item{\code{honesty = TRUE}}{Whether or not honest splitting (i.e.,
#'    sub-sample splitting) should be used.}
#'  
#'   \item{\code{honesty.fraction = 0.5}}{The fraction of data that will be used for determining splits if honesty = TRUE. Corresponds to set J1 in the notation of the paper. Default is 0.5 (i.e. half of the data is used for determining splits).}
#'  
#'   \item{\code{honesty.prune.leaves = TRUE}}{If TRUE, prunes the estimation sample tree such that no leaves are empty. If FALSE, keep the same tree as determined in the splits sample (if an empty leave is encountered, that tree is skipped and does not contribute to the estimate). Setting this to FALSE may improve performance on small/marginally powered data, but requires more trees (note: tuning does not adjust the number of trees). Only applies if honesty is enabled. Default is TRUE.}
#'
#'  
#'   \item{\code{alpha = 0.05}}{A tuning parameter that controls the maximum
#'    imbalance of a split.}
#'   \item{\code{imbalance.penalty = 0}}{A tuning parameter that controls how
#'    harshly imbalanced splits are penalized.}
#'    
#'    \item{\code{stabilize.splits=TRUE}}{Whether or not the treatment should be taken into account when determining the imbalance of a split.}
#'    
#'    \item{\code{ci.group.size=2}}{The forest will grow ci.group.size trees on each subsample. In order to provide confidence intervals, ci.group.size must be at least 2.}
#'       
#'    \item{\code{tune.parameters="none"}}{A vector of parameter names to tune. If "all": all tunable parameters are tuned by cross-validation. The following parameters are tunable: ("sample.fraction", "mtry", "min.node.size", "honesty.fraction", "honesty.prune.leaves", "alpha", "imbalance.penalty"). If honesty is FALSE the honesty.* parameters are not tuned. Default is "none" (no parameters are tuned).}
#'  
#'    \item{\code{tune.num.trees=200}}{The number of trees in each 'mini forest' used to fit the tuning model.}
#'  
#'    \item{\code{tune.num.reps=50}}{The number of forests used to fit the tuning model.}
#'  
#'    \item{\code{tune.num.draws=1000}}{The number of random parameter values considered when using the model to select the optimal parameters. Default is 1000.}
#'
#'    \item{\code{compute.oob.predictions=TRUE}}{Whether OOB predictions on training set should be precomputed.}
#'    
#'   \item{\code{num.threads = NULL}}{Number of threads used in training. By default, the number of threads is set to the maximum hardware concurrency.}
#'
#'   \item{\code{seed=seed = runif(1, 0, .Machine$integer.max)}}{The seed of the C++ random number generator.}
#'  
#' 
#' }
#'
#' @template common_parameters
#

Lrnr_grfcate <- R6Class(
  classname = "Lrnr_grfcate",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function( #X=X,          # covariates
                           #Y=Y,          # outcomes
                           #W=A,          # treatment
                           Y.hat = NULL,  # nuisance functions will be estimated 
                           W.hat = NULL,  # via  regression forests, using X
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
                           #seed = runif(1, 0, .Machine$integer.max)  # This is set later (is it?)
                          
                       
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
      args$X <- as.data.frame(task$X[,-1])      ##  Requires first column on X covariates to be A
      args$W <- task$X[[1]]                     ##  A (here noted as W) specified
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
        new_data = data.frame(task$X[,-1]),      # First column of X data frame was A hence removed it
        #quantiles = quantiles_pred
      )
      predictions <- as.numeric(predictions_list$predictions)
      return(predictions)
    },
    .required_packages = c("grf")
  )
)