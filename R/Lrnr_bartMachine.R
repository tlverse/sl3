#' BART Machine Learner
#'
#' This learner implements Bayesian Additive Regression Trees, using the 
#' \code{bartMachine} packahe
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
#'   \item{\code{Y}}{Outcome variable.}
#'   \item{\code{X}}{Covariate dataframe.}
#'   \item{\code{newX}}{Optional dataframe to predict the outcome.}
#'   \item{\code{obsWeights}}{Optional observation-level weights (supported but not tested).}
#'   \item{\code{id}}{Optional id to group observations from the same unit (not used
#'   currently).}
#'   \item{\code{family}}{"gaussian" for regression, "binomial" for binary classification.}
#'   \item{\code{num_trees }}{The number of trees to be grown in the sum-of-trees model.}
#'   \item{\code{num_burn_in}}{Number of MCMC samples to be discarded as "burn-in".}
#'   \item{\code{num_iterations_after_burn_in}}{Number of MCMC samples to draw from the
#'   posterior distribution of f(x).}
#'   \item{\code{alpha}}{Base hyperparameter in tree prior for whether a node is
#'   nonterminal or not.}
#'   \item{\code{beta}}{Power hyperparameter in tree prior for whether a node is
#'   nonterminal or not.}
#'   \item{\code{k}}{For regression, k determines the prior probability that E(Y|X) is
#'   contained in the interval (y_{min}, y_{max}), based on a normal
#'   distribution. For example, when k=2, the prior probability is 95\%. For
#'   classification, k determines the prior probability that E(Y|X) is between
#'   (-3,3). Note that a larger value of k results in more shrinkage and a more
#'   conservative fit.}
#'   \item{\code{q}}{Quantile of the prior on the error variance at which the data-based
#'   estimate is placed. Note that the larger the value of q, the more
#'   aggressive the fit as you are placing more prior weight on values lower
#'   than the data-based estimate. Not used for classification.}
#'   \item{\code{nu}}{Degrees of freedom for the inverse chi^2 prior. Not used for
#'   classification.}
#'   \item{\code{verbose }}{Prints information about progress of the algorithm to the
#'   screen.}
#'   
#' }
#'
#' @template common_parameters
#

Lrnr_bartMachine <- R6Class(
  classname = "Lrnr_bartMachine",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(num_trees = 50, num_burn_in = 250, verbose = F,
                          alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3,
                          num_iterations_after_burn_in = 1000, 
                          prob_rule_class = 0.5, ...) {
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
      
      fit_object <- call_with_args(bartMachine::bartMachine, args)
      
      return(fit_object)
    },
    
    .predict = function(task) {
      #outcome_type <- private$.training_outcome_type
      predictions <- stats::predict(
        private$.fit_object,
        new_data = data.frame(task$X))
      
      return(predictions)
    },
    .required_packages = c("rJava", "bartMachine")
  )
)