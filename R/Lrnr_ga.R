#' Nonlinear Optimization via Genetic Algorithm
#'
#' This meta-learner provides fitting procedures for any pairing of loss or risk
#' function and metalearner function, subject to constraints. The optimization
#' problem is solved by making use of \code{\link[Rsolnp]{solnp}}, using
#' Lagrange multipliers. An important note from the \code{\link[Rsolnp]{solnp}}
#' documentation states that the control parameters \code{tol} and \code{delta}
#' are key in getting any possibility of successful convergence, therefore it
#' is suggested that the user change these appropriately to reflect their
#' problem specification. For further details, consult the documentation of the
#' \code{Rsolnp} package.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#'   \item{\code{learner_function=metalearner_linear}}{A function(alpha, X) that
#'     takes a vector of covariates and a matrix of data and combines them into
#'     a vector of predictions. See \link{metalearners} for options.}
#'   \item{\code{eval_function=loss_squared_error}}{A function(pred, truth) that
#'     takes prediction and truth vectors and returns a loss vector or a risk
#'     scalar. See \link{loss_functions} and \link{risk_functions} for options
#'     and more detail.}
#'   \item{\code{make_sparse=TRUE}}{If TRUE, zeros out small alpha values.}
#'   \item{\code{convex_combination=TRUE}}{If \code{TRUE}, constrain alpha to
#'     sum to 1.}
#' }
#'
#' @template common_parameters
#
Lrnr_ga <- R6Class(
  classname = "Lrnr_ga",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner_function = metalearner_linear,
                          eval_function = loss_squared_error,
                          make_sparse = TRUE, convex_combination = TRUE) {
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights", "offset"
    ),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      learner_function <- params$learner_function
      eval_function <- params$eval_function
      outcome_type <- self$get_outcome_type(task)

      # specify data
      X <- as.matrix(task$X)
      Y <- outcome_type$format(task$Y)

      if (task$has_node("offset")) {
        offset <- task$offset
      } else {
        offset <- NULL
      }

      weights <- task$weights
      
      # Borrow the risk code from Lrnr_solnp
      # NB: We enforce convex combination by rescaling inside risk calculation
      # Probably better to use lagrange multipliers
      risk <- function(alphas) {
        if(sum(alphas)==0){
          return(NA)
        }
        alphas <- alphas/sum(alphas)
        if (!is.null(offset)) {
          preds <- learner_function(alphas, X, offset)
        } else {
          preds <- learner_function(alphas, X)
        }
        eval_result <- eval_function(preds, Y)
        
        if (!is.null(attr(eval_result, "risk"))) {
          risk <- eval_result
        } else {
          loss <- eval_result
          risk <- weighted.mean(loss, weights)
        }
        return(risk)
      }
      
      # build a matrix of suggestions
      # first all the discrete SL solutions
      discrete <- diag(p)
      
      # then equal weights
      equal <- rep(1/p,p)
      
      # maybe a nnls for good measure
      nnls_coef <- tryCatch({
        nnls_fit <- nnls(X,Y)
        coef(nnls_fit)
      },error=function(error){
        return(equal)
      })
      
      suggestions <- rbind(discrete, equal,nnls_coef)
      
      # note we flip back to fitness because GA is a maximizer
      GA1 <- ga(type = "real-valued", 
                fitness =  function(x){-1*risk(x)},
                lower = rep(0,p), upper = rep(1,p),
                suggestions = suggestions,
                popSize = 10*p, maxiter = 100, run=10,
                keepBest = TRUE,
                optim = TRUE)
      

      
      
      coefs <- as.vector(GA1@bestSol[[1]])
      names(coefs) <- colnames(task$X)
      
      fit_object <- list(ga_fit <- GA1)
      if (params$make_sparse) {
        max_coef <- max(coefs)
        threshold <- max_coef / 1000
        coefs[coefs < threshold] <- 0
        if (params$convex_combination) {
          # renormalize so coefficients sum to 1
          coefs <- coefs / sum(coefs)
        }
      }
      fit_object$coefficients <- coefs
      fit_object$training_offset <- task$has_node("offset")
      fit_object$name <- "ga"
      return(fit_object)
    },
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- as.matrix(task$X)
      alphas <- self$fit_object$coefficients

      if (self$fit_object$training_offset) {
        predictions <- self$params$learner_function(alphas, X, task$offset)
      } else {
        predictions <- self$params$learner_function(alphas, X)
      }
      return(predictions)
    },
    .required_packages = c("GA")
  )
)
