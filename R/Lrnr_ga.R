#' Nonlinear Optimization via Genetic Algorithm (GA)
#'
#' This metalearner provides fitting procedures for any pairing of loss or risk
#' function and metalearner function, subject to constraints. The optimization
#' problem is solved by making use of the \code{\link[GA]{ga}} function in the
#' \pkg{GA} R package. For further consult the documentation of this package.
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
#'   - \code{learner_function = metalearner_linear}: A function(alpha, X) that
#'       takes a vector of covariates and a matrix of data and combines them
#'       into a vector of predictions. See \code{\link{metalearners}} for
#'       options.
#'   - \code{eval_function = loss_squared_error}: A function(pred, truth) that
#'       takes prediction and truth vectors and returns a loss vector or a risk
#'       scalar. See \code{\link{loss_functions}} and
#'       \code{\link{risk_functions}} for options and more detail.
#'   - \code{make_sparse = TRUE}: If \code{TRUE}, zeros out small alpha values.
#'   - \code{convex_combination = TRUE}: If \code{TRUE}, constrain alpha to sum
#'       to 1.
#'   - \code{maxiter = 100}: The maximum number of iterations to run before the
#'       GA search is halted.
#'   - \code{run = 10}: The number of consecutive generations without any
#'       improvement in the best fitness value before the GA is stopped.
#'   - \code{optim = TRUE}: A logical determining whether or not a local search
#'       using general-purpose optimization algorithms should be used. Argument
#'       \code{optimArgs} of \code{\link[GA]{ga}} provides further details and
#'       finer control.
#'   - \code{...}: Additional arguments to \code{\link[GA]{ga}} and/or
#'       \code{\link{Lrnr_base}}.
#'
#' @examples
#' # define ML task
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # build relatively fast learner library (not recommended for real analysis)
#' lasso_lrnr <- Lrnr_glmnet$new()
#' glm_lrnr <- Lrnr_glm$new()
#' ranger_lrnr <- Lrnr_ranger$new()
#' lrnrs <- c(lasso_lrnr, glm_lrnr, ranger_lrnr)
#' names(lrnrs) <- c("lasso", "glm", "ranger")
#' lrnr_stack <- make_learner(Stack, lrnrs)
#'
#' # instantiate SL with GA metalearner
#' ga <- Lrnr_ga$new()
#' sl <- Lrnr_sl$new(lrnr_stack, ga)
#' sl_fit <- sl$train(task)
Lrnr_ga <- R6Class(
  classname = "Lrnr_ga",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner_function = metalearner_linear,
                          eval_function = loss_squared_error,
                          make_sparse = TRUE, convex_combination = TRUE,
                          maxiter = 100, run = 10, optim = TRUE, ...) {
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
        if (sum(alphas) == 0) {
          return(NA)
        }
        alphas <- alphas / sum(alphas)
        if (!is.null(offset)) {
          preds <- learner_function(alphas, X, offset)
        } else {
          preds <- learner_function(alphas, X)
        }
        eval_result <- eval_function(preds, Y)

        if (!is.null(attr(eval_result, "loss")) && !attr(eval_result, "loss")) {
          risk <- eval_result
        } else {
          loss <- eval_result
          risk <- weighted.mean(loss, weights)
        }
        if (!is.null(attr(eval_result, "optimize")) &&
          attr(eval_result, "optimize") == "maximize") {
          risk <- risk * -1
        }
        return(risk)
      }

      # build a matrix of suggestions
      p <- ncol(X)
      discrete <- diag(p) # first all the discrete SL solutions
      equal <- rep(1 / p, p) # then equal weights

      # maybe a nnls for good measure
      nnls_coef <- tryCatch(
        {
          nnls_fit <- nnls(X, Y)
          coef(nnls_fit)
        },
        error = function(error) {
          return(equal)
        }
      )

      suggestions <- rbind(discrete, equal, nnls_coef)

      # note we flip back to fitness because GA is a maximizer
      args <- c(list(
        type = "real-valued", fitness = function(x) {
          -1 * risk(x)
        },
        lower = rep(0, p), upper = rep(1, p), suggestions = suggestions,
        popSize = 10 * p, keepBest = TRUE
      ), params)
      GA1 <- call_with_args(
        GA::ga, args,
        ignore = c(
          "learner_function", "eval_function", "make_sparse",
          "convex_combination"
        )
      )

      coefs <- as.vector(GA1@bestSol[[1]])
      names(coefs) <- colnames(task$X)

      fit_object <- list(ga_fit <- GA1)
      if (params$make_sparse) {
        max_coef <- max(coefs)
        threshold <- max_coef / 1000
        coefs[coefs < threshold] <- 0
      }
      if (params$convex_combination) {
        # renormalize so coefficients sum to 1
        coefs <- coefs / sum(coefs)
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
