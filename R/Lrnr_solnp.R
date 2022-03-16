#' Nonlinear Optimization via Augmented Lagrange
#'
#' This meta-learner provides fitting procedures for any pairing of loss or risk
#' function and metalearner function, subject to constraints. The optimization
#' problem is solved by making use of \code{\link[Rsolnp]{solnp}}, using
#' Lagrange multipliers. An important note from the \code{\link[Rsolnp]{solnp}}
#' documentation states that the control parameters \code{tol} and \code{delta}
#' are key in getting any possibility of successful convergence, therefore it
#' is suggested that the user change these appropriately to reflect their
#' problem specification. For further details, consult the documentation of the
#' \pkg{Rsolnp} package.
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
#'   - \code{init_0 = FALSE}: If \code{TRUE}, alpha is initialized to all 0's,
#'       useful for TMLE. Otherwise, it is initialized to equal weights summing
#'       to 1, useful for Super Learner.
#'   - \code{rho = 1}: This is used as a penalty weighting scaler for
#'       infeasibility in the augmented objective function. The higher its
#'       value the more the weighting to bring the solution into the feasible
#'       region (default 1). However, very high values might lead to numerical
#'       ill conditioning or significantly slow down convergence.
#'   - \code{outer.iter = 400}: Maximum number of major (outer) iterations.
#'   - \code{inner.iter = 800}: Maximum number of minor (inner) iterations.
#'   - \code{delta = 1e-7}:Relative step size in forward difference evaluation.
#'   - \code{tol = 1e-8}: Relative tolerance on feasibility and optimality.
#'   - \code{trace = FALSE}: The value of the objective function and the
#'       parameters are printed at every major iteration.
#'   - \code{...}: Additional arguments defined in \code{\link{Lrnr_base}},
#'     such as \code{params} (like \code{formula}) and \code{name}.
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
#' # instantiate SL with solnp metalearner
#' solnp_meta <- Lrnr_solnp$new()
#' sl <- Lrnr_sl$new(lrnr_stack, solnp_meta)
#' sl_fit <- sl$train(task)
Lrnr_solnp <- R6Class(
  classname = "Lrnr_solnp",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner_function = metalearner_linear,
                          eval_function = loss_squared_error,
                          make_sparse = TRUE, convex_combination = TRUE,
                          init_0 = FALSE, rho = 1, outer.iter = 400,
                          inner.iter = 800, delta = 1e-7, tol = 1e-8,
                          trace = FALSE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
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
      risk <- function(alphas) {
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
      if (params$convex_combination) {
        eq_fun <- function(alphas) {
          sum(alphas)
        }
        eqB <- 1
        LB <- rep(0L, ncol(task$X))
      } else {
        eq_fun <- NULL
        eqB <- NULL
        LB <- NULL
      }
      p <- ncol(X)

      if (params$init_0) {
        init_alphas <- rep(0, p)
      } else {
        init_alphas <- rep(1 / p, p)
      }
      fit_object <- Rsolnp::solnp(
        init_alphas, risk,
        eqfun = eq_fun, eqB = eqB,
        LB = LB,
        control = list(
          outer.iter = params$outer.iter, inner.iter = params$inner.iter,
          delta = params$delta, tol = params$tol, trace = params$trace,
          rho = params$rho
        )
      )
      coefs <- fit_object$pars
      names(coefs) <- colnames(task$X)

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
      fit_object$name <- "solnp"
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
    .required_packages = c("Rsolnp")
  )
)
