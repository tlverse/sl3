#' Cross-Validated Selector
#'
#' This learner is the cross-validated (CV) selector, and it is intended
#' for use as the \code{metalearner} in \code{\link{Lrnr_sl}}.
#' \code{Lrnr_cv_selector} selects the candidate with the best CV
#' predictive performance (i.e., lowest CV risk). Specifically,
#' it aims to optimize the CV risk, and it is defined by a constrained
#' weighted combination: the weights can either be zero or one, and they
#' must sum to one. \code{Lrnr_cv_selector} optimizes the CV
#' predictive performance under these constraints by assigning the
#' candidate with the best CV predictive performance a weight of one
#' and all others a weight of zero. Thus, \code{Lrnr_cv_selector}
#' and its predictions will be identical to the best-performing
#' candidate learner and its predictions; this is why we say
#' \code{Lrnr_cv_selector} "selects" the candidate with the best
#' CV predictive performance.
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
#'   - \code{eval_function = loss_squared_error}: A function that takes as input
#'       a vector of predicted values as its first argument and a vector of
#'       observed outcome values as its second argument, and then returns a
#'       vector of losses or a numeric risk. See \link{loss_functions} and
#'       \link{risk_functions} for options.
#'   - \code{folds = NULL}: Optional \pkg{origami}-structured cross-validation
#'       folds from the task for training \code{Lrnr_sl}, e.g.,
#'       \code{task$folds}. This argument is only required and utilized
#'       when \code{eval_function} is not a loss function, since the risk
#'       has to be calculated on each validation set separately and then
#'       averaged across them in order to estimate the cross-validated risk.
#'       This argument is ignored when \code{eval_function} is a loss.
#'
#' @examples
#' \dontrun{
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' hal_lrnr <- Lrnr_hal9001$new(
#'   max_degree = 1, num_knots = c(20, 10), smoothness_orders = 0
#' )
#' lasso_lrnr <- Lrnr_glmnet$new()
#' glm_lrnr <- Lrnr_glm$new()
#' ranger_lrnr <- Lrnr_ranger$new()
#' lrnrs <- c(hal_lrnr, lasso_lrnr, glm_lrnr, ranger_lrnr)
#' names(lrnrs) <- c("hal", "lasso", "glm", "ranger")
#' lrnr_stack <- make_learner(Stack, lrnrs)
#' metalrnr_discrete_MSE <- Lrnr_cv_selector$new(loss_squared_error)
#' discrete_sl <- Lrnr_sl$new(
#'   learners = lrnr_stack, metalearner = metalrnr_discrete_MSE
#' )
#' discrete_sl_fit <- discrete_sl$train(task)
#' discrete_sl_fit$cv_risk(loss_squared_error)
#' }
Lrnr_cv_selector <- R6Class(
  classname = "Lrnr_cv_selector",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(eval_function = loss_squared_error, folds = NULL) {
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),
  active = list(
    coefficients = function() {
      self$assert_trained()
      coefs <- self$fit_object$coef
      return(coefs)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights", "wrapper"
    ),
    .train = function(task) {
      eval_function <- self$params$eval_function
      folds <- self$params$folds

      # specify data and weights
      outcome_type <- self$get_outcome_type(task)
      X <- as.matrix(task$X)
      Y <- outcome_type$format(task$Y)
      weights <- task$weights

      # is evaluation function a risk or loss function?
      loss_function <- TRUE
      optimizer_minimize <- TRUE
      eval_result_test <- eval_function(X[, 1], Y)
      if (!is.null(attr(eval_result_test, "loss")) &&
        !attr(eval_result_test, "loss")) {
        loss_function <- FALSE
        if (!is.null(attr(eval_result_test, "optimize")) &&
          attr(eval_result_test, "optimize") == "maximize") {
          optimizer_minimize <- FALSE
        }
      }

      # to calculate the CV risk, we need folds if evaluation function is risk
      if (is.null(folds) & !loss_function) {
        stop(
          "For evaluation functions that are not loss functions, ",
          "the folds from the task must be provided to Lrnr_cv_selector"
        )
      }

      # function to calculate risk
      risk_fun <- function(pred, obs) {
        eval_result <- eval_function(pred, obs)
        if (!is.null(attr(eval_result, "loss")) &&
          !attr(eval_result, "loss")) {
          risk <- eval_result
        } else {
          loss <- eval_result
          risk <- weighted.mean(loss, weights)
        }
        return(risk)
      }


      # instantiate empty fit object
      fit_object <- list()
      if (!loss_function) {
        validation_sets <- lapply(folds, "[[", "validation_set")
        cv_risk_fun <- function(pred, obs) {
          validation_risks <- lapply(validation_sets, function(v) {
            risk_fun(pred[v], obs[v])
          })
          mean(as.numeric(validation_risks))
        }
      } else {
        cv_risk_fun <- risk_fun
      }
      fit_object$cv_risk <- apply(X, 2, cv_risk_fun, Y)

      coefs <- rep(0L, length(fit_object$cv_risk))
      if (optimizer_minimize) {
        coefs[which.min(fit_object$cv_risk)] <- 1
        fit_object$name <- names(fit_object$cv_risk)[which.min(fit_object$cv_risk)]
      } else {
        coefs[which.max(fit_object$cv_risk)] <- 1
        fit_object$name <- names(fit_object$cv_risk)[which.max(fit_object$cv_risk)]
      }
      names(coefs) <- names(fit_object$cv_risk)
      fit_object$coefficients <- coefs

      return(fit_object)
    },
    .predict = function(task = NULL) {
      X <- as.matrix(task$X)
      predictions <- X[, self$fit_object$name]
      return(predictions)
    }
  )
)
