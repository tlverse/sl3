#' Cross-Validated Selector
#'
#' This meta-learner identifies the cross-validated selector (i.e., discrete
#' super learner) for any loss function.
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
#'   - \code{eval_function}: A function that takes a vector of predictions as
#'     it's first argument, and a vector of truths/observations as it's second
#'     argument, and then returns a vector of losses or a numeric risk. See
#'     \link{loss_functions} and \link{risk_functions} for options.
#'
#' @examples
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
#' discrete_sl_fit$cv_risk
Lrnr_cv_selector <- R6Class(
  classname = "Lrnr_cv_selector",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(eval_function = loss_squared_error) {
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights", "wrapper"
    ),
    .train = function(task) {
      eval_function <- self$params$eval_function

      # specify data
      outcome_type <- self$get_outcome_type(task)
      X <- as.matrix(task$X)
      Y <- outcome_type$format(task$Y)

      weights <- task$weights

      risk <- function(preds) {
        eval_result <- eval_function(preds, Y)
        if (!is.null(attr(eval_result, "risk"))) {
          risk <- eval_result
        } else {
          loss <- eval_result
          risk <- weighted.mean(loss, weights)
        }
        return(risk)
      }
      risks <- apply(X, 2, risk)

      fit_object <- list()
      fit_object$name <- colnames(task$X)[which.min(risks)]
      coef <- risks
      coef[which.min(risks)] <- 1
      coef[-(which.min(risks))] <- 0
      fit_object$coefficients <- as.numeric(coef)

      return(fit_object)
    },
    .predict = function(task = NULL) {
      X <- as.matrix(task$X)
      predictions <- X[, self$fit_object$name]
      return(predictions)
    }
  )
)
