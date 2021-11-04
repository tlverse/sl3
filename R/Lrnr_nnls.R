#' Non-negative Linear Least Squares
#'
#' This learner provides fitting procedures for models via non-negative linear
#' least squares regression, using \pkg{nnls} package's
#' \code{\link[nnls]{nnls}} function.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table data.table
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
#'   - \code{convex = FALSE}: Normalize the coefficients to be a convex
#'       combination.
#'   - \code{...}: Other parameters passed to \code{\link[nnls]{nnls}}.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' lrnr_nnls <- make_learner(Lrnr_nnls)
#' nnls_fit <- lrnr_nnls$train(task)
#' nnls_preds <- nnls_fit$predict()
#'
#' # NNLS is commonly used as a metalearner in a super learner (i.e., Lrnr_sl)
#' lrnr_glm <- make_learner(Lrnr_glm)
#' lrnr_glmnet <- Lrnr_glmnet$new()
#' lrnr_mean <- Lrnr_mean$new()
#' learners <- c(lrnr_glm, lrnr_glmnet, lrnr_mean)
#' names(learners) <- c("glm", "lasso", "mean") # optional, renaming learners
#' simple_learner_stack <- make_learner(Stack, learners)
#' sl <- Lrnr_sl$new(learners = simple_learner_stack, metalearner = lrnr_nnls)
#' sl_fit <- sl$train(task)
#' sl_preds <- sl_fit$predict()
Lrnr_nnls <- R6Class(
  classname = "Lrnr_nnls", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(convex = FALSE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    print = function() {
      print(self$name)
      print(self$fits)
    }
  ),
  active = list(
    fits = function() {
      fit_object <- private$.fit_object
      if (!is.null(fit_object)) {
        data.table::data.table(lrnrs = fit_object$lrnrs, weights = fit_object$x)
      } else {
        data.table::data.table(lrnrs = character(0), weights = numeric(0))
      }
    }
  ),
  private = list(
    .properties = c("continuous", "binomial"),
    .train = function(task) {
      args <- self$params
      x <- task$X
      y <- task$Y
      fit_object <- nnls::nnls(as.matrix(x), y)
      fit_object$lrnrs <- names(task$X)
      init_coef <- coefficients(fit_object)
      init_coef[is.na(init_coef)] <- 0
      if (sum(init_coef) == 0) {
        warning("All algorithms have zero weight")
      }

      if (args$convex == TRUE & sum(init_coef) > 0) {
        coef <- init_coef / sum(init_coef)
      } else {
        coef <- init_coef
      }

      fit_object$coefficients <- coef
      fit_object$x <- coef
      return(fit_object)
    },
    .predict = function(task = NULL) {
      if (sum(coef(private$.fit_object)) == 0) {
        warning("All predictions will be equal to 0, since all nnls coefficients are 0")
      }
      predictions <- as.matrix(task$X) %*% coef(private$.fit_object)
      return(predictions)
    },
    .required_packages = c("nnls")
  ),
)
