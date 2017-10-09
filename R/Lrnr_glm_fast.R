## ------------------------------------------------------------------------
## Faster GLM with speedglm, fall back on glm.fit in case of error
## - Always use the internal fitting function (speedglm.wfit, glm.fit)
## - GLM objects are stripped of all the junk (minimal memory footprint)
## - No formula interface (design mat is the input data.table in task$X)
## - Separate interface for interactions (params[["interactions"]])
## - Can over-ride the covariates with a subset of those in task$nodes$covariates (params[["covariates"]])
## - All predictions are based on external matrix multiplication with a family-based link functions
## ------------------------------------------------------------------------

#if warning is in ignoreWarningList, ignore it; otherwise post it as usual
SuppressGivenWarnings <- function(expr, warningsToIgnore) {
  h <- function (w) {
    if (w$message %in% warningsToIgnore) invokeRestart("muffleWarning")
  }
  withCallingHandlers(expr, warning = h)
}

GetWarningsToSuppress <- function(update.step=FALSE) {
  warnings.to.suppress <- c("glm.fit: fitted probabilities numerically 0 or 1 occurred",
                            "prediction from a rank-deficient fit may be misleading",
                            "non-integer #successes in a binomial glm!",
                            "the matrix is either rank-deficient or indefinite",
                            "glm.fit: algorithm did not converge")
  return(warnings.to.suppress)
}

#' Faster GLM Fits
#'
#' This learner provides faster fitting procedures for generalized linear models
#' by way of a wrapper relying on the \code{speedglm} package.
#'
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field family A \code{family} object from package \code{stats} describing the
#'  error family of the model to be fit. See the documentation for the package
#'  \code{speedglm} for details.
#' @field method The type of matrix decomposition to be used in the model
#'  fitting process. See documentation for the package \code{speedglm} for
#'  further details.
#' @field covariates Extra covariate terms to be passed to the model fitting
#'  process. See documentation of the \code{speedglm} package for details.
#' @field ... Additional arguments.
#'
#' @importFrom R6 R6Class
#' @importFrom data.table data.table
#' @importFrom stats glm.fit
#' @importFrom speedglm speedglm.wfit
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#
Lrnr_glm_fast <- R6Class(classname = "Lrnr_glm_fast", inherit = Lrnr_base,
                         portable = TRUE, class = TRUE,
  public = list(
    initialize = function(family = gaussian(),
                          method = c('Cholesky', 'eigen','qr'),
                          covariates = NULL,
                          ...) {
      params <- list(family = family, method = method[1L],
                     covariates = covariates, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      family <- params[["family"]]
      if (is.character(family)) {
        family <- get(family, mode = "function", envir = parent.frame())
        family <- family()
      }
      family_name <- family[["family"]]
      linkinv_fun <- family[["linkinv"]]
      method <- params[["method"]]
      X <- task$X_intercept

      SuppressGivenWarnings({
        fit_object <- try(speedglm::speedglm.wfit(X = as.matrix(X),
                                                  y = task$Y,
                                                  method = method,
                                                  family = family,
                                                  trace = FALSE,
                                                  weights = task$weights),
                        silent = TRUE)
        }, GetWarningsToSuppress())

      if (inherits(fit_object, "try-error")) {
        # if failed, fall back on stats::glm
        ## TODO: find example where speedglm fails and this runs, add to tests
        if (verbose) {
          message("speedglm::speedglm.wfit failed, falling back on stats:glm.fit; ", fit_object)
        }
        ctrl <- glm.control(trace = FALSE)
        SuppressGivenWarnings({
          fit_object <- stats::glm.fit(x = X,
                                      y = task$Y,
                                      family = family,
                                      control = ctrl,
                                      weights = task$weights)
        }, GetWarningsToSuppress())
        fit_object$linear.predictors <- NULL
        fit_object$weights <- NULL
        fit_object$prior.weights <- NULL
        fit_object$y <- NULL
        fit_object$residuals <- NULL
        fit_object$fitted.values <- NULL
        fit_object$effects <- NULL
        fit_object$qr <- NULL
      }
      fit_object[["linkinv_fun"]] <- linkinv_fun
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- task$X_intercept
      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) {
        coef <- private$.fit_object$coef
        if (!all(is.na(coef))) {
          eta <- as.matrix(X[, which(!is.na(coef)), drop = FALSE,
                           with = FALSE]) %*% coef[!is.na(coef)]
          predictions <- as.vector(private$.fit_object$linkinv_fun(eta))
        }
      }
      return(data.table::data.table(predictions))
    },
    .required_packages = c("speedglm")
), )

