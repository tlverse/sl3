## ------------------------------------------------------------------------
## Faster GLM with speedglm, fall back on glm.fit in case of error
## - Always use the internal fitting function (speedglm.wfit, glm.fit)
## - GLM objects are stripped of all the junk (minimal memory footprint)
## - No formula interface (design mat is the input data.table in task$X)
## - Separate interface for interactions (params[["interactions"]])
## - Can over-ride the covariates with a subset of those in task$nodes$covariates (params[["covariates"]])
## - All predictions are based on external matrix multiplication with a family-based link functions
## ------------------------------------------------------------------------


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
#' @importFrom stats glm.fit
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#
Lrnr_glm_fast <- R6Class(classname = "Lrnr_glm_fast", inherit = Lrnr_base,
                         portable = TRUE, class = TRUE,
  public = list(
    initialize = function(family = NULL, 
                          method = c('Cholesky', 'eigen','qr'),
                          covariates = NULL,
                          ...) {
      params <- list(family = family, method = method[1L],
                     covariates = covariates, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      outcome_type <- self$get_outcome_type(task)
      
      # use family from parameters first
      family <- params$family
      if (is.character(family)) {
        family <- get(family, mode = "function", envir = parent.frame())
        family <- family()
      } else if(is.null(family)){
        # if family isn't specified, use a family appropriate for the outcome_type
        if(outcome_type=="continuous"){
          family = gaussian()
        } else if(outcome_type=="binomial"){
          family = binomial()
        } else{
          warning("No family specified and untested outcome_type. Defaulting to gaussian")
          family = gaussian()
        }
      }
      params$family <- family
      family_name <- family$family
      linkinv_fun <- family$linkinv

      # generate arguments from params and other sources
      args <- keep_only_fun_args(params, speedglm::speedglm.wfit)
      args$X <- as.matrix(task$X_intercept)
      args$y <- task$Y
      args$trace <- FALSE
      
      if(task$has_node("weights")){
        args$weights <- task$weights
      }
      
      if(task$has_node("offset")){
        args$offset <- task$offset
      }
      
      browser()
      SuppressGivenWarnings({
        fit_object <- try(do.call(speedglm::speedglm.wfit, args),
                        silent = TRUE)
        }, GetWarningsToSuppress())

      if (inherits(fit_object, "try-error")) {
        # if failed, fall back on stats::glm
        ## TODO: find example where speedglm fails and this runs, add to tests
        if (verbose) {
          message("speedglm::speedglm.wfit failed, falling back on stats:glm.fit; ", fit_object)
        }
        args$ctrl <- glm.control(trace = FALSE)
        args$method <- NULL
        SuppressGivenWarnings({
          fit_object <- do.call(stats::glm.fit, args)
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
      fit_object$linkinv_fun <- linkinv_fun
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

