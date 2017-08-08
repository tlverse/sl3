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
    if (w$message %in% warningsToIgnore) invokeRestart( "muffleWarning" )
  }
  withCallingHandlers(expr, warning = h )
}

GetWarningsToSuppress <- function(update.step=FALSE) {
  warnings.to.suppress <- c("glm.fit: fitted probabilities numerically 0 or 1 occurred",
                            "prediction from a rank-deficient fit may be misleading",
                            "non-integer #successes in a binomial glm!",
                            "the matrix is either rank-deficient or indefinite")
  if (update.step) {
    warnings.to.suppress <- c(warnings.to.suppress, "glm.fit: algorithm did not converge")
  }
  return(warnings.to.suppress)
}

## -----------------------------------------------------------------------------
## Add columsn with interactions (by reference) to input design matrix (data.table). Used for training / predicting.
## The function purposefully returns nothing (NULL), since the input data.table is being modified by REFERENCE.
## -----------------------------------------------------------------------------
add_interactions_toDT <- function(XmatDT, interactions) {
  prod.matrix <- function(x) {
    y <- x[,1]
    for(i in 2:dim(x)[2])
      y <- y*x[,i]
    return(y)
  }

  prod.DT <- function(x) {
    y <- x[[1]]
    for(i in 2:ncol(x))
      y <- y*x[[i]]
    return(y)
  }

  for (i in seq_along(interactions)) {
    interact <- interactions[[i]]
    name <- names(interactions)[i]
    if (is.null(name)) name <- paste0(interact, collapse = "_")
    if (all(interact %in% names(XmatDT)))
      XmatDT[, (name) := prod.DT(.SD), .SD = interact]
  }

  return(invisible(NULL))
}

## Define the design matrix for GLM regression. Returns a data.table.
## Allows to subset the taks$X data.table by a smaller set of covariates if spec'ed in params
## Can define interaction columns if spec'ed in params
defineX <- function(task, params) {
  covariates <- task$nodes$covariates
  if ("covariates" %in% names(params)) {
    covariates <- intersect(covariates, params$covariates)
  }
  X <- cbind(Intercept = 1L, task$X[,covariates, with=FALSE, drop=FALSE])
  if (!is.null(params[["interactions"]])) {
    print("adding interactions in GLM:")
    str(params[["interactions"]])
    ## this is a hack to fix pointer allocation problem (so that X can be modified by reference inside add_interactions_toDT())
    ## see this for more: http://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
    ## and this: http://stackoverflow.com/questions/36434717/adding-column-to-nested-r-data-table-by-reference
    data.table::setDF(X)
    data.table::setDT(X)
    add_interactions_toDT(X, params[["interactions"]])
  }
  return(X)
}

#' @importFrom assertthat assert_that is.count is.flag
#' @export
fastGLM_Learner <- R6Class(classname = "fastGLM_Learner", inherit = Learner, portable = TRUE, class = TRUE, private = list(

  .train = function(task) {
    params <- self$params
    if ("family" %in% names(params)) {
      family <- params[["family"]]
      if (is.character(family)) {
        family <- get(family, mode = "function", envir = parent.frame())
        family <- family()
      }
      if (!class(family) %in% "family") stop("family arg must be of an object of class 'family'")
    } else {
      family <- stats::gaussian()
    }
    family_name <- family[["family"]]
    linkinv_fun <- family[["linkinv"]]

    if ("method" %in% names(params)) {
      method <- params[["method"]]
    } else {
      method <- 'Cholesky'
    }

    X <- defineX(task, params)

    SuppressGivenWarnings({
      fit_object <- try(speedglm::speedglm.wfit(X = as.matrix(X),
                                               y = task$Y,
                                               method = method,
                                               family = family,
                                               trace = FALSE,
                                               weights = task$weights),
                      silent = TRUE)
      }, GetWarningsToSuppress())

    if (inherits(fit_object, "try-error")) { # if failed, fall back on stats::glm
        # if (gvars$verbose)
        message("speedglm::speedglm.wfit failed, falling back on stats:glm.fit; ", fit_object)
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
    X <- defineX(task, self$params)
    predictions <- rep.int(NA, nrow(X))
    if (nrow(X) > 0) {
      coef <- private$.fit_object$coef
      if (!all(is.na(coef))) {
        eta <- as.matrix(X[, which(!is.na(coef)), drop = FALSE, with = FALSE]) %*% coef[!is.na(coef)]
        predictions <- as.vector(private$.fit_object$linkinv_fun(eta))
      }
    }
    return(predictions)
  }
), )