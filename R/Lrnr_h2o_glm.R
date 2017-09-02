## ------------------------------------------------------------------------
## Faster GLM with h2o
## todo: add tests for 'offset_column' and 'weights_column' with h2o.glm()
## ------------------------------------------------------------------------

## take a list of args, take a function body and return only the args that belong to function signature
keep_only_fun_args <- function(Args, fun) {
  keepArgs <- intersect(names(Args), names(formals(fun))) # captures optional arguments given by user
  if (length(keepArgs) > 0) {
    Args <- Args[keepArgs]
  } else {
    Args <- NULL
  }
  return(Args)
}

## Replace any arg in mainArgs if it also appears in userArgs
## Add any arg from userArgs that also appears in formals(fun) of function
replace_add_user_args <- function(mainArgs, userArgs, fun) {
  replaceArgs <- intersect(names(mainArgs), names(userArgs)) # captures main arguments that were overridden by user
  if(length(replaceArgs) > 0) {
    mainArgs[replaceArgs] <- userArgs[replaceArgs]
    userArgs[replaceArgs] <- NULL
  }
  newArgs <- intersect(names(formals(fun)), names(userArgs)) # captures any additional args given by user that are not in mainArgs
  if (length(newArgs) > 0) {
    mainArgs <- c(mainArgs, userArgs[newArgs])
  }
  return(mainArgs)
}

## Upload input data as h2o.Frame
## Allows to subset the taks$X data.table by a smaller set of covariates if spec'ed in params
define_h2o_X = function(task) {
  op <- options("h2o.use.data.table"=TRUE) # op <- options("datatable.verbose"=TRUE, "h2o.use.data.table"=TRUE)
  X <- h2o::as.h2o(task$data[,c(task$nodes$covariates, task$nodes$outcome), with=FALSE, drop=FALSE])
  options(op)
  return(X)
}

#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_h2o_glm <- R6Class(classname = "Lrnr_h2o_glm", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(family = "gaussian", covariates = NULL, ...) {
      if (is.function(family)) family <- family()[["family"]]
      params <- list(family = family, covariates = covariates, ...)
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .train = function(task) {
      verbose = getOption("sl3.verbose")
      params <- self$params

      connectH2O <- try(h2o::h2o.getConnection(), silent = TRUE)
      if (inherits(connectH2O, "try-error")) {
        stop("No active H2O cluster found, please initiate h2o cluster first by running 'h2o::h2o.init()'")
      }

      X <- define_h2o_X(task)
      if (verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()

      mainArgs <- list(x = task$nodes$covariates,
                       y = task$nodes$outcome,
                       training_frame = X,
                       intercept = TRUE,
                       standardize = TRUE,
                       lambda = 0L,
                       max_iterations = 100,
                       ignore_const_cols = FALSE,
                       missing_values_handling = "Skip")

      mainArgs <- replace_add_user_args(mainArgs, params, fun = h2o::h2o.glm)
      fit_object <- do.call(h2o::h2o.glm, mainArgs)

      h2o::h2o.show_progress()
      ## assign the fitted coefficients in correct order (same as predictor order in x)
      ## NOT USED FOR NOW
      # out_coef <- vector(mode = "numeric", length = length(x)+1)
      # out_coef[] <- NA
      # names(out_coef) <- c("Intercept", x)
      # out_coef[names(fit_object@model$coefficients)] <- fit_object@model$coefficients
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose = getOption("sl3.verbose")
      if (verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()
      X <- define_h2o_X(task)
      predictions <- h2o::h2o.predict(private$.fit_object, X)
      if ("p1" %in% colnames(predictions)) {
        predictions <- predictions[,"p1"]
      } else {
        predictions <- predictions[,"predict"]
      }
      predictions <- data.table::as.data.table(predictions)
      h2o::h2o.show_progress()
      return(predictions)
    },
    .required_packages = c("h2o")
), )