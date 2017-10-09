## ------------------------------------------------------------------------
## TODO: add tests for 'offset_column' and 'weights_column' with h2o.glm()
## ------------------------------------------------------------------------

#' h2o Model Definition
#'
#' Definition of \code{h2o} type models. This function is for internal use only.
#' This function uploads input data into an \code{h2o.Frame}, allowing the data
#' to be subset to the \code{task$X} \code{data.table} by a smaller set of
#' covariates if spec'ed in params.
#'
#' @param task An object of type \code{Lrnr_base} as defined in this package.
#'
#' @importFrom h2o as.h2o
#'
#' @rdname Lrnr_h2o_glm
#'
#' @name Lrnr_h2o_glm
#'
#' @export
#
define_h2o_X = function(task) {
  op <- options("h2o.use.data.table" = TRUE)
  # op <- options("datatable.verbose" = TRUE, "h2o.use.data.table" = TRUE)
  X <- h2o::as.h2o(task$data[, c(task$nodes$covariates, task$nodes$outcome),
                   with = FALSE, drop = FALSE])
  options(op)
  return(X)
}

#' Faster GLMs with h2o
#'
#' This learner provides faster fitting procedures for generalized linear models
#' by using the \code{h2o} package. Such procedures use the H2O platform to fit
#' GLMs in a computationally efficient manner. For details on the procedure,
#' consult the documentation of the \code{h2o} package.
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
#' @field family A character describing the type of model to be fit in terms of
#'  the error family of the GLM.
#' @field covariates Further variables to used in fitting GLMs with \code{h2o}.
#' @field ... Additional arguments.
#'
#' @importFrom R6 R6Class
#' @importFrom h2o h2o.getConnection h2o.show_progress h2o.no_progress
#' h2o.predict
#' @importFrom data.table as.data.table
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @rdname Lrnr_h2o_glm
#'
#' @name Lrnr_h2o_glm
#'
#' @export
#
Lrnr_h2o_glm <- R6Class(classname = "Lrnr_h2o_glm", inherit = Lrnr_base,
                        portable = TRUE, class = TRUE,
  public = list(
    initialize = function(family = "gaussian", covariates = NULL, ...) {
      if (is.function(family)) {
        family <- family()[["family"]]
      }
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
      if (verbose) {
        h2o::h2o.show_progress()
      } else {
        h2o::h2o.no_progress()
      }

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
      if (verbose) {
        h2o::h2o.show_progress()
      } else {
        h2o::h2o.no_progress()
      }
      X <- define_h2o_X(task)
      predictions <- h2o::h2o.predict(private$.fit_object, X)
      if ("p1" %in% colnames(predictions)) {
        predictions <- predictions[, "p1"]
      } else {
        predictions <- predictions[, "predict"]
      }
      predictions <- data.table::as.data.table(predictions)
      h2o::h2o.show_progress()
      return(predictions)
    },
    .required_packages = c("h2o")
  ),
)

