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
#' @param outcome_type An object of type \code{Variable_Tyoe} for use in formatting the outcome
#'
#' @rdname Lrnr_h2o_glm
#'
#' @name Lrnr_h2o_glm
#'
#' @export
#
define_h2o_X = function(task, outcome_type = NULL) {
  op <- options("h2o.use.data.table" = TRUE)
  # op <- options("datatable.verbose" = TRUE, "h2o.use.data.table" = TRUE)
  data <- task$data
  
  if(!is.null(outcome_type)){
    y_formatted <- outcome_type$format(task$Y) 
    set(data, j=task$nodes$outcome, value = y_formatted)
  }
  
  X <- h2o::as.h2o(data)
  
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
    initialize = function(intercept = TRUE,
                     standardize = TRUE,
                     lambda = 0L,
                     max_iterations = 100,
                     ignore_const_cols = FALSE,
                     missing_values_handling = "Skip", ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights", "offset"),
    .train = function(task) {
      verbose = getOption("sl3.verbose")
      args <- self$params

      outcome_type <- self$get_outcome_type(task)
      
      
      if(is.null(args$family)){
        args$family <- outcome_type$glm_family()
      }
      
      
      if(inherits(args$family,"family")){
        args$family <- args$family$family
      }

      connectH2O <- try(h2o::h2o.getConnection(), silent = TRUE)
      if (inherits(connectH2O, "try-error")) {
        stop("No active H2O cluster found, please initiate h2o cluster first by running 'h2o::h2o.init()'")
      }

      h2o_data <- define_h2o_X(task, outcome_type)
      if (verbose) {
        h2o::h2o.show_progress()
      } else {
        h2o::h2o.no_progress()
      }

      args$x <- task$nodes$covariates
      args$y <- task$nodes$outcome
      args$training_frame <- h2o_data

      if(task$has_node("weights")){
        args$weights_column <- task$nodes$weights
      }

      if(task$has_node("offset")){
        args$offset_column <- task$nodes$offset
      }

      fit_object <- call_with_args(h2o::h2o.glm, args)

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

