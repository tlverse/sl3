#' Conditional Density Estimation
#'
#' This learner provides facilities for conditional density estimation using
#' the \code{condensier} package.
#'
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#' prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field bin_method The type of smoothing to be performed. See documentation of
#'  the \code{condensier} package for details.
#' @field nbins The number of observations per bin. See documentation of the
#'  \code{condensier} package for details.
#' @field max_n_cat Maximum number of unique levels for categorical outcomes.
#'  See documentation of the \code{condensier} package for details.
#' @field pool Whether pooling of data across bins should be performed. See
#'  documentation of the \code{condensier} package for details.
#' @field max_n_bin Maximum number of observations per single bin for continuous
#'  outcome. See documentation of the \code{condensier} package for details.
#' @field parfit Whether to invoke parallelization in the fitting procedure. See
#'  documentation of the \code{condensier} package for details.
#' @field bin_estimator The classification algorithm to be used in the fitting
#'  process. See documentation of the \code{condensier} package for details.
#' @field intrvls An interval range to be used for custom bin definitions. See
#'  documentation of the \code{condensier} package for details.
#'
#' @importFrom R6 R6Class
#'
#' @family Learners
#'
#' @export
#
Lrnr_condensier <- R6Class(classname = "Lrnr_condensier", inherit = Lrnr_base,
                           portable = TRUE, class = TRUE,
  public = list(
    initialize = function(bin_method = c("equal.mass", "equal.len", "dhist"),
                          nbins = 5,
                          max_n_cat = 20,
                          pool = FALSE,
                          max_n_bin = NA_integer_,
                          parfit = FALSE,
                          bin_estimator = Lrnr_glm_fast$new(family = binomial()),
                          intrvls = NULL,
                          ...) {
      params <- args_to_list()
      assert_that(is(bin_estimator, "Lrnr_base") || is(bin_estimator,
                                                       "logisfitR6"))

      ## Perform additional injection if bin_estimator is a learner from sl3.
      ## Wrap sl3 learner object into special wrapper class that
      ## provides a communication link between the two packages.
      if (inherits(bin_estimator,"Lrnr_base")) {
        params$bin_estimator <- Lrnr_pkg_condensier_logisfitR6$new(sl3_lrnr = bin_estimator)
      }

      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("density", "continuous"),
    .covariates = NULL,
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      
      args <- self$params
      args$X = task$nodes$covariates
      args$Y = task$nodes$outcome
      args$input_data = task$data
      
      fit_object <- call_with_args(condensier::fit_density, args)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose = getOption("sl3.verbose")
      predictions <- condensier::predict_probability(private$.fit_object, task$data)
      # sampled_value <- condensier::sample_value(private$.fit_object, task$data)
      # predictions <- data.table::data.table(likelihood = predictions, sampled_value = sampled_value)
      predictions <- data.table::data.table(likelihood = predictions)
      return(predictions)
    },
    .required_packages = c("condensier")
), )

