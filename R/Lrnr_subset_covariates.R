#' Learner with Covariate Subsetting
#'
#' This learner provides fitting procedures for subsetting covariates. It is a
#' convenience utility for reducing the number of covariates to be fit.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#
Lrnr_subset_covariates <- R6Class(classname = "Lrnr_subset_covariates",
                                  inherit = Lrnr_base, portable = TRUE,
                                  class = TRUE,
  public = list(
    initialize = function(...) {
      params = list(...)
      super$initialize(...)
    }
  ),

  private = list(
    .train = function(task) {
      fit_object <- list()
      return(fit_object)
    },

    .predict = function(task = NULL) {
    # nothing to do here: we're relying on Lrnr_base to subset covariates
      return(task$X)
    },

    .chain = function(task = NULL) {
    # nothing to do here: we're relying on Lrnr_base to subset covariates
      return(task)
    }
  )
)

