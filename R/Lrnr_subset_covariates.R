#' Learner with Covariate Subsetting
#'
#' This learner provides fitting procedures for subsetting covariates. It is a
#' convenience utility for reducing the number of covariates to be fit.
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
#' @field ... Additional arguments. Currently unused.
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
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

