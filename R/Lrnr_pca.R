#' Principal Components Analysis
#'
#' This learner provides facilities for performing principal components analysis
#' (PCA) to reduce the dimensionality of a data set to a pre-specified value.
#' For further details, consult the documentation of \code{prcomp} from the core
#' package \code{stats}. This learner object is primarily intended for use with
#' other learners as part of a pre-processing pipeline.
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
#'   \item{\code{n_comp}}{A \code{numeric} value indicating the number of
#'     components to be produced as a result of the PCA dimensionality
#'     reduction. For convenience, this defaults to two (2) components.}
#'   \item{\code{center}}{A \code{logical} value indicating whether the input
#'     data matrix should be centered before performing PCA. This defaults to
#'     \code{TRUE} since that is the recommended practice. Consider consulting
#'     the documentation of \code{prcomp} for details.}
#'   \item{\code{scale}}{A \code{logical} value indicating whether the input
#'     data matrix should be scaled (to unit variance) before performing PCA.
#      This defaults to \code{TRUE} since that is the recommended practice.
#'     Consider consulting the documentation of \code{prcomp} for details.}
#'   \item{\code{...}}{Other optional parameters to be passed to \code{prcomp}.
#'     Consider consulting the documentation of \code{prcomp} for details.}
#' }
#'
#' @template common_parameters
#
Lrnr_pca <- R6Class(
  classname = "Lrnr_pca",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(n_comp = 2, center = TRUE, scale = TRUE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "preprocessing"),

    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      fit_args <- self$params
      fit_args$x <- task$X
      fit_args <- fit_args[names(fit_args) != "n_comp"]
      fit_object <- call_with_args(prcomp, fit_args)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      dim_args <- self$params[names(self$params) == "n_comp"]
      preds_pca <- private$.fit_object$x[, seq_len(unlist(dim_args))]
      predictions <- as.data.table(preds_pca)
      return(predictions)
    },
    .required_packages = c("stats", "data.table")
  )
)

