#' Grouped Time-Series Forecasting
#'
#' This learner supports prediction using grouped time-series modeling, using
#' \pkg{hts}. Fitting is done with \code{\link[hts]{hts}} and prediction is
#' performed via \code{\link[hts]{forecast.gts}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
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
#'   \item{\code{groups}}{Group matrix indicating the group structure, with one
#'     column for each series when completely disaggregated, and one row for
#'     each grouping of the time series. It allows either a numerical matrix or
#'     a matrix consisting of strings that can be used for labelling.}
#'   \item{\code{h)}}{Forecast horizon. If \code{NULL}, will be guessed from
#'     the frequency of the time series.}
#'   \item{\code{method)}}{Method for distributing forecasts within hierarchy.
#'     See details of \code{\link[hts]{forecast.gts}}.}
#'   \item{\code{weights)}}{Weights used for "optimal combination" method:
#'     \code{weights="ols"} uses an unweighted combination (as described in
#'     Hyndman et al 2011); \code{weights="wls"} uses weights based on forecast
#'     variances (as described in Hyndman et al 2015); \code{weights="mint"}
#'     uses a full covariance estimate to determine the weights (as described
#'     in Hyndman et al 2016); \code{weights="nseries"} uses weights based on
#'     the number of series aggregated at each node.}
#'   \item{\code{fmethod)}}{Forecasting method to use for each series.}
#'   \item{\code{algorithms)}}{An algorithm to be used for computing the
#'     combination forecasts (when \code{method=="comb"}). The combination
#'     forecasts are based on an ill-conditioned regression model. "lu"
#'     indicates LU decomposition is used; "cg" indicates a conjugate gradient
#'     method; "chol" corresponds to a Cholesky decomposition; "recursive"
#'     indicates the recursive hierarchical algorithm of Hyndman et al (2015);
#'     "slm" uses sparse linear regression. Note that \code{algorithms =
#'     "recursive"} and \code{algorithms = "slm"} cannot be used if
#'     \code{weights="mint"}.}
#'   \item{\code{covariance)}}{Type of the covariance matrix to be used with
#'     \code{weights="mint"}: either a shrinkage estimator ("shr") with
#'     shrinkage towards the diagonal; or a sample covariance matrix ("sam").}
#'   \item{\code{keep.fitted)}}{If \code{TRUE}, keep fitted values at the bottom
#'     level.}
#'   \item{\code{keep.resid)}}{If \code{TRUE}, keep residuals at the bottom
#'     level.}
#'   \item{\code{positive)}}{If \code{TRUE}, forecasts are forced to be strictly
#'     positive (by setting \code{lambda=0}).}
#'   \item{\code{lambda)}}{Box-Cox transformation parameter.}
#'   \item{\code{level}}{Level used for "middle-out" method (only used when
#'     \code{method = "mo"}).}
#'   \item{\code{parallel}}{If \code{TRUE}, import \pkg{parallel} to allow
#'     parallel processing.}
#'   \item{\code{num.cores}}{If \code{parallel = TRUE}, specify how many cores
#'     are going to be used.}
#' }
#'
Lrnr_gts <- R6Class(
  classname = "Lrnr_gts",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(groups,
                          h = NULL,
                          method = "comb",
                          weights = "wls",
                          fmethod = "ets",
                          algorithms = "lu",
                          covariance = "shr",
                          keep.fitted = FALSE,
                          keep.resid = FALSE,
                          positive = FALSE,
                          lambda = NULL,
                          level = NULL,
                          parallel = FALSE,
                          num.cores = 1,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("timeseries", "continuous"),
    .train = function(task) {
      args <- self$params
      args$y <- ts(task$X)
      fit_object <- call_with_args(gts, args)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      args <- self$params
      if (is.null(args$h)) {
        args$h <- ifelse(frequency(args$object$bts) > 1L,
          2L * frequency(args$object$bts), 10L
        )
      }
      all_gts_fits <- aggts(private$.fit_object)
      all_gts_forecasts <- lapply(seq_len(ncol(all_gts_fits)), function(iter) {
        args$object <- all_gts_fits[, iter]
        call_with_args(forecast, args, other_valid = list("h"))$mean
      })
      gts_forecast_total <-
        all_gts_forecasts[[which(colnames(all_gts_fits) == "Total")]]
      predictions <- as.numeric(gts_forecast_total)
      return(predictions)
    },
    .required_packages = c("hts")
  )
)
