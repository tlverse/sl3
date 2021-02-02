#' Hierarchical Time-Series Forecasting
#'
#' This learner supports prediction using hierarchical time-series modeling,
#' using \pkg{hts}. Fitting is done with \code{\link[hts]{hts}} and prediction
#' is performed via \code{\link[hts]{forecast.gts}}.
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
Lrnr_hts <- R6Class(
  classname = "Lrnr_hts",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(method = "comb",
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
      wide_formula <- sprintf("%s ~ %s", task$nodes$time, task$nodes$id)
      args$y <- ts(as.matrix(dcast(task$data, as.formula(wide_formula),
        value.var = task$nodes$outcome
      ))[, -1])
      fit_object <- call_with_args(hts, args, silent = TRUE)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      args <- self$params
      # get horizon based on training and testing tasks
      test_hmax <- max(unique(task$get_node("time")))
      train_hmax <- max(unique(self$training_task$get_node("time")))
      args$h <- test_hmax - train_hmax
      # get predictions for each time series
      args$object <- private$.fit_object
      hts_forecasts <- call_with_args(forecast.gts, args, silent = TRUE)$bts
      # reformat predictions to match input task
      hts_dt <-
        as.data.table(hts_forecasts)[, time := (train_hmax + 1):test_hmax]
      predictions <- melt(hts_dt, id.vars = "time", variable.name = "series")
      test_data_formerge <- as.data.table(list(
        time = task$get_node("time"),
        series = task$get_node("id")
      ))
      predictions <- merge(predictions, test_data_formerge, sort = FALSE)$value
      return(predictions)
    },
    .required_packages = c("hts")
  )
)
