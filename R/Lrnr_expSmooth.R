#' Exponential Smoothing
#'
#' This learner supports exponential smoothing models using the \code{forecast}
#' package. Fitting is done with the \code{\link[forecast]{ets}} function.
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
#'   \item{\code{model="ZZZ"}}{Three-character string identifying method. In all
#'     cases, "N"=none, "A"=additive, "M"=multiplicative, and "Z"=automatically
#'     selected. The first letter denotes the error type, second letter denotes
#'     the trend type, third letter denotes the season type. For example, "ANN"
#'     is simple exponential smoothing with additive errors, "MAM" is
#'     multiplicative Holt-Winters' methods with multiplicative errors, etc.}
#'   \item{\code{damped=NULL}}{If TRUE, use a damped trend (either additive or
#'     multiplicative). If NULL, both damped and non-damped trends will be tried
#'     and the best model (according to the information criterion ic) returned.}
#'   \item{\code{alpha=NULL}}{Value of alpha. If NULL, it is estimated.}
#'   \item{\code{beta=NULL}}{Value of beta. If NULL, it is estimated.}
#'   \item{\code{gamma=NULL}}{Value of gamma. If NULL, it is estimated.}
#'   \item{\code{phi=NULL}}{Value of phi. If NULL, it is estimated.}
#'   \item{\code{lambda=NULL}}{Box-Cox transformation parameter. Ignored if
#'     \code{NULL}. When lambda is specified, \code{additive.only} is set to
#'     \code{TRUE}.}
#'   \item{\code{additive.only=FALSE}}{If \code{TRUE}, will only consider
#'     additive models.}
#'   \item{\code{biasadj=FALSE}}{Use adjusted back-transformed mean for Box-Cox
#'     transformations.}
#'   \item{\code{lower=c(rep(1e-04, 3), 0.8)}}{Lower bounds for the parameters
#'     (alpha, beta, gamma, phi).}
#'   \item{\code{upper=c(rep(0.9999,3), 0.98)}}{Upper bounds for the parameters
#'     (alpha, beta, gamma, phi)}
#'   \item{\code{opt.crit="lik"}}{Optimization criterion.}
#'   \item{\code{nmse=3}}{Number of steps for average multistep MSE (1 <= nmse
#'     <= 30).}
#'   \item{\code{bounds="both"}}{Type of parameter space to impose: "usual"
#'     indicates all parameters must lie between specified lower and upper
#'     bounds; "admissible" indicates parameters must lie in the admissible
#'     space; "both" (default) takes the intersection of these regions.}
#'   \item{\code{ic="aic"}}{Information criterion to be used in model
#'     selection.}
#'   \item{\code{restrict=TRUE}}{If TRUE, models with infinite variance will not
#'     be allowed.}
#'   \item{\code{allow.multiplicative.trend=FALSE}}{If TRUE, models with
#'     multiplicative trend are allowed when searching for a model.}
#'   \item{\code{use.initial.values=FALSE}}{If \code{TRUE} and model is of class
#'     "ets", then the initial values in the model are also not re-estimated.}
#'   \item{\code{n.ahead}}{The forecast horizon. If not specified, returns
#'     forecast of size \code{task$X}.}
#'   \item{\code{freq=1}}{the number of observations per unit of time.}
#' }
#
Lrnr_expSmooth <- R6Class(classname = "Lrnr_expSmooth",
                          inherit = Lrnr_base,
                          portable = TRUE,
                          class = TRUE,
  public = list(
    initialize = function(model = "ZZZ", damped = NULL, alpha = NULL,
                          beta = NULL, gamma = NULL, phi = NULL, lambda = NULL,
                          additive.only = FALSE, biasadj = FALSE,
                          lower = c(rep(1e-04, 3), 0.8),
                          upper = c(rep(0.9999, 3), 0.98),
                          opt.crit = "lik", nmse = 3, bounds = "both",
                          ic = "aic", restrict = TRUE,
                          allow.multiplicative.trend = FALSE,
                          use.initial.values = FALSE, freq = 1, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("timeseries", "continuous"),

    .train = function(task) {
      args <- self$params
      args$y = ts(task$X, frequency = args$freq)
      if (args$model == "ZZZ") {
        fit_object <- forecast::ets(args$y)
      } else {
        fit_object <- call_with_args(forecast::ets, args)
      }
      return(fit_object)
    },

    .predict = function(task = NULL) {
      params <- self$params
      n.ahead <- params[["n.ahead"]]

      if (is.null(n.ahead)) {
        n.ahead = nrow(task$X)
      }
      predictions <- forecast::forecast(private$.fit_object, h = n.ahead)
      #Create output as in glm
      predictions <- as.numeric(predictions$mean)
      predictions <- structure(predictions, names = seq_len(n.ahead))
      return(predictions)
    },
    .required_packages = c("forecast")
  )
)

