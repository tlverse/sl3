#' Nonlinear Time Series Analysis
#'
#' This learner supports various forms of nonlinear autoregression, including
#' additive AR, neural nets, SETAR and LSTAR models, threshold VAR and VECM.
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
#'   \item{\code{learner}}{Available built-in time series models. Currently
#'     available are listed by with availableModels() function.}
#'   \item{\code{m=1}}{embedding dimension.}
#'   \item{\code{size=1}}{number of hidden units in the neural network.}
#'   \item{\code{lag=1}}{Number of lags to include in each regime.}
#'   \item{\code{d=1}}{time delay.}
#'   \item{\code{include="const"}}{Type of deterministic regressors to include.}
#'   \item{\code{type="level"}}{Whether the variable is taken is level,
#'     difference or a mix as in the ADF test.}
#'   \item{\code{n.ahead="null"}}{The forecast horizon.}
#'   \item{\code{mL=m}}{ autoregressive order for low regime.}
#'   \item{\code{mH=m}}{ autoregressive order for high regime.}
#'   \item{\code{mM=NULL}}{ autoregressive order for middle regime.}
#'   \item{\code{thDelay=0}}{Delay Time delay for the threshold variable.}
#'   \item{\code{common="none"}}{Indicates which elements are common to all
#'     regimes.}
#'   \item{\code{ML=seq_len(mL)}}{vector of lags for order for low.}
#'   \item{\code{MM=NULL}}{vector of lags for order for middle.}
#'   \item{\code{MH=seq_len(mH)}}{vector of lags for order for high.}
#'   \item{\code{nthresh=1}}{Threshold of the model.}
#'   \item{\code{trim=0.15}}{trimming parameter indicating the minimal
#'     percentage of observations in each regime.}
#'   \item{\code{sig=0.05}}{significance level for the tests to select the
#'     number of regimes.}
#'   \item{\code{control=list()}}{further arguments to be passed as control list
#'     to \code{optim}.}
#'   \item{\code{r=1}}{Number of cointegrating relationships.}
#'   \item{\code{model="VAR"}}{Model to estimate. Choices: VAR/VECM/TAR/MTAR.}
#'   \item{\code{I="level"}}{For VAR only: whether in the VAR variables are to
#'     be taken in levels or as a difference.}
#'   \item{\code{beta=NULL}}{For VECM only: imposed cointegrating value.}
#'   \item{\code{estim="2OLS"}}{Type of estimator for the VECM (two-step
#'     approach or Johansen MLE).}
#'   \item{\code{exogen=NULL}}{Inclusion of exogenous variables.}
#'   \item{\code{LRinclude="none"}}{Possibility to include in the long-run
#'     relationship and the ECT trend.}
#'   \item{\code{commonInter=FALSE}}{Whether the deterministic regressors are
#'     regime specific.}
#'   \item{\code{mTh=1}}{combination of variables with same lag order for the
#'     transition variable.}
#'   \item{\code{gamma=NULL}}{prespecified threshold values.}
#'   \item{\code{dummyToBothRegimes=TRUE}}{Whether the dummy in the one
#'     threshold model is applied to each regime. }
#'   \item{\code{max.iter=2}}{Number of iterations for the algorithm.}
#'   \item{\code{ngridBeta=50}}{Number of elements to search for the
#'     cointegrating value.}
#'   \item{\code{ngridTh=50}}{Number of elements to search for the threshold
#'     value.}
#'   \item{\code{th1}}{different possibilities to pre-specify an exact value, an
#'     interval or a central point for the search of the threshold. }
#'   \item{\code{th2}}{different possibilities to pre-specify an exact value or
#'     a central point for the search of the second threshold. }
#'   \item{\code{beta0=0}}{Additional regressors to include in the cointegrating
#'     relation.}
#'   \item{\code{...}}{Not currently used.}
#' }
#
Lrnr_tsDyn <- R6Class(
  classname = "Lrnr_tsDyn", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, m = 1, size = 1, lag = 1, d = 1,
                          include = "const", type = "level", n.ahead = NULL,
                          mL = m, mH = m, mM = NULL, thDelay = 0,
                          common = "none", ML = seq_len(mL), MM = NULL,
                          MH = seq_len(mH), nthresh = 1, trim = 0.15,
                          sig = 0.05, control = list(), r = 1, model = "VAR",
                          I = "level", beta = NULL, estim = "2OLS",
                          exogen = NULL, LRinclude = "none",
                          commonInter = FALSE, mTh = 1, gamma = NULL,
                          dummyToBothRegimes = TRUE, max.iter = 2,
                          ngridBeta = 50, ngridTh = 50,
                          th1 = list(
                            exact = NULL, int = c("from", "to"),
                            around = "val"
                          ),
                          th2 = list(
                            exact = NULL, int = c("from", "to"),
                            around = "val"
                          ),
                          beta0 = 0, ...) {
      params <- args_to_list()
      super$initialize(params = params)
      if (!is.null(n.ahead)) {
        warning("n.ahead paramater is specified- obtaining an ensemble will fail. 
                Please only use for obtaining individual learner forcasts.")
      }
    }
  ),

  private = list(
    .properties = c("timeseries", "continuous", "multivariate_outcome"),

    .train = function(task) {
      args <- self$params
      learner <- args$learner
      learner_fun <- get(
        learner,
        mode = "function",
        envir = asNamespace("tsDyn")
      )
      model <- args$model
      
      if(length(task$X)>0){
        #TO DO: add option for extrenal regressors
        args$data <- args$x <- as.matrix(task$Y)
      }else{
        args$data <- args$x <- as.matrix(task$Y)
      }
    
      if (learner == "setar") {
        if (!model %in% c("TAR", "MTAR")) {
          stop(paste(
            "When trying to fit self exciting threshold",
            "autoregressive model, must specify model to be either",
            "TAR or MTAR."
          ))
        }
      } else if (learner == "lineVar") {
        if (!model %in% c("VAR", "VECM")) {
          stop("Must specify model to be either VAR or VECM.")
        }
      } else if (learner == "TVAR") {
        if (!model %in% c("TAR", "MTAR")) {
          stop(paste(
            "When trying to fit multivariate Threshold VAR model,",
            "must specify model to be either TAR or MTAR."
          ))
        }
      }

      # kludge for tsDyn (https://groups.google.com/forum/#!topic/tsdyn/qgvR7mEqf64)
      attach(list(lag = stats::lag), name = "stats_lag_kludge", warn.conflicts = FALSE)

      fit_object <- call_with_args(learner_fun, args)

      detach("stats_lag_kludge")
      return(fit_object)
    },

    .predict = function(task = NULL) {
      params <- self$params
      n.ahead <- params[["n.ahead"]]
      learner <- params[["learner"]]

      # See if there is gap between training and validation:
      gap <- min(task$folds[[1]]$validation_set) - max(task$folds[[1]]$training_set)

      if (gap > 1) {
        if (is.null(n.ahead)) {
          n.ahead <- task$nrow + gap
        } else {
          n.ahead <- n.ahead + gap
        }

        if (learner == "TVAR") {
          stop("No forecast for multivariate Threshold VAR model implemented.")
        } else {
          predictions <- predict(private$.fit_object, n.ahead = n.ahead)
          # Create output as in glm
          predictions <- as.numeric(predictions)
          predictions <- structure(predictions, names = seq_len(length(predictions)))
          return(predictions)
        }
      } else if (gap == 1) {
        if (is.null(n.ahead)) {
          n.ahead <- task$nrow
        }
        if (learner == "TVAR") {
          stop("No forecast for multivariate Threshold VAR model implemented.")
        } else {
          predictions <- predict(private$.fit_object, n.ahead = n.ahead)
          # Create output as in glm
          predictions <- as.numeric(predictions)
          predictions <- structure(predictions, names = seq_len(n.ahead))
          return(predictions)
        }
      } else if (gap < 1) {
        warning("Validation samples come before Training samples; 
                please specify one of the time-series fold structures.")
        if (is.null(n.ahead)) {
          n.ahead <- task$nrow
        }
        if (learner == "TVAR") {
          stop("No forecast for multivariate Threshold VAR model implemented.")
        } else {
          predictions <- predict(private$.fit_object, n.ahead = n.ahead)
          # Create output as in glm
          predictions <- as.numeric(predictions)
          predictions <- structure(predictions, names = seq_len(n.ahead))
          return(predictions)
        }
      }
    },
    .required_packages = c("tsDyn", "mgcv")
  )
)
