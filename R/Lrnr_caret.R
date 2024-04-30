#' Caret (Classification and Regression) Training
#'
#' This learner uses the \pkg{caret} package's \code{\link[caret]{train}}
#' function to automatically tune a predictive model. It does this by defining
#' a grid of model-specific tuning parameters; fitting the model according to
#' each tuning parameter specification, to establish a set of models fits;
#' calculating a resampling-based performance measure each variation; and
#' then selecting the model with the best performance.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict family
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'  - \code{method}: A string specifying which \pkg{caret} classification or
#'      regression model to use. Possible models can be found using
#'      \code{names(caret::getModelInfo())}. Information about a model,
#'      including the parameters that are tuned, can be found using
#'      \code{caret::modelLookup()}, e.g.,
#'      \code{caret::modelLookup("xgbLinear")}. Consult the \code{caret}
#'      package's documentation on \code{\link[caret]{train}} for more details.
#'  - \code{metric = NULL}: An optional string specifying the summary metric to
#'      be used to select the optimal model. If not specified, it will be set
#'      to "RMSE" for continuous outcomes and "Accuracy" for categorical and
#'      binary outcomes. Other options include "MAE", "Kappa", "Rsquared" and
#'      "logLoss". Regression models are defined when \code{metric} is set as
#'      "RMSE", "logLoss", "Rsquared", or "MAE". Classification models are
#'      defined when \code{metric} is set as "Accuracy" or "Kappa". Custom
#'      performance metrics can also be used. Consult the \code{caret} package's
#'      \code{\link[caret]{train}} documentation for more details.
#'  - \code{trControl = list(method = "cv", number = 10)}: A list for specifying
#'      the arguments for \code{\link[caret]{trainControl}} object. If not
#'      specified, it will consider "cv" with 10 folds as the resampling method,
#'      instead of \code{caret}'s default resampling method, "boot". For a
#'      detailed description, consult the \code{caret} package's documentation
#'      for \code{\link[caret]{train}} and \code{\link[caret]{trainControl}}.
#'  - \code{factor_binary_outcome = TRUE}: Logical indicating whether a binary
#'      outcome should be defined as a factor instead of a numeric. This
#'      only needs to be modified to \code{FALSE} in the following uncommon
#'      instance: when \code{metric} is specified by the user, \code{metric}
#'      defines a regression model, and the task's outcome is binary. Note that
#'      \code{\link[caret]{train}} could throw warnings/errors when regression
#'      models are considered for binary outcomes; this argument should only
#'      be modified by advanced users in niche settings.
#'  - \code{...}: Other parameters passed to \code{\link[caret]{train}} and
#'      additional arguments defined in \code{\link{Lrnr_base}}, such as
#'      \code{params} like \code{formula}.
#'
#' @examples
#' \dontrun{
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#' autotuned_RF_lrnr <- Lrnr_caret$new(method = "rf")
#' set.seed(693)
#' autotuned_RF_fit <- autotuned_RF_lrnr$train(task)
#' autotuned_RF_predictions <- autotuned_RF_fit$predict()
#' }
Lrnr_caret <- R6Class(
  classname = "Lrnr_caret",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(method,
                          metric = NULL,
                          trControl = list(method = "cv", number = 10),
                          factor_binary_outcome = TRUE,
                          ...) {
      params <- list(
        method = method,
        metric = metric,
        factor_binary_outcome = factor_binary_outcome,
        ...
      )

      if (typeof(trControl) == "list") {
        params$trControl <- trControl
      } else {
        stop("Specified trControl is unsupported in Lrnr_caret.")
      }

      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "wrapper"),
    .train = function(task) {

      # load args
      args <- self$params

      # verbosity
      verbose <- args$verbose
      if (is.null(verbose)) {
        verbose <- getOption("sl3.verbose")
      }

      # outcome type
      outcome_type <- self$get_outcome_type(task)

      # data
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)

      # metric
      if (is.null(args$metric)) {
        if (outcome_type$type == "continuous") {
          args$metric <- "RMSE"
        } else if (outcome_type$type %in% c("binomial", "categorical")) {
          args$metric <- "Accuracy"
        } else {
          stop("Specified outcome type is unsupported in Lrnr_caret.")
        }
      }

      # make binary outcome a factor if metric defines classification model
      if (args$factor_binary_outcome & outcome_type$type == "binomial") {
        args$y <- as.factor(args$y)
      }

      # specify internal CV via trControl's indexOut argument when
      # k-fold CV is specified and it is not a repeated k-fold CV scheme
      # and when the user is not trying to control the folds with index args
      if (!is.null(args$trControl) &
        (!is.null(args$trControl$method) && args$trControl$method == "cv") &
        (is.null(args$trControl$repeats) || args$trControl$repeats == 1) &
        (is.null(args$trControl$indexOut) & is.null(args$trControl$index))) {
        caret_nfolds <- args$trControl$number # number of k-fold CV folds
        folds <- task$folds # training task's CV folds

        # we need to create the folds when caret_nfolds is provided and it is
        # not equal to the number of folds in the task, otherwise we
        # can just use "folds" (above) as the CV folds for fitting
        if (!is.null(caret_nfolds) && length(folds) != caret_nfolds) {
          folds <- task$get_folds(V = caret_nfolds)
        }
        args$trControl$indexOut <- lapply(
          seq_along(folds), function(i) folds[[i]]$validation
        )
      }
      args$trControl <- call_with_args(caret::trainControl, args$trControl)

      # fit
      fit_object <- call_with_args(
        caret::train, args,
        keep_all = TRUE, ignore = "factor_binary_outcome"
      )
      return(fit_object)
    },
    .predict = function(task) {
      fit <- private$.fit_object
      outcome_type <- self$training_outcome_type$type

      if (!is.null(task) && task$outcome_type$type != outcome_type) {
        stop("Outcome types in tasks for training and prediction do not match")
      }

      if (fit$modelType == "Classification") {
        if (outcome_type == "binomial") {
          predictions <- as.numeric(
            stats::predict(fit, newdata = task$X, type = "prob")[, 2]
          )
        } else if (outcome_type == "categorical") {
          predictions <- pack_predictions(
            as.matrix(stats::predict(fit, newdata = task$X, type = "prob"))
          )
        }
      } else {
        predictions <- as.numeric(stats::predict(fit, newdata = task$X))
      }
      return(predictions)
    },
    .required_packages = c("caret")
  )
)
