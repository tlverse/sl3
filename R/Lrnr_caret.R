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
#'  - \code{algorithm}: A string specifying which \pkg{caret} classification or
#'      regression model to use. Possible models can be found using
#'      \code{names(caret::getModelInfo())}. Information about a model,
#'      including the parameters that are tuned, can be found using
#'      \code{caret::modelLookup()}, e.g.,
#'      \code{caret::modelLookup("xgbLinear")}. Consult the caret package's
#'      documentation on \code{\link[caret]{train}} for more details.
#'  - \code{metric = NULL}: An optional string specifying the summary metric to
#'      be used to select the optimal model. If not specified, it will be set
#'      to "RMSE" for continuous outcomes and "Accuracy" for categorical and
#'      binary outcomes. Other options include "MAE", "Kappa", "Rsquared" and
#'      "logLoss". Regression models are defined when \code{metric} is set as
#'      "RMSE", "logLoss", "Rsquared", or "MAE". Classification models are
#'      defined when \code{metric} is set as "Accuracy" or "Kappa". Custom
#'      performance metrics can also be used. Consult the caret package's
#'      \code{\link[caret]{train}} documentation for more details.
#'  - \code{trControl = list(method = "cv", number = 10)}: A list for specifying
#'      the arguments for \code{\link[caret]{trainControl}} object. If not
#'      specified, it will consider "cv" with 10 folds as the resampling method,
#'      instead of caret's default resampling method, "boot". For a detailed
#'      description, consult the caret package's documentation for
#'      \code{\link[caret]{train}} and \code{\link[caret]{trainControl}}.
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
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#' autotune_bart_lrnr <- Lrnr_caret$new("bartMachine")
#' autotune_bart_fit <- autotune_bart$train(task)
#' autotune_bart_predictions <- autotune_bart_fit$predict()
Lrnr_caret <- R6Class(
  classname = "Lrnr_caret",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(algorithm,
                          metric = NULL,
                          trControl = list(method = "cv", number = 10),
                          factor_binary_outcome = TRUE,
                          ...) {
      params <- list(
        method = algorithm,
        metric = metric,
        factor_binary_outcome = factor_binary_outcome,
        ...
      )

      if (typeof(trControl) == "list") {
        params$trControl <- call_with_args(caret::trainControl, trControl)
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

      if (fit$modelType == "Classification" & outcome_type == "binomial") {
        predictions <- stats::predict(fit, newdata = task$X, type = "prob")[, 2]
      } else {
        predictions <- stats::predict(fit, newdata = task$X)
      }

      if (outcome_type != "categorical") {
        predictions <- as.numeric(predictions)
      }

      return(predictions)
    },
    .required_packages = c("caret")
  )
)
