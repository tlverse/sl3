#' xgboost: eXtreme Gradient Boosting
#'
#' This learner provides fitting procedures for \code{xgboost} models, using
#' the \pkg{xgboost} package, via \code{\link[xgboost]{xgb.train}}. Such
#' models are classification and regression trees with extreme gradient
#' boosting. For details on the fitting procedure, consult the documentation of
#' the \pkg{xgboost} and \insertCite{xgboost;textual}{sl3}).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
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
#' @seealso [Lrnr_gbm] for standard gradient boosting models (via the \pkg{gbm}
#'  package) and [Lrnr_lightgbm] for the faster and more efficient gradient
#'  boosted trees from the LightGBM framework (via the \pkg{lightgbm} package).
#'
#' @section Parameters:
#'   - \code{nrounds=20}: Number of fitting iterations.
#'   - \code{...}: Other parameters passed to \code{\link[xgboost]{xgb.train}}.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' data(mtcars)
#' mtcars_task <- sl3_Task$new(
#'   data = mtcars,
#'   covariates = c(
#'     "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
#'     "gear", "carb"
#'   ),
#'   outcome = "mpg"
#' )
#'
#' # initialization, training, and prediction with the defaults
#' xgb_lrnr <- Lrnr_xgboost$new()
#' xgb_fit <- xgb_lrnr$train(mtcars_task)
#' xgb_preds <- xgb_fit$predict()
#'
#' # get feature importance from fitted model
#' xgb_varimp <- xgb_fit$importance()
Lrnr_xgboost <- R6Class(
  classname = "Lrnr_xgboost", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(nrounds = 20, nthread = 1, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    importance = function(...) {
      self$assert_trained()

      # initiate argument list for xgboost::xgb.importance
      args <- list(...)
      args$model <- self$fit_object

      # calculate importance metrics, already sorted by decreasing importance
      importance_result <- call_with_args(xgboost::xgb.importance, args)
      rownames(importance_result) <- importance_result[["Feature"]]
      return(importance_result)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights",
      "offset", "importance"
    ),
    .train = function(task) {
      # Safe helper for %||%
      `%||%` <- function(a, b) if (!is.null(a)) a else b
        
      args <- self$params
    
      # verbosity
      verbose <- args$verbose
      if (is.null(verbose)) verbose <- getOption("sl3.verbose")
      args$verbose <- as.integer(verbose)
    
      # outcome
      outcome_type <- self$get_outcome_type(task)
      Y <- outcome_type$format(task$Y)
      if (outcome_type$type == "categorical") Y <- as.numeric(Y) - 1L
    
      # raw covariates, keep factors intact
      Xdf <- task$get_data(columns = task$nodes$covariates, expand_factors = FALSE)
    
      # (optional but recommended) explicit feature types
      feat_types <- vapply(Xdf, function(z) {
        if (is.factor(z)) "c" else if (is.integer(z)) "int"
        else if (is.logical(z)) "i" else "float"
      }, character(1))
    
      # DMatrix
      dtrain <- try(xgboost::xgb.DMatrix(
        data = Xdf, label = Y,
        feature_names = colnames(Xdf),
        feature_types = feat_types
      ), silent = TRUE)
    
      if (!inherits(dtrain, "xgb.DMatrix")) {
        cls <- vapply(Xdf, function(z) paste(class(z), collapse=","), character(1))
        stop("xgb.DMatrix construction failed. Column classes: ",
             paste(sprintf("%s:[%s]", names(cls), cls), collapse="; "))
      }
    
      # weights
      if (task$has_node("weights")) {
        xgboost::setinfo(dtrain, "weight", task$weights)
      }
    
      # offset (base_margin)
      link_fun <- NULL
      if (task$has_node("offset")) {
        if (outcome_type$type == "categorical") {
          stop("offsets not yet supported for outcome_type='categorical'")
        }
        family <- outcome_type$glm_family(return_object = TRUE)
        link_fun <- family$linkfun
        offset <- task$offset_transformed(link_fun)
        xgboost::setinfo(dtrain, "base_margin", offset)
      }
    
      # ----- xgboost arguments: use params + evals -----
      nrounds <- if (!is.null(args$nrounds)) args$nrounds else 20L
      params  <- if (!is.null(args$params)) args$params else list()
    
      # set objective/metric if not provided
      if (is.null(params$objective)) {
        if (outcome_type$type %in% c("binomial")) {
          params$objective  <- "binary:logistic"
          params$eval_metric <- params$eval_metric %||% "logloss"
        } else if (outcome_type$type == "quasibinomial") {
          params$objective <- "reg:logistic"
        } else if (outcome_type$type == "categorical") {
          params$objective  <- "multi:softprob"
          params$eval_metric <- params$eval_metric %||% "mlogloss"
          params$num_class <- as.integer(length(outcome_type$levels))
        } else {
          params$objective <- params$objective %||% "reg:squarederror"
        }
      }
    
      fit_booster <- xgboost::xgb.train(
        data   = dtrain,
        nrounds = nrounds,
        params  = params,
        evals   = list(train = dtrain),
        verbose = args$verbose
      )
    
      # DO NOT mutate the booster; wrap it instead
      fit_object <- list(
        booster = fit_booster,
        meta = list(
          training_offset   = task$has_node("offset"),
          link_fun          = link_fun
        )
      )
      class(fit_object) <- c("sl3_xgb_fit", "list")
    
      return(fit_object)
    },
    .predict = function(task = NULL) {
      fit_object <- private$.fit_object
      booster    <- fit_object$booster
      meta       <- fit_object$meta
    
      # raw covariates; relevel to training levels
      Xdf <- task$get_data(columns = task$nodes$covariates, expand_factors = FALSE)
    
      xgb_data <- try(xgboost::xgb.DMatrix(Xdf), silent = TRUE)
      if (!inherits(xgb_data, "xgb.DMatrix")) stop("Failed to build DMatrix for prediction.")
    
      # base_margin if used in training
      if (isTRUE(meta$training_offset)) {
        offset <- task$offset_transformed(meta$link_fun, for_prediction = TRUE)
        xgboost::setinfo(xgb_data, "base_margin", offset)
      }
    
      predictions <- stats::predict(booster, newdata = xgb_data, strict_shape=TRUE)
    
      if (private$.training_outcome_type$type == "categorical") {
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("xgboost")
  )
)
