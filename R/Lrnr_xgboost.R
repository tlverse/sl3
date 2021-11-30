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
      args <- self$params

      verbose <- args$verbose
      if (is.null(verbose)) {
        verbose <- getOption("sl3.verbose")
      }
      args$verbose <- as.integer(verbose)

      # set up outcome
      outcome_type <- self$get_outcome_type(task)
      Y <- outcome_type$format(task$Y)
      if (outcome_type$type == "categorical") {
        Y <- as.numeric(Y) - 1
      }

      # set up predictor data
      Xmat <- as.matrix(task$X)
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }
      if (nrow(Xmat) != nrow(task$X) & ncol(Xmat) == nrow(task$X)) {
        Xmat <- t(Xmat)
      }
      args$data <- try(xgboost::xgb.DMatrix(Xmat, label = Y), silent = TRUE)

      # specify weights
      if (task$has_node("weights")) {
        try(xgboost::setinfo(args$data, "weight", task$weights), silent = TRUE)
      }

      # specify offset
      if (task$has_node("offset")) {
        if (outcome_type$type == "categorical") {
          # TODO: fix
          stop("offsets not yet supported for outcome_type='categorical'")
        }
        family <- outcome_type$glm_family(return_object = TRUE)
        link_fun <- args$family$linkfun
        offset <- task$offset_transformed(link_fun)
        try(xgboost::setinfo(args$data, "base_margin", offset), silent = TRUE)
      } else {
        link_fun <- NULL
      }

      # specify objective if it's NULL to avoid xgb warnings
      if (is.null(args$objective)) {
        if (outcome_type$type == "binomial") {
          args$objective <- "binary:logistic"
          args$eval_metric <- "logloss"
        } else if (outcome_type$type == "quasibinomial") {
          args$objective <- "reg:logistic"
        } else if (outcome_type$type == "categorical") {
          args$objective <- "multi:softprob"
          args$eval_metric <- "mlogloss"
          args$num_class <- as.integer(length(outcome_type$levels))
        }
      }

      args$watchlist <- list(train = args$data)
      fit_object <- call_with_args(xgboost::xgb.train, args, keep_all = TRUE)
      fit_object$training_offset <- task$has_node("offset")
      fit_object$link_fun <- link_fun

      return(fit_object)
    },
    .predict = function(task = NULL) {
      fit_object <- private$.fit_object

      # set up test data for prediction
      Xmat <- as.matrix(task$X)
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }
      # order of columns has to be the same in xgboost training and test data
      Xmat_ord <- as.matrix(Xmat[, match(fit_object$feature_names, colnames(Xmat))])
      if ((nrow(Xmat_ord) != nrow(Xmat)) & (ncol(Xmat_ord) == nrow(Xmat))) {
        Xmat_ord <- t(Xmat_ord)
      }
      stopifnot(nrow(Xmat_ord) == nrow(Xmat))
      # convert to xgb.DMatrix
      xgb_data <- try(xgboost::xgb.DMatrix(Xmat_ord), silent = TRUE)

      # incorporate offset, if it wasspecified in training
      if (self$fit_object$training_offset) {
        offset <- task$offset_transformed(
          self$fit_object$link_fun,
          for_prediction = TRUE
        )
        try(xgboost::setinfo(xgb_data, "base_margin", offset), silent = TRUE)
      }

      # incorporate ntreelimit, if training model was not a gblinear-based fit
      ntreelimit <- 0
      if (!is.null(fit_object[["best_ntreelimit"]]) &
        !("gblinear" %in% fit_object[["params"]][["booster"]])) {
        ntreelimit <- fit_object[["best_ntreelimit"]]
      }

      predictions <- rep.int(list(numeric()), 1)
      if (nrow(Xmat) > 0) {
        # will generally return vector, needs to be put into data.table column
        predictions <- stats::predict(
          fit_object,
          newdata = xgb_data, ntreelimit = ntreelimit, reshape = TRUE
        )

        if (private$.training_outcome_type$type == "categorical") {
          # pack predictions in a single column
          predictions <- pack_predictions(predictions)
        }
      }

      return(predictions)
    },
    .fit_can_remove = c("raw", "call"),
    .required_packages = c("xgboost")
  )
)
