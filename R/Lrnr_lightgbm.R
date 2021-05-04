#' LightGBM: Light Gradient Boosting Machine
#'
#' This learner provides fitting procedures for \code{lightgbm} models, using
#' the \pkg{lightgbm} package, via \code{\link[lightgbm]{lgb.train}}. These
#' gradient boosted decision tree models feature faster training speed and
#' efficiency, lower memory usage than competing frameworks (e.g., from the
#' \pkg{xgboost} package), better prediction accuracy, and improved handling of
#' large-scale data. For details on the fitting procedure and its tuning
#' parameters, consult the documentation of the \pkg{lightgbm} package. The
#' LightGBM framework was introduced in \insertCite{lightgbm;textual}{sl3}).
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
#'  package) and [Lrnr_xgboost] for the extreme gradient boosted tree models
#'  from the Xgboost framework (via the \pkg{xgboost} package).
#'
#' @section Parameters:
#'   - \code{num_threads = 1L}: Number of threads for hyperthreading.
#'   - \code{...}: Other arguments passed to \code{\link[lightgbm]{lgb.train}}.
#'       See its documentation for further details.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' # currently disabled since LightGBM crashes R on Windows
#' # more info at https://github.com/tlverse/sl3/issues/344
#' data(cpp_imputed)
#' # create task for prediction
#' cpp_task <- sl3_Task$new(
#'   data = cpp_imputed,
#'   covariates = c("bmi", "parity", "mage", "sexn"),
#'   outcome = "haz"
#' )
#'
#' # initialization, training, and prediction with the defaults
#' lgb_lrnr <- Lrnr_lightgbm$new()
#' lgb_fit <- lgb_lrnr$train(cpp_task)
#' lgb_preds <- lgb_fit$predict()
#'
#' # get feature importance from fitted model
#' lgb_varimp <- lgb_fit$importance()
#' }
Lrnr_lightgbm <- R6Class(
  classname = "Lrnr_lightgbm", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(num_threads = 1L, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    importance = function(...) {
      self$assert_trained()

      # initiate argument list for lightgbm::lgb.importance
      args <- list(...)
      args$model <- self$fit_object

      # calculate importance metrics, already sorted by decreasing importance
      importance_result <- call_with_args(lightgbm::lgb.importance, args)
      rownames(importance_result) <- importance_result[["Feature"]]
      return(importance_result)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights", "offset",
      "importance"
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
        # multiclass labels must start from zero
        Y <- as.numeric(Y) - 1L
      }

      # set up training data in custom format
      args$data <- try(lightgbm::lgb.Dataset(
        data = as.matrix(task$X),
        label = as.numeric(Y)
      ), silent = TRUE)

      # add observation-level weights if detected
      if (task$has_node("weights")) {
        try(lightgbm::setinfo(args$data, "weight", as.numeric(task$weights)),
          silent = TRUE
        )
      }

      # specify offset
      if (task$has_node("offset")) {
        # get outcome type and transform offset accordingly
        family <- outcome_type$glm_family(return_object = TRUE)
        link_fun <- args$family$linkfun
        offset <- as.numeric(task$offset_transformed(link_fun))

        # append offset to data
        try(lightgbm::setinfo(args$data, "init_score", offset), silent = TRUE)
      } else {
        link_fun <- NULL
      }

      # specify objective if it's NULL for fitting lightgbm
      if (is.null(args$objective)) {
        if (outcome_type$type == "continuous") {
          args$obj <- "regression"
        } else if (outcome_type$type == "binomial") {
          args$obj <- "binary"
          args$eval <- "binary_logloss"
        } else if (outcome_type$type == "quasibinomial") {
          args$obj <- "regression"
        } else if (outcome_type$type == "categorical") {
          args$obj <- "multiclass"
          args$eval <- "multi_error"
          args$num_class <- as.integer(length(outcome_type$levels))
        }
      }

      fit_object <- call_with_args(lightgbm::lgb.train, args, keep_all = TRUE)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      fit_object <- private$.fit_object

      # set up test data for prediction (must be matrix or sparse matrix)
      Xmat <- as.matrix(task$X)
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }

      # order of columns has to be the same in xgboost training and test data
      train_name_ord <- match(names(private$.training_task$X), colnames(Xmat))
      Xmat_ord <- as.matrix(Xmat[, train_name_ord])
      if ((nrow(Xmat_ord) != nrow(Xmat)) & (ncol(Xmat_ord) == nrow(Xmat))) {
        Xmat_ord <- t(Xmat_ord)
      }
      stopifnot(nrow(Xmat_ord) == nrow(Xmat))

      # NOTE: cannot incorporate offset in prediction, at least for now
      # see https://github.com/microsoft/LightGBM/issues/1978

      # will generally return vector, needs to be put into data.table column
      predictions <- stats::predict(
        fit_object, Xmat_ord,
        reshape = TRUE
      )

      if (private$.training_outcome_type$type == "categorical") {
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("lightgbm")
  )
)
