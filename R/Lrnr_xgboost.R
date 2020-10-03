#' xgboost: eXtreme Gradient Boosting
#'
#' This learner provides fitting procedures for \code{xgboost} models, using the
#' \code{xgboost} package, using the \code{\link[xgboost]{xgb.train}} function.
#' Such models are classification and regression trees with extreme gradient
#' boosting. For details on the fitting procedure, consult the documentation of
#' the \code{xgboost} package.
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
#'   \item{\code{nrounds=20}}{Number of fitting iterations.}
#'   \item{\code{...}}{Other parameters passed to
#'     \code{\link[xgboost]{xgb.train}}.}
#' }
#'
#' @template common_parameters
#
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
      if (is.null(verbose)) verbose <- getOption("sl3.verbose")

      outcome_type <- self$get_outcome_type(task)

      Xmat <- as.matrix(task$X)
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }
      Y <- outcome_type$format(task$Y)
      if (outcome_type$type == "categorical") {
        Y <- as.numeric(Y) - 1
      }
      args$data <- try(xgboost::xgb.DMatrix(Xmat, label = Y))

      if (task$has_node("weights")) {
        try(xgboost::setinfo(args$data, "weight", task$weights))
      }
      if (task$has_node("offset")) {
        if (outcome_type$type == "categorical") {
          # todo: fix
          stop("offsets not yet supported for outcome_type='categorical'")
        }

        family <- outcome_type$glm_family(return_object = TRUE)
        link_fun <- args$family$linkfun
        offset <- task$offset_transformed(link_fun)
        try(xgboost::setinfo(args$data, "base_margin", offset))
      } else {
        link_fun <- NULL
      }
      args$verbose <- as.integer(verbose)
      args$watchlist <- list(train = args$data)

      if (is.null(args$objective)) {
        if (outcome_type$type == "binomial") {
          args$objective <- "binary:logistic"
        } else if (outcome_type$type == "quasibinomial") {
          args$objective <- "reg:logistic"
        } else if (outcome_type$type == "categorical") {
          args$objective <- "multi:softprob"
          args$num_class <- length(outcome_type$levels)
        }
      }
      fit_object <- call_with_args(xgboost::xgb.train, args, keep_all = TRUE)

      fit_object$training_offset <- task$has_node("offset")
      fit_object$link_fun <- link_fun

      return(fit_object)
    },

    .predict = function(task = NULL) {
      outcome_type <- private$.training_outcome_type
      verbose <- getOption("sl3.verbose")
      
      fit_object <- private$.fit_object

      Xmat <- as.matrix(task$X)
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }
      # order of columns has to be the same in xgboost training and test data
      Xmat <- Xmat[, match(fit_object$feature_names, colnames(Xmat))]

      xgb_data <- try(xgboost::xgb.DMatrix(Xmat))

      if (self$fit_object$training_offset) {
        offset <- task$offset_transformed(self$fit_object$link_fun,
          for_prediction = TRUE
        )
        xgboost::setinfo(xgb_data, "base_margin", offset)
      }

      predictions <- rep.int(list(numeric()), 1)

      if (nrow(Xmat) > 0) {
        # Use ntreelimit for prediction, if used during model training.
        # Use it only for gbtree (not gblinear, i.e., glm -- not implemented)
        ntreelimit <- 0
        if (!is.null(fit_object[["best_ntreelimit"]]) &&
          !(fit_object[["params"]][["booster"]] %in% "gblinear")) {
          ntreelimit <- fit_object[["best_ntreelimit"]]
        }
        # will generally return vector, needs to be put into data.table column
        predictions <- stats::predict(
          fit_object,
          newdata = xgb_data,
          ntreelimit = ntreelimit, reshape = TRUE
        )
      }
      if (outcome_type$type == "categorical") {
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      # names(pAoutDT) <- names(models_list)
      return(predictions)
    },
    .required_packages = c("xgboost")
  )
)
