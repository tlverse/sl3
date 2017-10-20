

#' xgboost: eXtreme Gradient Boosting
#'
#' This learner provides fitting procedures for \code{xgboost} models, using the
#' \code{xgboost} package. Such models are classification and regression trees
#' with extreme gradient boosting. For details on the fitting procedure, consult
#' the documentation of the \code{xgboost} package.
#'
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @rdname Lrnr_xgboost
#'
#' @name Lrnr_xgboost
#'
#' @export
#
Lrnr_xgboost <- R6Class(classname = "Lrnr_xgboost", inherit = Lrnr_base,
                        portable = TRUE, class = TRUE,
  public = list(
    initialize = function(nrounds = 20, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
  }),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights", "offset"),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      Xmat <- as.matrix(task$X)

      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }

      Y <- task$format_Y(outcome_type)

      if(outcome_type=="categorical"){
        Y <- as.numeric(Y)-1
      }

      args$data <- try(xgboost::xgb.DMatrix(Xmat, label = Y))

      if(task$has_node("weights")){
        try(xgboost::setinfo(args$data, "weight", task$weights))
      }

      if(task$has_node("offset")){
        try(xgboost::setinfo(args$data, "base_margin", task$offset))
      }

      args$verbose <- as.integer(verbose)
      args$print_every_n <- 1000
      args$watchlist <- list(train = args$data)

      if(outcome_type=="binomial"){
        args$objective <- "binary:logistic"
      } else if(outcome_type=="quasibinomial"){
        args$objective <- "reg:logistic"
      } else if(outcome_type=="categorical"){
        args$objective <- "multi:softprob"
        args$num_class <- length(task$outcome_levels)
      }

      fit_object <- call_with_args(xgboost::xgb.train, args, keep_all = TRUE)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      outcome_type <- private$.training_outcome_type
      verbose <- getOption("sl3.verbose")

      Xmat <- as.matrix(task$X)

      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }

      xgb_data <- try(xgboost::xgb.DMatrix(Xmat))
      fit_object <- private$.fit_object
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
        predictions <- stats::predict(fit_object, newdata = xgb_data,
                                      ntreelimit = ntreelimit, reshape=TRUE)
      }

      if(outcome_type=="categorical"){
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      # names(pAoutDT) <- names(models_list)
      return(predictions)
    },
    .required_packages = c("xgboost")
  )
)

