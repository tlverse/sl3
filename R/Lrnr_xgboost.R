#' xgboost: eXtreme Gradient Boosting Definition
#'
#' Definition of \code{xgboost} type models. This function is for internal use
#' only.
#'
#' @param task An object of type \code{Lrnr_base} as defined in this package.
#' @param add_outcome A \code{logical} indicating whether the trained model
#'  object should be fit in a supervised fashion (with the outcome vector) or in
#'  an unsupervised fashion.
#'
#' @rdname Lrnr_xgboost
#'
#' @name Lrnr_xgboost
#'
#' @export
#
define_xgboost_X = function(task, add_outcome = FALSE) {
  Xmat <- task$X
  Xmat <- as.matrix(Xmat)
  if (is.integer(Xmat)) {
    Xmat[, 1] <- as.numeric(Xmat[, 1])
  }
  if (add_outcome) {
    fit_dmat <- try(xgboost::xgb.DMatrix(Xmat, label = task$Y))
  } else {
    fit_dmat <- try(xgboost::xgb.DMatrix(Xmat))
  }
  return(fit_dmat)
}

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
  private = list(
    .covariates = NULL,
    .classify = FALSE,
    .return_prediction_as_vector = TRUE,

    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      private$.covariates <- task$nodes$covariates
      if ("covariates" %in% names(params) && !is.null(params[["covariates"]])) {
        private$.covariates <- intersect(private$.covariates,
                                         params$covariates)
      }
      X <- define_xgboost_X(task, add_outcome = TRUE)
      mainArgs <- list(data = X)
      mainArgs <- c(mainArgs, params)
      mainArgs[["verbose"]] <- as.integer(verbose)
      mainArgs[["print_every_n"]] <- 20
      mainArgs[["watchlist"]] <- list(train = X)
      fit_object <- do.call(xgboost::xgb.train, mainArgs)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- define_xgboost_X(task)
      fit_object <- private$.fit_object
      pAoutDT <- rep.int(list(numeric()), 1)

      if (nrow(X) > 0) {
        # Use ntreelimit for prediction, if used during model training.
        # Use it only for gbtree (not gblinear, i.e., glm -- not implemented)
        ntreelimit <- 0
        if (!is.null(fit_object[["best_ntreelimit"]]) &&
            !(fit_object[["params"]][["booster"]] %in% "gblinear")) {
          ntreelimit <- fit_object[["best_ntreelimit"]]
        }
        # will generally return vector, needs to be put into data.table column
        pAoutDT[[1]] <- stats::predict(fit_object, newdata = X,
                                       ntreelimit = ntreelimit)
      }
      predictions <- data.table::as.data.table(pAoutDT)
      # names(pAoutDT) <- names(models_list)
      return(predictions)
    },
    .required_packages = c("xgboost")
  )
)

