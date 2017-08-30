#' @importFrom utils str
NULL

define_xgboost_X = function(task, add_outcome = FALSE) {
  Xmat <- task$X

  Xmat <- as.matrix(Xmat)
  if (is.integer(Xmat)) Xmat[,1] <- as.numeric(Xmat[,1])
  if (add_outcome) {
    fit_dmat <- try(xgboost::xgb.DMatrix(Xmat, label = task$Y))
  } else {
    fit_dmat <- try(xgboost::xgb.DMatrix(Xmat))
  }
  return(fit_dmat)
}

#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_xgboost <- R6Class(classname = "Lrnr_xgboost", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  private = list(
    .covariates = NULL,
    .classify = FALSE,
    .return_prediction_as_vector = TRUE,

    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      private$.covariates <- task$nodes$covariates
      if ("covariates" %in% names(params) && !is.null(params[["covariates"]])) {
        private$.covariates <- intersect(private$.covariates, params$covariates)
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

    ##todo: write S3 predict method for object "H2OGrid" or just write a new custom (not S3 method)
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- define_xgboost_X(task)

      fit_object <- private$.fit_object
      pAoutDT <- rep.int(list(numeric()), 1)

      if (nrow(X) > 0) {
        ## Use ntreelimit for prediction, if it was actually used during model training.
        ## Use it only for gbtree (not for gblinear, i.e., glm, as it is not implemented)
        ntreelimit <- 0
        if (!is.null(fit_object[["best_ntreelimit"]]) && !(fit_object[["params"]][["booster"]] %in% "gblinear")) {
          ntreelimit <- fit_object[["best_ntreelimit"]]
        }
        ## will generally return a vector, needs to be put into a corresponding column of a data.table
        pAoutDT[[1]] <- predict(fit_object, newdata = X, ntreelimit = ntreelimit)
      }
      predictions <- as.data.table(pAoutDT)
      # names(pAoutDT) <- names(models_list)
      return(predictions)
    }
  ), )
