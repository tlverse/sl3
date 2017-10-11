

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
    .properties = c("continuous", "binomial", "categorical"),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      params <- self$params
      outcome_type <- self$get_outcome_type(task)
      
      Xmat <- as.matrix(task$X)
      
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }
      
      Y <- task$format_Y(outcome_type)
      
      if(outcome_type=="categorical"){
        num_class <- length(levels(Y))
        Y <- as.numeric(Y)-1
      }
      xgb_data <- try(xgboost::xgb.DMatrix(Xmat, label = Y))
        
      mainArgs <- list(data = xgb_data)
      mainArgs <- c(mainArgs, params)
      mainArgs[["verbose"]] <- as.integer(verbose)
      mainArgs[["print_every_n"]] <- 20
      mainArgs[["watchlist"]] <- list(train = xgb_data)
      
      if(outcome_type=="binomial"){
        mainArgs[["objective"]] <- "binary:logistic"
      } else if(outcome_type=="categorical"){
        mainArgs[["objective"]] <- "multi:softprob"
        mainArgs[["num_class"]] <- num_class
      }
      fit_object <- do.call(xgboost::xgb.train, mainArgs)
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

