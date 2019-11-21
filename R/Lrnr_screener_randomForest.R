#' Random Forest Screener
#'
#' This learner provides covariate screening procedures using variable
#' importance measures as produced by random forest models, provided by the
#' \code{randomForest} package, \code{\link[randomForest]{randomForest}}
#' function.
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
#'   \item{\code{nVar = 10}}{Number of covariates to select.}
#'   \item{\code{ntree = 1000}}{Number of trees in forest.}
#'   \item{\code{...}}{Other parameters passed to
#'     \code{\link[randomForest]{randomForest}}.}
#' }
#
Lrnr_screener_randomForest <- R6Class(
  classname = "Lrnr_screener_randomForest",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(nVar = 10, 
                          ntree = 1000, 
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  
  private = list(
    .properties = c("binomial", "continuous", "categorical"),
    
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      args$x <- task$X
      args$y <- outcome_type$format(task$Y)
      
      if (is.null(args$mtry)) {
        args$mtry <- floor(ncol(args$x))
      }
      if (outcome_type$type == "binomial") {
        args$y <- factor(args$y, levels = c(0, 1))
      }
      rf_fun <- getS3method(
        "randomForest", "default",
        envir = getNamespace("randomForest")
      )
      rf_object <- call_with_args(rf_fun, args)
      
      selected <- (rank(-rf_object$importance) <= args$nVar)
      selected_names <- names(task$X)[selected]
      covariates <- task$nodes$covariates
      covariate_selected <- sapply(covariates, function(covariate) {
        any(grep(covariate, selected_names))
      })
      fit_object <- list(selected = covariates[covariate_selected])
      return(fit_object)
    },
    
    .predict = function(task) {
      task$X[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    
    .required_packages = c("randomForest")
  )
)
