#' Variable Importance Screener
#'
#' This learner provides covariate screening procedures using variable
#' importance measures as produced by learners that support variable importance.
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
#'   \item{\code{learner}}{An instantiated learner to use for estimating
#'     variable importance.}
#'   \item{\code{importance_fun}}{Name of the variable importance function.}
#'   \item{\code{model_arg}}{Name of the argument that defines the fitted model.}
#'   \item{\code{importance_fun_control}}{List of additional arguments to be 
#'   passed to importance_fun, in addition to the fitted model.}
#'   \item{\code{feature_var}}{Name of importance object column that corresponds 
#'   to the covariates}
#'   \item{\code{importance_var}}{Name of importance object column that corresponds 
#'   to the importance metric of interest. This variable will be used to sort 
#'   and screen variables. If NULL, importance object is assumed to be sorted,
#'   either by default or in user-supplied \code{format_importance_object_fun} 
#'   function.}
#'   \item{\code{min_retain = 2}}{Minimum number of variables to be kept.}
#'   \item{\code{max_retain = 10}}{Maximum number of variables to be kept.}
#'   \item{\code{threshold = NULL}}{Minimum size of importance_var to be kept. 
#'   If NULL, will select max_retain variables.}
#'   \item{\code{format_importance_object_fun}}{Function to customize 
#'   formatting of importance object. This function must return a 
#'   \code{data.table} or \code{data.frame}, number of rows must be greater than 
#'   max_retain, and \code{feature_var} value as a column name.}
#'   \item{\code{verbose = FALSE}}{Print selected variables.}
#'   \item{\code{...}}{Other parameters passed to \code{learner}.}
#' }
#' 
Lrnr_screener_importance <- R6Class(
  classname = "Lrnr_screener_importance",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, importance_fun, model_arg, 
                          importance_fun_control = NULL, feature_var, 
                          importance_var = NULL, min_retain = 2, max_retain = 10, 
                          threshold = NULL, format_importance_object_fun = NULL, 
                          verbose = FALSE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  
  private = list(
    .properties = c("screener"),
    
    .train = function(task) {
      # obtain model used for variable importance learner
      learner <- self$params$learner
      fit <- learner$train(task)
      mod <- fit$fit_object
      
      # check args
      covs <- task$nodes$covariates
      args <- self$params
      min_retain <- args$min_retain
      importance_fun <- args$importance_fun
      feature_var <- args$feature_var
      importance_var <- args$importance_var
      model_arg <- args$model_arg
      required_args <- list(min_retain, importance_fun, feature_var, 
                            importance_var, model_arg)
      max_retain <- args$max_retain
      threshold <- args$threshold
      if(any(is.null(required_args))){
        stop("all of the following arguments must be specified: 
             min_retain, importance_fun, feature_var, importance_var, model_arg")
      }
      if(is.null(threshold) & is.null(max_retain)){
        stop("threshold, max_retain or both must be provided")
      }
      
      if(is.null(min_retain)){
        stop("min_retain must be provided")
      }
      
      if(length(min_retain) >= length(covs)){
        stop("modify min_retain to be less than the number of covariates")
      }
      
      if(length(max_retain) > length(covs)){
        max_retain <- NULL
        warning("max_retain is greater than the number of covariates, modifying max_retain to NULL")
      }

      if(is.null(importance_var)){
        warning("importance_var not provided, assuming importance object is already sorted accordingly. 
                supply format_importance_object_fun with function to customize how object is handled.")
      }
      
      if(is.null(importance_var) & !is.null(threshold)){
        warning("cannot use threshold unless importance_var is provided")
      }
      
      # calculate variable importance
      importance_fun_control <- args$importance_fun_control
      args_importance <- c(list(mod), importance_fun_control)
      names(args_importance)[1] <- model_arg
      importance_object <- do.call(importance_fun, args_importance)
    
      # format variable importance object 
      format_fun <- args$format_importance_object_fun
      if(!is.null(format_fun)){
        importance_object <- do.call(format_fun, list(importance_object))
      }
      
      # convert variable importance object to data.table
      appropriate_classes <- c("data.table", "data.frame")
      if(any(class(importance_object) %in% appropriate_classes)){
        importance_object <- data.table(importance_object)
      } else {
        stop("create/modify format_importance_object_fun to return a data.table using take as input the object returned by importance_fun")
      }
      
      if(!is.null(importance_var)){
        importance_sorted <- importance_object[order(get(importance_var), decreasing = TRUE)]
      } else {
        importance_sorted <- importance_object
      }
      covs_sorted <- intersect(unique(importance_sorted[[feature_var]]), covs)
      
      if(!is.null(threshold)){
        important <- importance_sorted[get(importance_var) > threshold,]
        important_features <- important[[feature_var]]
        selected <- intersect(unique(important_features), covs)
        if(length(selected) < min_retain){
          selected <- covs_sorted[1:min_retain]
        }
        if( !is.null(max_retain) & (length(selected) > max_retain) ){
          selected <- selected[1:max_retain]
        }
      }
      
      if(is.null(threshold)){
        selected <- covs_sorted[1:max_retain]
      }
      
      if(length(selected) == length(covs)){
        warning("all covariates selected by Lrnr_screener_importance, consider decreasing max_retain and/or increasing threshold")
      }
      if(verbose){
        print(paste0("Lrnr_screener_importance selected the following variables: ", 
                     selected))
      }
      fit_object <- list(selected = selected)
      return(fit_object)
    },
    
    .predict = function(task) {
      task$X[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    
    .required_packages = c()
  )
)