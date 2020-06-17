#' Coefficient Magnitude Screener
#'
#' This learner provides screening of covariates based on the magnitude of
#' their estimated coefficients in a (possibly regularized) GLM.
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
#'     coefficients used in screening.}
#'   \item{\code{threshold = 1e-3}}{Minimum size of coefficients to be kept. 
#'   If NULL, will select max_retain variables.}
#'   \item{\code{min_retain = 2}}{Minimum number of variables to be kept.}
#'   \item{\code{max_retain = NULL}}{Maximum number of variables to be kept. 
#'   Default selects all with absolute coefficient above threshold.}
#'   \item{\code{verbose = FALSE}}{Print selected variables.}
#'   \item{\code{...}}{Other parameters passed to \code{learner}.}
#' }
Lrnr_screener_coefs <- R6Class(
  classname = "Lrnr_screener_coefs",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, threshold = 1e-3, min_retain = 2, 
                          max_retain = NULL, verbose = FALSE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("screener"),

    .train = function(task) {
      
      # check arguments
      threshold <- self$params$threshold
      min_retain <- self$params$min_retain
      max_retain <- self$params$max_retain
      covs <- task$nodes$covariates
      
      if(is.null(threshold) & is.null(max_retain)){
        stop("threshold, max_retain or both must be provided")
      }
      
      if(is.null(min_retain)){
        stop("min_retain must be provided. ")
      }
      
      if(length(min_retain) >= length(covs)){
        stop("modify min_retain to be less than the number of covariates")
      }
      
      if(length(max_retain) > length(covs)){
        max_retain <- NULL
        warning("max_retain is greater than the number of covariates, modifying max_retain to NULL")
      }
      
      learner <- self$params$learner
      fit <- learner$train(task)
      coefs <- as.vector(coef(fit))
      coef_names <- fit$rownames(coef(fit))
      if(is.null(coef_names)){
        coef_names <- names(coef(fit))
      }
      
      if(is.null(coef_names)){
        stop("could not extract names from fit coefficients, consider different learner or screener")
      }
      
      # remove intercept
      covs_idx <- which(gsub("\\..*","",coef_names) %in% covs)
      coef_names <- coef_names[covs_idx]
      coefs <- coefs[covs_idx]
      
      # sort by absolute value of coefficients
      sorted <- sort(abs(coefs), decreasing = TRUE, index.return = TRUE)
      coef_names_sorted <- coef_names[sorted$ix] 
      coefs_sorted <- sorted$x
      covs_sorted <- intersect(unique(coef_names_sorted), covs)
      
      if(!is.null(threshold)){
        selected_coefs <- coef_names[which(coefs > threshold)]
        selected <- intersect(unique(selected_coefs), covs)
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
        warning("all covariates selected by Lrnr_screener_coefs, consider decreasing max_retain and/or increasing threshold")
      }
      if(verbose){
        print(paste0("Lrnr_screener_coefs selected the following variables: ", 
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
