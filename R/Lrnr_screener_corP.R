#' Correlation P-value Screener
#'
#' This learner provides covariate screening procedures using p-values returned 
#' from a test of correlation provided by the \code{stats} package, 
#' \code{\link[stats]{cor.test}} function.
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
#'   \item{\code{method = 'pearson'}}{Correlation coefficient used for test.}
#'   \item{\code{minPvalue = 0.1}}{P-value threshold for covariate selection.}
#'   \item{\code{minscreen = 2}}{Minimum number of covariates to select.}
#' }
#
Lrnr_screener_corP <- R6Class(
  classname = "Lrnr_screener_corP",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(method = 'pearson', 
                          minPvalue = 0.1, 
                          minscreen = 2) {
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),
  
  private = list(
    .properties = c("binomial", "continuous", "categorical"),
    
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      method <- args$method
      minPvalue <- args$minPvalue
      minscreen <- args$minscreen
      X <- task$X
      Y <- outcome_type$format(task$Y)
      
      listPvalue <- apply(X, 2, function(x, Y, method) { 
        ifelse(var(x) <= 0, 1, cor.test(x, y = Y, method = method)$p.value)
        }, Y = Y, method = method)
      selected <- (listPvalue <= minPvalue)
      if (sum(selected) < minscreen) {
        selected[rank(listPvalue) <= minscreen] <- TRUE
      }
      selected_names <- names(X)[selected]
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
    }
  )
)
